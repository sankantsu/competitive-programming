#include <iostream>
#include <vector>
#include <random>
#include <cmath>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

static mt19937 mt;
static uniform_real_distribution<double> uniform(0., 1.);

static int D;
static int costs[26];
static int scores[365][26];

struct Solution {
    vector<int> contests;
    vector<vector<int>> days;
};

int calc_score(const Solution& sol) {
    int score = 0;
    vector<int> penalties(26);

    rep(i, D) {
        int c = sol.contests[i];
        long p = 0;
        rep(j, 26) {
            if (j == c) {
                penalties[c] = 0;
            }
            else {
                penalties[j] += costs[j];
            }
            p += penalties[j];
        }
        score += scores[i][c];
        score -= p;
    }
    return score;
}

Solution gen_random_sol() {
    vector<int> ans;
    rep(i,D) {
        int c = mt() % 26;
        ans.push_back(c);
    }
    return Solution{ans};
}

Solution gen_rotation() {
    vector<int> ans;
    rep(i,D) {
        int c = i % 26;
        ans.push_back(c);
    }
    return Solution{ans};
}

Solution greedy() {
    constexpr int inf = 1<<28;
    vector<int> ans;
    vector<int> last(26, -1);
    rep(i, D) {
        int best_score = -inf;
        int best_c = -1;
        rep(c, 26) {
            int score = 0;
            score += scores[i][c];
            rep(j, 26) {
                if (j == c) continue;
                score -= (i - last[j]) * costs[j];
            }
            if (score > best_score) {
                best_score = score;
                best_c = c;
            }
        }
        ans.push_back(best_c);
        last[best_c] = i;
    }
    return Solution {ans};
}

void print_solution(const Solution& sol) {
    rep(i, D) {
        cout << sol.contests[i] + 1 << endl;
    }
}

int calc_penalty(int contest, const vector<int>& days) {
    int penalty = 0;
    rep(i, days.size()) {
        int next = (i == days.size() - 1) ? D : days[i+1];
        int n = next - days[i];
        penalty += costs[contest]*n*(n-1)/2;
    }
    return penalty;
};

void climb(Solution& sol) {
    int score = calc_score(sol);

    double start_temp = 2000;
    double end_temp = 200;
    int iter = 0;
    const int max_iter = 5000000;
    while (iter++ < max_iter) {
        double temp = start_temp + (end_temp - start_temp) * iter / max_iter;
        int type = mt() % 3;
        if (type == 0) {
            int ri = mt() % D;
            int rc = mt() % 26;

            int org = sol.contests[ri];
            if (org == rc) continue;

            auto& org_days = sol.days[org];
            auto& new_days = sol.days[rc];
            int old_score_local = scores[ri][org] - calc_penalty(org, org_days) - calc_penalty(rc, new_days);

            // update
            sol.contests[ri] = rc;
            org_days.erase(find(org_days.begin(), org_days.end(), ri));
            new_days.insert(lower_bound(new_days.begin(), new_days.end(), ri), ri);
            int new_score_local = scores[ri][rc] - calc_penalty(org, org_days) - calc_penalty(rc, new_days);

            int new_score = score + (new_score_local - old_score_local);
            double accept_prob = exp((new_score - score)/temp);
            double p = uniform(mt);
            if (p < accept_prob) {
                score = new_score;
            }
            else {
                // restore
                sol.contests[ri] = org;
                new_days.erase(find(new_days.begin(), new_days.end(), ri));
                org_days.insert(lower_bound(org_days.begin(), org_days.end(), ri), ri);
            }
        }
        else {
            const int max_distance = 8;
            int sign = (mt() % 2 == 0) ? -1 : +1;
            int ri = mt() % D;
            int rj = ri + sign*((mt() % max_distance) + 1);
            if (rj < 0 || D <= rj) continue;

            int org1 = sol.contests[ri];
            int org2 = sol.contests[rj];
            auto& days_1 = sol.days[org1];
            auto& days_2 = sol.days[org2];
            int old_score_local = scores[ri][org1] + scores[rj][org2] - calc_penalty(org1, days_1) - calc_penalty(org2, days_2);

            // update
            swap(sol.contests[ri], sol.contests[rj]);
            *find(days_1.begin(), days_1.end(), ri) = rj;
            *find(days_2.begin(), days_2.end(), rj) = ri;
            sort(days_1.begin(), days_1.end());
            sort(days_2.begin(), days_2.end());
            int new_score_local = scores[ri][org2] + scores[rj][org1] - calc_penalty(org1, days_1) - calc_penalty(org2, days_2);

            int new_score = score + new_score_local - old_score_local;
            double accept_prob = exp((new_score - score)/temp);
            double p = uniform(mt);
            if (p < accept_prob) {
                score = new_score;
            }
            else {
                // restore
                swap(sol.contests[ri], sol.contests[rj]);
                *find(days_1.begin(), days_1.end(), rj) = ri;
                *find(days_2.begin(), days_2.end(), ri) = rj;
                sort(days_1.begin(), days_1.end());
                sort(days_2.begin(), days_2.end());
            }
        }
    }
}

int main() {
    cin >> D;

    rep(i, 26) {
        cin >> costs[i];
    }

    rep(i, D) {
        rep(j, 26) {
            cin >> scores[i][j];
        }
    }

    Solution sol = greedy();
    vector<vector<int>> days(26, vector<int>{-1});
    rep(d, D) {
        days[sol.contests[d]].push_back(d);
    }
    sol.days = std::move(days);

    climb(sol);

    int score = calc_score(sol);
    cerr << "Score: " << score << endl;
    print_solution(sol);
}
