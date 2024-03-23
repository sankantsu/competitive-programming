#include <iostream>
#include <vector>
#include <random>
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

int calc_score_diff(const Solution& sol, int d, int c) {
    auto calc_penalty = [](int contest, const vector<int>& days) {
        int penalty = 0;
        rep(i, days.size()) {
            int next = (i == days.size() - 1) ? D : days[i+1];
            int n = next - days[i];
            penalty += costs[contest]*n*(n-1)/2;
        }
        return penalty;
    };

    int old = sol.contests[d];
    vector<int> old_days = sol.days[old];
    vector<int> new_days = sol.days[c];
    int old_penalty = calc_penalty(old, old_days) + calc_penalty(c, new_days);

    old_days.erase(find(old_days.begin(), old_days.end(), d));
    new_days.insert(lower_bound(new_days.begin(), new_days.end(), d), d);
    int new_penalty = calc_penalty(old, old_days) + calc_penalty(c, new_days);

    int old_score = scores[d][old] - old_penalty;
    int new_score = scores[d][c] - new_penalty;
    return new_score - old_score;
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

    vector<int> t(D);
    rep(i, D) {
        cin >> t[i];
        t[i]--;
    }

    vector<vector<int>> days(26, vector<int>{-1});
    rep(d, D) {
        days[t[d]].push_back(d);
    }

    Solution sol {std::move(t), std::move(days)};
    int score = calc_score(sol);

    int m;
    cin >> m;
    rep(i,m) {
        int d, q;
        cin >> d >> q;
        d--; q--;

        int diff = calc_score_diff(sol, d, q);
        score += diff;
        cout << score << endl;
        
        // update
        int org = sol.contests[d];
        sol.contests[d] = q;
        sol.days[org].erase(find(sol.days[org].begin(), sol.days[org].end(), d));
        sol.days[q].insert(lower_bound(sol.days[q].begin(), sol.days[q].end(), d), d);
    }
}
