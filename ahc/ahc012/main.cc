#include <iostream>
#include <vector>
#include <random>
#include <algorithm>
#include <chrono>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

static constexpr int radius = 10000;

static mt19937 mt(1);
static uniform_real_distribution<double> uniform(0.,1.);

static int n, k;
static int n_participant;
static vector<int> a(10);
static vector<int> x, y;
static vector<vector<int>> xindex(2*radius + 1);
static vector<vector<int>> yindex(2*radius + 1);

struct Solution {
    vector<int> xs;
    vector<int> ys;
    vector<vector<int>> counts;
    vector<int> pieces;
};

auto gen_random_1d(int len) {
    vector<int> xs;
    rep(i, len) {
        int r = mt() % (2*radius);
        xs.push_back(r - radius);
    }
    xs.push_back(-radius);
    xs.push_back(radius+1);
    sort(xs.begin(), xs.end());
    return xs;
}

auto gen_eqwidth_1d(int len) {
    vector<int> xs;
    rep(i, len) {
        xs.push_back(-radius + (2*radius / (len + 1)) * (i + 1));
    }
    xs.push_back(-radius);
    xs.push_back(radius+1);
    sort(xs.begin(), xs.end());
    return xs;
}

auto gen_random() {
    int n_xline = 10;
    int n_yline = 90;
    auto xs = gen_random_1d(n_xline);
    auto ys = gen_random_1d(n_yline);
    vector<vector<int>> counts(n_xline + 1, vector<int>(n_yline + 1));

    rep(k,n) {
        auto it1 = upper_bound(xs.begin(), xs.end(), x[k]);
        auto it2 = upper_bound(ys.begin(), ys.end(), y[k]);
        auto i = distance(xs.begin(), it1);
        auto j = distance(ys.begin(), it2);
        counts[i-1][j-1]++;
    }

    vector<int> pieces(11);
    rep(i, counts.size()) {
        rep(j, counts[i].size()) {
            if (counts[i][j] <= 10) {
                pieces[counts[i][j]]++;
            }
        }
    }

    return Solution {xs, ys, counts, pieces};
}

double calc_score(const Solution& sol) {
    int x = 0;
    rep(i,10) {
        x += min(a[i], sol.pieces[i+1]);
    }
    double score = (1000000. * x) / n_participant;
    return score;
}

void print_solution(const Solution& sol) {
    int n_line = sol.xs.size() + sol.ys.size() - 4;
    cout << n_line << endl;
    for (size_t i = 1; i < sol.xs.size() - 1; i++) {
        int x = sol.xs[i];
        cout << x - 1 << " " << -radius << " " << x << " " << radius << endl;
    }
    for (size_t i = 1; i < sol.ys.size() - 1; i++) {
        int y = sol.ys[i];
        cout << -radius << " " << y - 1 << " " << radius << " " << y << endl;
    }
}

double climb_score(const Solution& sol) {
    int score1 = 0;
    int sum = 0;
    for (int i = 1; i <= 10; i++) {
        sum += i*sol.pieces[i];
        int ext = max(0, sol.pieces[i] - a[i-1]);
        int need = max(0, a[i-1] - sol.pieces[i]);
        score1 -= (2/i)*ext*ext;
        score1 -= i*need*need;
    }
    score1 -= 10*(n - sum);
    return score1;

    /* int score2 = 0; */
    /* for (int i = 1; i <= 10; i++) { */
    /*     score2 += min(a[i-1], sol.pieces[i]); */
    /* } */
    /* return score1*(1 - r*r*r) + score2*r*r*r; */
}

void climb(Solution& sol) {
    long score = climb_score(sol);
    /* int iter = 0; */
    /* const int max_iter = 5000000; */
    long duration = 2950;
    auto start_time = chrono::steady_clock::now();
    auto end_time = start_time + chrono::milliseconds(duration);
    double start_temp = 300;
    double end_temp = 1;
    long print_cnt = 0;
    /* while (iter++ < max_iter) { */
    while (true) {
        auto now = chrono::steady_clock::now();
        if (now > end_time) {
            break;
        }
        /* double progress = static_cast<double>(iter) / max_iter; */
        double progress = static_cast<double>(chrono::duration_cast<chrono::milliseconds>(now - start_time).count()) / duration;
        double temp = start_temp + (end_temp - start_temp) * progress;
        int dir = mt() % 2;

        auto& xs = (dir == 0) ? sol.xs : sol.ys;
        auto& ys = (dir == 0) ? sol.ys : sol.xs;
        const auto& index = (dir == 0) ? xindex : yindex;

        int r = (mt() % (xs.size() - 2)) + 1;

        const int change_lim = 30;
        int mn = max(xs[r] - change_lim, xs[r - 1]);
        int mx = min(xs[r] + change_lim, xs[r + 1]);
        if (mn == mx) continue;

        int v = mn + mt() % (mx - mn + 1);
        if (v == xs[r]) continue;

        auto count_ref = [&](int i, int j) -> int& {
            if (dir == 0) {
                return sol.counts[i][j];
            }
            else {
                return sol.counts[j][i];
            }
        };

        auto update_count_ref = [&](int i_old, int i_new, int j) {
            int cur1 = count_ref(i_old, j);
            int cur2 = count_ref(i_new, j);
            count_ref(i_old, j)--;
            if (cur1 <= 10) {
                sol.pieces[cur1]--;
            }
            if (cur1 - 1 <= 10) {
                sol.pieces[cur1 - 1]++;
            }
            count_ref(i_new, j)++;
            if (cur2 <= 10) {
                sol.pieces[cur2]--;
            }
            if (cur2 + 1 <= 10) {
                sol.pieces[cur2 + 1]++;
            }
        };

        auto update_count = [&](int old_v, int new_v) {
            for (int w = min(old_v, new_v); w < max(old_v, new_v); w++) {
                for (auto y : index[w + radius]) {
                    auto it = upper_bound(ys.begin(), ys.end(), y);
                    auto j = distance(ys.begin(), it);
                    int old_i = (new_v < old_v) ? r - 1 : r;
                    int new_i = (new_v < old_v) ? r : r - 1;
                    update_count_ref(old_i, new_i, j - 1);
                }
            }
        };

        int org = xs[r];
        xs[r] = v;
        update_count(org, v);

        long new_score = climb_score(sol);
        double accept_prob = exp((new_score - score) / temp);
        double rnd = uniform(mt);
        if (rnd < accept_prob) {
            score = new_score;
        }
        else {
            // restore
            xs[r] = org;
            update_count(v, org);
        }
        if (print_cnt < (now - start_time) / chrono::milliseconds(500)) {
            print_cnt++;
            print_solution(sol);
            cerr << "score: " << calc_score(sol) << endl;
            cerr << "climb_score: " << score << endl;
            for (auto x : sol.pieces) {
                cerr << x << " ";
            }
            cerr << endl;
        }
    }
}

int main() {
    cin >> n >> k;
    x.resize(n);
    y.resize(n);

    rep(i,10) {
        cin >> a[i];
        n_participant += a[i];
    }

    rep(i,n) {
        cin >> x[i] >> y[i];
    }

    rep(i,n) {
        xindex[x[i] + radius].push_back(y[i]);
        yindex[y[i] + radius].push_back(x[i]);
    }

    auto sol = gen_random();
    climb(sol);
    cerr << "------------------------" << endl;
    cerr << "pieces: ";
    for (auto x : sol.pieces) {
        cerr << x << " ";
    }
    cerr << endl;
    cerr << "Final score: " << calc_score(sol) << endl;
    print_solution(sol);
}
