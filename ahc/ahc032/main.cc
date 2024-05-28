#include <iostream>
#include <vector>
#include <set>
#include <queue>
#include <random>
#include <chrono>
#include <atcoder/modint>

using mint = atcoder::modint998244353;

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

static mt19937 mt;

constexpr long mod = 998244353;

int n, m, k;
int a[9][9];
int s[20][3][3];

struct P {
    int m;
    int x;
    int y;
};

using Sol = std::vector<P>;

long calc_score(const Sol& sol) {
    mint b[9][9];
    rep(i,n) rep(j,n) b[i][j] = a[i][j];

    for (auto [r, x, y] : sol) {
        rep(i,3) rep(j,3) b[x+i][y+j] += s[r][i][j];
    }
    
    long sum = 0;
    rep(i,n) rep(j,n) sum += b[i][j].val();
    return sum;
}

void print_sol(const Sol& sol) {
    cout << sol.size() << endl;
    for (auto [r, x, y] : sol) {
        cout << r << " " << x << " " << y << endl;
    }
}

Sol random_sol() {
    vector<P> sol;
    for (int l = 0; l < 5; l++) {
        int r = mt() % m;
        int x = mt() % (n - 3 + 1);
        int y = mt() % (n - 3 + 1);
        P p;
        p.m = r;
        p.x = x;
        p.y = y;
        sol.push_back(p);
    }
    return sol;
}

struct Board {
    Board() {
        init();
    }
    void init() {
        rep(i,n) rep(j,n) _b[i][j] = a[i][j];
        _sol = Sol{};
    }
    auto make_candidate(int cnt) {
        std::vector<std::vector<int>> cs(1);
        for (int l = 0; l < cnt; l++) {
            std::vector<std::vector<int>> ncs;
            for (auto cand : cs) {
                int mn = 0;
                if (!cand.empty()) {
                    mn = cand[cand.size() - 1];
                }
                for (int r = mn; r < m; r++) {
                    std::vector<int> ms = cand;
                    ms.push_back(r);
                    ncs.push_back(std::move(ms));
                }
            }
            cs = std::move(ncs);
        }
        return cs;
    }
    auto solve_range(int x, int y, int h, int w, int cnt) {
        long mx = 0;
        std::vector<int> ans;
        auto cs = make_candidate(cnt);
        for (auto cand : cs) {
            mint acc[h][w];
            rep(i,h) rep(j,w) acc[i][j] = 0;
            for (auto r : cand) {
                rep(i,h) rep(j,w) {
                    acc[i][j] += s[r][i][j];
                }
            }
            long v = 0;
            rep(i,h) rep(j,w) v += (_b[x+i][y+j] + acc[i][j]).val();
            if (v > mx) {
                mx = v;
                ans = cand;
            }
        }
        return ans;
    }
    void greedy_step(int x, int y, int h, int w, int max_cnt) {
        auto ans = solve_range(x, y, h, w, max_cnt);
        for (auto r : ans) {
            P p;
            p.m = r; p.x = x; p.y = y;
            if (_sol.size() < k) _sol.push_back(p);
        }

        for (auto r : ans) {
            rep(i,3) rep(j,3) {
                _b[x+i][y+j] += s[r][i][j];
            }
        }
    }
    auto random_ord(int cnt) {
        std::set<int> s;
        while (s.size() < cnt) {
            int r = mt() % 36;
            s.insert(r);
        }
        return s;
    }
    long greedy_ans(const std::set<int>& ords) {
        for (int x = 0; x < 6; x++) for (int y = 0; y < 6; y++) {
            int ord = 6*x + y;
            int cnt = (ords.count(ord)) ? 2 : 1;
            greedy_step(x, y, 1, 1, cnt);
        }
        for (int x = 0; x < 6; x++) {
            int y = 6;
            greedy_step(x, y, 1, 3, 3);
        }
        for (int y = 0; y < 6; y++) {
            int x = 6;
            greedy_step(x, y, 3, 1, 3);
        }
        {
            int x = 6, y = 6;
            greedy_step(x, y, 3, 3, l_last);
        }
        return calc_score(_sol);
    }
    auto find_worst_tiles(int cnt, const std::set<int>& exclude) {
        using P = std::pair<long, int>;  // tile score, ord
        std::priority_queue<P> q;
        for (int x = 0; x < 6; x++) for (int y = 0; y < 6; y++) {
            long s = _b[x][y].val();
            int ord = 6*x + y;
            q.emplace(-s, ord);
        }

        std::vector<int> ans;
        while (ans.size() < cnt) {
            auto [_, ord] = q.top();
            q.pop();
            if (!exclude.count(ord)) ans.push_back(ord);
        }
        return ans;
    }
    auto neighbor(const std::set<int>& ords) {
        std::set<int> nords = ords;
        int r = mt() % ords.size();
        auto it = nords.begin();
        for (int i = 0; i < r; i++) it++;
        nords.erase(it);

        int n_cand = 3;
        auto cand = find_worst_tiles(n_cand, nords);
        while (nords.size() < ords.size()) {
            int r = mt() % n_cand;
            nords.insert(cand[r]);
        }
        return nords;
    }
    Sol solve() {
        using namespace std::chrono;
        auto start_time = steady_clock::now();
        auto duration = milliseconds(1950);
        long best_score = 0;
        Sol best_sol;
        auto ords = random_ord(9 - l_last);
        int iter = 0;
        std::set<std::set<int>> visited;
        while (true) {
            iter++;
            auto t = steady_clock::now();
            if (t - start_time > duration) break;

            init();
            auto org = ords;
            ords = neighbor(ords);
            if (visited.count(ords)) {
                ords = org;
                continue;
            }
            visited.insert(ords);
            long score = greedy_ans(ords);
            cerr << "------------------------" << endl;
            cerr << "ords: "; for (auto x : ords) cerr << x << " "; cerr << endl;
            cerr << "score: " << score << endl;
            if (score > best_score) {
                best_score = score;
                best_sol = _sol;
            }
            else {
                ords = org;
            }
        }
        cerr << "------------------------" << endl;
        cerr << "iter: " << iter << endl;
        return best_sol;
    }
    private:
    static constexpr int l_last = 6;
    mint _b[9][9];
    Sol _sol;
};

int main() {
    cin >> n >> m >> k;

    rep(i,n) rep(j,n) cin >> a[i][j];
    rep(l,m) rep(i,3) rep(j,3) cin >> s[l][i][j];

    Board b;
    auto sol = b.solve();
    long score = calc_score(sol);
    cerr << "L: " << sol.size() << endl;
    cerr << "Final score: " << score << endl;
    print_sol(sol);
}
