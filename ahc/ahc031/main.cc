#include <iostream>
#include <vector>
#include <algorithm>
#include <set>
#include <map>
#include <queue>


struct Problem {
    Problem() {}
    Problem(int w_, int d_, int n_, std::vector<std::vector<int>>&& a_)
        : w(w_), d(d_), n(n_), a(std::move(a_))
    {
        for (int i = 0; i < d; i++) {
            int s = 0;
            for (auto x : a[i]) s += x;
            sum.push_back(s);
        }
    }
    int w;
    int d;
    int n;
    std::vector<std::vector<int>> a;
    std::vector<int> sum;
};
static Problem problem;

struct Point {
    int x;
    int y;
};

struct Rectangle {
    Point upper_left;
    Point lower_right;
    int area() const {
        return (lower_right.x - upper_left.x) * (lower_right.y - upper_left.y);
    }
};

struct Arrangement {
    std::vector<Rectangle> rectangles;
    auto horz_partitions() const {
        std::vector<bool> vs((problem.w - 1) * problem.w);
        for (auto r : rectangles) {
            if (r.upper_left.x != 0) {
                for (int j = r.upper_left.y; j < r.lower_right.y; j++) {
                    vs[r.upper_left.x * problem.w + j] = true;
                }
            }
            if (r.lower_right.x != problem.w) {
                for (int j = r.upper_left.y; j < r.lower_right.y; j++) {
                    vs[r.lower_right.x * problem.w + j] = true;
                }
            }
        }
        return vs;
    }
    auto vert_partitions() const {
        std::vector<bool> vs(problem.w * (problem.w - 1));
        for (auto r : rectangles) {
            if (r.upper_left.y != 0) {
                for (int i = r.upper_left.x; i < r.lower_right.x; i++) {
                    vs[i * (problem.w - 1) + r.upper_left.y] = true;
                }
            }
            if (r.lower_right.y != problem.w) {
                for (int i = r.upper_left.x; i < r.lower_right.x; i++) {
                    vs[i * (problem.w - 1) + r.lower_right.y] = true;
                }
            }
        }
        return vs;
    }
};

struct Solution {
    std::vector<Arrangement> arrangements;
    void print() {
        for (int d = 0; d < problem.d; d++) {
            /* std::cerr << "----------------------" << std::endl; */
            for (int k = 0; k < problem.n; k++) {
                auto rect = arrangements[d].rectangles[k];
                auto [ul,lr] = rect;
                std::cout << ul.x << " " << ul.y << " " << lr.x << " " << lr.y << std::endl;
            }
        }
    }
};

int calc_score(const Solution& sol) {
    int score = 0;
    for (int d = 0; d < problem.d; d++) {
        for (int k = 0; k < problem.n; k++) {
            int a = problem.a[d][k];
            int b = sol.arrangements[d].rectangles[k].area();
            if (b <= a) {
                score += 100*(a - b);
            }
        }
    }
    for (int d = 0; d < problem.d - 1; d++) {
        auto hs1 = sol.arrangements[d].horz_partitions();
        auto hs2 = sol.arrangements[d+1].horz_partitions();
        for (int i = 0; i < problem.w-1; i++) for (int j = 0; j < problem.w; j++) {
            auto h1 = hs1[i*problem.w + j];
            auto h2 = hs2[i*problem.w + j];
            if (h1 != h2) score++;
        }
        
        auto vs1 = sol.arrangements[d].vert_partitions();
        auto vs2 = sol.arrangements[d+1].vert_partitions();
        for (int i = 0; i < problem.w; i++) for (int j = 0; j < problem.w-1; j++) {
            auto v1 = vs1[i*(problem.w - 1) + j];
            auto v2 = vs2[i*(problem.w - 1) + j];
            if (v1 != v2) score++;
        }
    }
    return score;
}

struct Matching {
    int cost;
    int value;
    std::vector<int> indices;
};

int find_nearest(const std::vector<int>& v, int x) {
    auto cost = [&](int y) {
        return std::abs(y - x);
    };
    auto it = std::lower_bound(v.begin(), v.end(), x);
    auto it2 = (it != v.begin()) ? std::prev(it) : v.begin();
    if (it == v.end()) it = std::prev(it);
    it = (cost(*it) < cost(*it2)) ? it : it2;
    return std::distance(v.begin(), it);
}

auto find_matching(int d1, int d2) {
    std::vector<Matching> ms;
    for (int k = 0; k < problem.n; k++) {
        const auto& a1 = problem.a[d1];
        const auto& a2 = problem.a[d2];
        int v1 = a1[k];
        int k2 = find_nearest(a2, v1);
        int v2 = a2[k2];
        Matching m {
            .cost = std::abs(v2 - v1), .value = std::max(v1, v2),
            .indices = std::vector<int>{k, k2},
        };
        ms.push_back(std::move(m));
    }
    std::sort(ms.begin(), ms.end(), [](const Matching& lhs, const Matching& rhs) { return lhs.cost < rhs.cost; });
    return ms;
}

struct RowAssignment {
    using Rest = std::pair<int, size_t>;  // remaining area + row id
    static int nrow;
    static std::vector<int> horz_partitions;
    static std::set<Rest> default_pool;

    static void init_horz_partitions() {
        std::vector<int> largest_of_ranks;
        for (int k = problem.n - 1; k >= 0; k--) {
            int mx = 0;
            for (int d = 0; d < problem.d; d++) {
                mx = std::max(mx, problem.a[d][k]);
            }
            largest_of_ranks.push_back(mx);
        }

        std::vector<int> hs{0};
        int x = 0;
        for (auto s : largest_of_ranks) {
            int h = (s + problem.w - 1) / problem.w;
            x += h;
            if (x >= problem.w) break;
            hs.push_back(x);
        }
        hs.push_back(problem.w);
        horz_partitions = std::move(hs);

        nrow = horz_partitions.size() - 1;
        for (int k = 0; k < nrow; k++) {
            int s = (horz_partitions[k+1] - horz_partitions[k]) * problem.w;
            default_pool.emplace(s, k);
        }
    }

    RowAssignment() : _assignments(nrow) {
        init_pool();
    }
    void init_pool() {
        _pool = default_pool;
    }
    auto find_best_fit(int size) {
        auto it = _pool.lower_bound(Rest{size, -1});
        if (it == _pool.end()) it--;
        return it;
    }
    void add_assignment(int s, int k) {
        auto it = find_best_fit(s);
        auto [r, row] = *it;
        int h = horz_partitions[row + 1] - horz_partitions[row];
        int w = (s + h - 1) / h;
        int rest = r - h*w;
        _assignments[row].emplace_back(s, k);
        _pool.erase(it);
        _pool.emplace(rest, row);
    }
    void fill_empty_row(int d) {
        int m = _assignments.size();
        for (int row = 0; row < m; row++) {
            if (_assignments[row].empty()) {
                // move one entry to empty row
                int free = (horz_partitions[row+1] - horz_partitions[row]) * problem.w;
                bool done = false;
                for (int i = 0; i < m; i++) {
                    if (done) break;
                    if (i == row) continue;
                    for (int j = _assignments[i].size() - 1; j >= 1; j--) {
                        auto [_, k] = _assignments[i][j];
                        int s = problem.a[d][k];
                        if (s <= free) {
                            auto it = _assignments[i].begin() + j;
                            _assignments[i].erase(it);
                            _assignments[row].emplace_back(s, k);
                            done = true;
                            break;
                        }
                    }
                }
            }
        }
    }
    Arrangement to_arrangement() const {
        std::vector<Rectangle> arrangement(problem.n);
        int m = _assignments.size();
        for (int row = 0; row < m; row++) {
            const auto& ar = _assignments[row];
            int h = horz_partitions[row+1] - horz_partitions[row];
            int y = 0;
            for (int i = 0; i < ar.size(); i++) {
                auto [s, k] = ar[i];
                int w = (s + h - 1) / h;
                int ny = (i == ar.size() - 1) ? problem.w : y + w;
                arrangement[k] = Rectangle{{horz_partitions[row], y}, {horz_partitions[row+1], ny}};
                y = ny;
            }
        }
        return Arrangement{arrangement};
    }

    private:
    using Entry = std::pair<int, int>;  // size, id
    std::vector<std::vector<Entry>> _assignments;
    std::set<Rest> _pool;
};
int RowAssignment::nrow;
std::vector<int> RowAssignment::horz_partitions;
std::set<RowAssignment::Rest> RowAssignment::default_pool;

Solution solve() {
    std::vector<RowAssignment> ras;
    for (int d = 0; d < problem.d; d += 2) {
        std::cerr << "-----------------------" << std::endl;
        int d1 = d;
        int d2 = (d1 == problem.d - 1) ? d1 : d1 + 1;
        std::map<int, int> mp1;  // id -> extended area
        std::map<int, int> mp2;
        auto ms = find_matching(d1, d2);
        std::vector<RowAssignment> ra(2);
        int n_pad = problem.n/3;
        for (int i = 0; i < n_pad; i++) {
            auto m = ms[i];
            int s = m.value;
            mp1[m.indices[0]] = s;
            mp2[m.indices[1]] = s;
        }
        for (int k = problem.n - 1; k >= 0; k--) {
            int s1 = (mp1.find(k) != mp1.end()) ? mp1[k] : problem.a[d1][k];
            int s2 = (mp2.find(k) != mp2.end()) ? mp2[k] : problem.a[d2][k];
            ra[0].add_assignment(s1,k);
            ra[1].add_assignment(s2,k);
        }
        ra[0].fill_empty_row(d1);
        ra[1].fill_empty_row(d2);
        ras.push_back(ra[0]);
        if (d1 != d2) ras.push_back(ra[1]);
    }

    std::vector<Arrangement> arr;
    for (int d = 0; d < problem.d; d++) {
        arr.push_back(ras[d].to_arrangement());
    }
    return Solution{arr};
}

Problem read_input() {
    int w, d, n;
    std::cin >> w >> d >> n;

    std::vector<std::vector<int>> a(d, std::vector<int>(n));
    for (int i = 0; i < d; i++) {
        for (int j = 0; j < n; j++) {
            std::cin >> a[i][j];
        }
    }
    return Problem(w, d, n, std::move(a));
}

void init() {
    problem = read_input();
    RowAssignment::init_horz_partitions();
}

int main() {
    init();
    Solution sol = solve();
    /* std::cerr << "Score: " << calc_score(sol) + 1 << std::endl; */
    sol.print();
}
