#include <iostream>
#include <vector>
#include <algorithm>
#include <set>
#include <map>
#include <queue>


struct Problem {
    int w;
    int d;
    int n;
    std::vector<std::vector<int>> a;
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

auto make_horz_partitions() {
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
    return hs;
}

Solution solve() {
    std::vector<int> hs = make_horz_partitions();
    int m = hs.size() - 1;

    using Rest = std::pair<int, size_t>;  // remaining area + row id
    std::set<Rest> default_pool;
    for (int k = 0; k < m; k++) {
        int s = (hs[k+1] - hs[k]) * problem.w;
        default_pool.emplace(s, k);
    }

    std::vector<Arrangement> arr;
    for (int d = 0; d < problem.d; d++) {
        /* std::cerr << "-----------------------" << std::endl; */
        /* std::cerr << "Day " << d << std::endl; */
        decltype(default_pool) pool = default_pool;
        std::vector<std::vector<int>> assignments(m);
        for (int k = problem.n - 1; k >= 0; k--) {
            int s = problem.a[d][k];
            auto it = pool.lower_bound(Rest{s, -1});
            if (it == pool.end()) it--;
            auto [r, row] = *it;
            /* std::cerr << "k,row: " << k << " " << row << std::endl; */
            int h = hs[row + 1] - hs[row];
            int w = (s + h - 1) / h;
            int rest = r - h*w;
            assignments[row].push_back(k);
            pool.erase(it);
            pool.emplace(rest, row);
        }
        for (int row = 0; row < m; row++) {
            if (assignments[row].empty()) {
                // move one entry to empty row
                int free = (hs[row+1] - hs[row]) * problem.w;
                bool done = false;
                for (int i = 0; i < m; i++) {
                    if (done) break;
                    if (i == row) continue;
                    for (int j = assignments[i].size() - 1; j >= 1; j--) {
                        int k = assignments[i][j];
                        int s = problem.a[d][k];
                        if (s <= free) {
                            auto it = assignments[i].begin() + j;
                            assignments[i].erase(it);
                            assignments[row].push_back(k);
                            done = true;
                            break;
                        }
                    }
                }
            }
        }
        std::vector<Rectangle> arrangement(problem.n);
        for (int row = 0; row < m; row++) {
            const auto& ar = assignments[row];
            int h = hs[row+1] - hs[row];
            int y = 0;
            for (int i = 0; i < ar.size(); i++) {
                int k = ar[i];
                int s = problem.a[d][k];
                int w = (s + h - 1) / h;
                int ny = (i == ar.size() - 1) ? problem.w : y + w;
                arrangement[k] = Rectangle{{hs[row], y}, {hs[row+1], ny}};
                y = ny;
            }
        }
        arr.push_back(Arrangement{arrangement});
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
    return Problem {w, d, n, std::move(a)};
}

void init() {
    problem = read_input();
}

int main() {
    init();
    Solution sol = solve();
    /* std::cerr << "Score: " << calc_score(sol) + 1 << std::endl; */
    sol.print();
}
