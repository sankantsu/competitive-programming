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
    std::priority_queue<int> areas;
    for (int d = 0; d < problem.d; d++) {
        for (int k = 0; k < problem.n; k++) {
            areas.push(problem.a[d][k]);
        }
    }

    std::vector<int> hs{0};
    int x = 0;
    while (!areas.empty()) {
        int s = areas.top();
        areas.pop();
        int h = (s + problem.w - 1)/problem.w;
        x += h;
        if (x >= problem.w) break;
        hs.push_back(x);
    }
    hs.push_back(problem.w);
    return hs;
}

Solution solve() {
    std::vector<int> hs = make_horz_partitions();
    using Entry = std::pair<int, Rectangle>;  // area + rectangle
    auto compare = [](const Entry& lhs, const Entry& rhs) {
        if (lhs.first != rhs.first) return lhs.first < rhs.first;
        int h1 = problem.w*lhs.second.upper_left.x + lhs.second.upper_left.y;
        int h2 = problem.w*rhs.second.upper_left.x + rhs.second.upper_left.y;
        return h1 < h2;
    };
    std::set<Entry, decltype(compare)> default_pool;
    for (int k = 0; k < hs.size()-1; k++) {
        int x1 = hs[k];
        int x2 = hs[k+1];
        int s = (x2 - x1) * problem.w;
        default_pool.emplace(s, Rectangle{{x1, 0}, {x2, problem.w}});
        /* std::cerr << "x1,x2: " << x1 << " " << x2 << std::endl; */
    }

    std::vector<Arrangement> arrs;
    for (int d = 0; d < problem.d; d++) {
        decltype(default_pool) pool = default_pool;
        std::vector<Rectangle> rectangles;
        /* std::cerr << "------------------" << std::endl; */
        /* std::cerr << "Day " << d << std::endl; */
        for (int k = problem.n - 1; k >= 0; k--) {
            int s = problem.a[d][k];
            Rectangle dummy_rect = {{0,0},{0,0}};
            auto it = pool.lower_bound(Entry{s, dummy_rect});
            if (it != pool.end()) {
                auto [ul, lr] = it->second;
                int h = lr.x - ul.x;
                int w = (s + h - 1) / h;
                rectangles.push_back(Rectangle{{ul.x, ul.y}, {lr.x, ul.y + w}});
                pool.erase(it);
                int s2 = (lr.x - ul.x) * (lr.y - ul.y - w);
                if (ul.y + w != lr.y) pool.emplace(s2, Rectangle{{ul.x, ul.y + w}, {lr.x, lr.y}});
            }
            else if (!pool.empty()){
                --it;
                rectangles.push_back(it->second);
                pool.erase(it);
            }
            else {
                break;
            }
        }
        if (rectangles.size() < problem.n) {
            // 割り当て済の長方形を削ってまかなう
            int m = problem.n - rectangles.size();
            auto rect = rectangles[rectangles.size() - 1];
            rectangles.pop_back();
            auto [ul, lr] = rect;
            int h = 1;
            for (int k = 0; k < m+1; k++) {
                rectangles.push_back(Rectangle{{ul.x + k*h, ul.y}, {ul.x + (k+1)*h, lr.y}});
            }
        }
        std::reverse(rectangles.begin(), rectangles.end());
        arrs.push_back(Arrangement{rectangles});
    }
    return Solution{arrs};
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
    /* std::cerr << "Score: " << calc_score(sol) << std::endl; */
    sol.print();
}
