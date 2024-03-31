#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <set>
#include <random>
#include <cassert>

static std::mt19937 mt;
static std::uniform_real_distribution<double> uniform(0., 1.);

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

auto adjust_vert_partition(const std::vector<int>& a, const std::vector<int>& b) {
    if (b.size() == 0) {
        return a;
    }

    const long m = a.size();
    std::vector<int> p1(m + 1), p2(b.size());
    for (int i = 0; i < m; i++) {
        p1[i+1] = p1[i] + a[i];
    }
    for (int i = 0; i < b.size() - 1; i++) {
        p2[i+1] = p2[i] + b[i];
    }

    int free = problem.w - p1[m];
    int pad_sum = 0;
    std::vector<int> paddings(m);
    int i = 1;
    // 左から順に partition 揃えられる限り padding を消費
    while (i < m) {
        auto it = std::lower_bound(p2.begin(), p2.end(), p1[i] + pad_sum);
        if (it == p2.end()) break;
        int pos = *it;
        while (i < m && p1[i+1] + pad_sum < pos) i++;
        int pad = pos - p1[i] - pad_sum;
        assert(pad >= 0);
        if (pad <= free) {
            paddings[i-1] = pad;
            free -= pad;
            pad_sum += pad;
        }
        i++;
    }

    std::vector<int> res = a;
    for (int i = 0; i < m-1; i++) {
        res[i] += paddings[i];
    }
    return res;
}

struct RowAssignment {
    using Rest = std::pair<int, size_t>;  // remaining area + row id
    static inline int nrow;
    static inline std::vector<int> horz_partitions;
    static inline std::vector<int> capacities;

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
        for (int i = 0; i < nrow; i++) {
            int s = (horz_partitions[i+1] - horz_partitions[i]) * problem.w;
            capacities.push_back(s);
        }
    }
    static int get_height(int row) {
        return horz_partitions[row + 1] - horz_partitions[row];
    }
    int calc_section_width(int k, int row) {
        int s = problem.a[_day][k];
        int h = get_height(row);
        int w = (s + h - 1) / h;
        return w;
    }
    int calc_section_size(int k, int row) {
        return get_height(row) * calc_section_width(k, row);
    }

    RowAssignment(int day) :
        _day(day), _assignments(problem.n),
        _assigned_sizes(nrow), _n_col(nrow)
    {
        init_greedy();
    }
    int row_penalty(int row) {
        int penalty = 0;
        int cap = capacities[row];
        int s = _assigned_sizes[row];
        if (cap < s) {
            penalty += 100*(s - cap);
        }
        else {
            penalty += (cap - s)/10;
        }
        if (_n_col[row] == 0) {
            penalty += problem.w;
        }
        int n_partition = std::max(0, _n_col[row] - 1);
        int l = get_height(row);
        penalty += 2*l*n_partition;
        return penalty;
    }
    int calc_penalty() {
        long penalty = 0;
        for (int i = 0; i < nrow; i++) {
            penalty += row_penalty(i);
        }
        return penalty;
    }
    void debug_print() {
        std::cerr << "_assignments:" << std::endl;
        for (int k = 0; k < problem.n; k++) {
            auto [s, row] = _assignments[k];
            std::cerr << k << " -> (" << s << ", " << row << ")" << std::endl;
        }
        std::cerr << "_assigned sizes:" << std::endl;
        for (int i = 0; i < nrow; i++) {
            std::cerr << _assigned_sizes[i] << std::endl;
        }
    }
    void init_random() {
        for (int k = 0; k < problem.n; k++) {
            int r = mt() % nrow;
            int s0 = problem.a[_day][k];
            int s = calc_section_size(k, r);
            _assignments[k] = std::make_pair(s, r);
            _assigned_sizes[r] += s;
            _n_col[r]++;
        }
    }
    void init_greedy() {
        using Row = std::pair<int, int>;  // remaining size, row id
        std::set<Row> pool;
        for (int row = 0; row < nrow; row++) {
            int h = get_height(row);
            int s = h * problem.w;
            pool.emplace(s, row);
        }
        for (int k = problem.n - 1; k >= 0; k--) {
            int s0 = problem.a[_day][k];
            auto it = pool.lower_bound(Row{s0, -1});
            if (it == pool.end()) {
                it = std::prev(it);
            }
            auto [rest, row] = *it;
            int s = calc_section_size(k, row);
            _assignments[k] = std::make_pair(s, row);
            _assigned_sizes[row] += s;
            _n_col[row]++;

            pool.erase(it);
            pool.emplace(rest - s, row);
        }
    }
    // move k th section to row
    int move(int k, int row) {
        // remove old assignment
        auto [s, i] = _assignments[k];
        int prev_penalty = row_penalty(i) + row_penalty(row);
        _assigned_sizes[i] -= s;
        _n_col[i]--;
        // new assignment
        int ns = calc_section_size(k, row);
        _assignments[k] = std::make_pair(ns, row);
        _assigned_sizes[row] += ns;
        _n_col[row]++;
        int new_penalty = row_penalty(i) + row_penalty(row);
        return -(new_penalty - prev_penalty);
    }
    // swap position of k the section and l th section
    int swap(int k, int l) {
        auto [s1, i] = _assignments[k];
        auto [s2, j] = _assignments[l];
        int prev_penalty = row_penalty(i) + row_penalty(j);

        _assigned_sizes[i] -= s1;
        _assigned_sizes[j] -= s2;

        int ns1 = calc_section_size(k, j);
        int ns2 = calc_section_size(l, i);
        _assignments[k] = std::make_pair(ns1, j);
        _assignments[l] = std::make_pair(ns2, i);
        _assigned_sizes[i] += ns2;
        _assigned_sizes[j] += ns1;

        int new_penalty = row_penalty(i) + row_penalty(j);
        return -(new_penalty - prev_penalty);
    }
    int greedy_rearrange(const std::vector<int>& rows) {
        int prev_penalty = 0;
        for (auto row : rows) {
            prev_penalty += row_penalty(row);
        }

        std::vector<int> ks;
        for (int k = problem.n - 1; k >= 0; k--) {
            auto [s, row] = _assignments[k];
            if (std::find(rows.begin(), rows.end(), row) != rows.end()) {
                ks.push_back(k);
            }
        }
        for (auto row : rows) {
            _assigned_sizes[row] = 0;
            _n_col[row] = 0;
        }
        using Row = std::pair<int, int>;  // remaining size, row id
        std::set<Row> pool;
        for (auto row : rows) {
            int h = get_height(row);
            int s = h * problem.w;
            pool.emplace(s, row);
        }
        for (auto k : ks) {
            int s0 = problem.a[_day][k];
            auto it = pool.lower_bound(Row{s0, -1});
            if (it == pool.end()) {
                it = std::prev(it);
            }
            auto [rest, row] = *it;
            int s = calc_section_size(k, row);
            _assignments[k] = std::make_pair(s, row);
            _assigned_sizes[row] += s;
            _n_col[row]++;

            pool.erase(it);
            pool.emplace(rest - s, row);
        }

        int new_penalty = 0;
        for (auto row : rows) {
            new_penalty += row_penalty(row);
        }
        return -(new_penalty - prev_penalty);
    }
    enum class Strategy {
        SWAP,
        MOVE,
        REARRANGE,
    };
    Strategy select_strategy() {
        int r = mt() % 300;
        if (r < 1) {
            return Strategy::REARRANGE;
        }
        else if (r < 240) {
            return Strategy::SWAP;
        }
        else {
            return Strategy::MOVE;
        }
    }
    void climb() {
        const int max_iter = 30000000/problem.d;
        int iter = 0;
        int current_score = -calc_penalty();
        int best_score = current_score;
        RowAssignment best_assignment = *this;
        double start_temp = 100000;
        double end_temp = 2000;
        while (iter++ < max_iter) {
            double temp = start_temp + (end_temp - start_temp)*iter / max_iter;
            if (current_score > best_score) {
                best_score = current_score;
                best_assignment = *this;
                /* std::cerr << "--------------------------" << std::endl; */
                /* std::cerr << "Day " << _day << " best_score: " << best_score << std::endl; */
                /* std::cerr << "iter, temp: " << iter << " " << temp <<std::endl; */
            }
            auto strategy = select_strategy();
            if (strategy == Strategy::MOVE) {
                int k = mt() % problem.n;
                int r = mt() % nrow;
                int org_row = _assignments[k].second;
                if (r == org_row) continue;

                int score_diff = move(k, r);

                double prob = std::exp(static_cast<double>(score_diff)/temp);
                double rd = uniform(mt);
                if (rd < prob) {
                    current_score += score_diff;
                }
                else {
                    move(k, org_row);
                }
            }
            else if (strategy == Strategy::SWAP) {
                int k = mt() % problem.n;
                int l = mt() % problem.n;
                if (k == l) continue;

                int score_diff = swap(k, l);

                int score = -calc_penalty();
                double prob = std::exp(static_cast<double>(score_diff)/temp);
                double rd = uniform(mt);
                if (rd < prob) {
                    current_score += score_diff;
                }
                else {
                    swap(k, l);
                }
            }
            else if (strategy == Strategy::REARRANGE) {
                int n_rearrange = 2;
                std::set<int> rows;
                while (rows.size() < n_rearrange) {
                    int row = mt() % nrow;
                    rows.insert(row);
                }
                auto saved_assignments = _assignments;
                auto saved_sizes = _assigned_sizes;
                auto saved_n_col = _n_col;
                int score_diff = greedy_rearrange(std::vector(rows.begin(), rows.end()));
                if (score_diff > 0) {
                    current_score += score_diff;
                }
                else {
                    _assignments = saved_assignments;
                    _assigned_sizes = saved_sizes;
                    _n_col = saved_n_col;
                }
            }
        }
        std::cerr << "--------------------------" << std::endl;
        std::cerr << "Day " << _day << " best_score: " << best_score << std::endl;
        *this = best_assignment;
    }
    auto get_row_assignments() const {
        std::vector<std::vector<int>> id_list(nrow);
        std::vector<std::vector<int>> ws(nrow);
        for (int k = 0; k < problem.n; k++) {
            auto [s, row] = _assignments[k];
            int h = get_height(row);
            int w = (s + h - 1) / h;
            id_list[row].push_back(k);
            ws[row].push_back(w);
        }
        return std::make_pair(id_list, ws);
    }
    void adjust_vert(const RowAssignment& to) {
        auto [id, a] = get_row_assignments();
        auto [_, b] = to.get_row_assignments();
        for (int i = 0; i < nrow; i++) {
            int h = get_height(i);
            auto a2 = adjust_vert_partition(a[i], b[i]);
            int sum = 0;
            for (int j = 0; j < a2.size(); j++) {
                if (a2[j] != a[i][j]) {
                std::cerr << "(Day " << std::setw(2) << _day << ") "
                        << std::setw(2) << i << " th row: "
                        << "pad " << std::setw(2) << j << "th col "
                        << a[i][j] << " -> " << a2[j] << std::endl;
                }
                _assignments[id[i][j]] = std::make_pair(h*a2[j], i);
                sum += h*a2[j];
            }
            _assigned_sizes[i] = sum;
        }
    }
    Arrangement to_arrangement() const {
        std::vector<Rectangle> arrangement(problem.n);
        std::vector<int> last_section(nrow);
        for (int k = 0; k < problem.n; k++) {
            auto [s, row] = _assignments[k];
            last_section[row] = k;
        }
        std::vector<int> ys(nrow);
        for (int k = 0; k < problem.n; k++) {
            auto [s, row] = _assignments[k];
            int x1 = horz_partitions[row];
            int x2 = horz_partitions[row+1];
            int h = x2 - x1;
            int w = (s + h - 1) / h;
            int y1 = ys[row];
            int y2 = (last_section[row] == k) ? problem.w : y1 + w;
            Rectangle rect {{x1, y1}, {x2, y2}};
            arrangement[k] = rect;

            ys[row] = y2;
        }
        return Arrangement{arrangement};
    }

    private:
    using Entry = std::pair<int, int>;  // size, id
    int _day;
    std::vector<Entry> _assignments;  // section id -> (section size, row id)
    std::vector<int> _n_col;  // row id -> number of sections for the row
    std::vector<int> _assigned_sizes;  // row id -> sum of assigned size
};

Solution solve() {
    std::vector<RowAssignment> ras;
    for (int d = 0; d < problem.d; d++) {
        RowAssignment ra(d);
        ra.climb();
        ras.push_back(ra);
    }

    int n_adjust = 5;
    for (int i = 0; i < n_adjust; i++) {
        // forward
        for (int d = 0; d < problem.d - 1; d++) {
            ras[d].adjust_vert(ras[d+1]);
        }
        // backward
        for (int d = problem.d - 1; d > 0; d--) {
            ras[d].adjust_vert(ras[d-1]);
        }
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
