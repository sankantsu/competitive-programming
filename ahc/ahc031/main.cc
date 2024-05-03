#include <iostream>
#include <vector>
#include <algorithm>
#include <set>
#include <queue>
#include <random>
#include <chrono>
#include <tuple>
#include <variant>
#include <cassert>

static std::mt19937 mt;
static std::uniform_real_distribution<double> uniform(0., 1.);

struct Problem {
    static constexpr long area_penalty = 100;
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
    int get_area(int day, int k) const {
        return a[day][k];
    }
    int get_weight(int day) const {
        return (day == 0 || day == d - 1) ? 1 : 2;
    }
    int w;
    int d;
    int n;
    std::vector<std::vector<int>> a;
    std::vector<int> sum;
};
static Problem problem;

constexpr int max_d = 50;
constexpr int max_n = 50;

struct HorzPartitions {
    void init_random() {
        int nrow = problem.n/3 + mt()%(problem.n - problem.n/3);
        std::set<int> hs {0, problem.w};
        for (int i = 0; i < nrow - 1; i++) {
            int r = mt() % (problem.w - 1);
            hs.insert(r);
        }
        _partitions = std::vector<int>(hs.begin(), hs.end());
    }
    int nrow() const {
        return _partitions.size() - 1;
    }
    auto get_pos(int row) const {
        return std::make_pair(_partitions[row], _partitions[row + 1]);
    }
    int get_height(int row) const {
        return _partitions[row + 1] - _partitions[row];
    }
    int calc_section_width(int day, int k, int row) const {
        int s = problem.get_area(day, k);
        int h = get_height(row);
        int w = (s + h - 1) / h;
        return w;
    }
    int add_partition(int pos) {
        auto it = std::lower_bound(_partitions.begin(), _partitions.end(), pos);
        if (it != _partitions.end() && *it == pos) {
            return -1;
        }
        _partitions.insert(it, pos);
        // `it` might be stale now
        it = std::lower_bound(_partitions.begin(), _partitions.end(), pos);
        return std::distance(_partitions.begin(), it);
    }
    void remove_partition(int idx) {
        assert(0 < idx && idx < _partitions.size() - 1);
        auto it = std::next(_partitions.begin(), idx);
        _partitions.erase(it);
    }
    void shift_partition(int idx, int delta) {
        assert(_partitions[idx - 1] <= _partitions[idx] + delta);
        assert(_partitions[idx] + delta <= _partitions[idx+1]);
        _partitions[idx] += delta;
    }
    private:
    std::vector<int> _partitions;
};

struct OneDayArrangement {
    OneDayArrangement(int day, const HorzPartitions& horz_partitions):
        _day(day), _horz_partitions(horz_partitions),
        _assignments(horz_partitions.nrow()),
        _free_rooms(horz_partitions.nrow(), problem.w),
        _row_of_idx(problem.n, -1)
    {
    }
    void set(int k, int row) {
        assert(_row_of_idx[k] == -1);
        int w = _horz_partitions.calc_section_width(_day, k, row);
        _assignments[row].insert(k);
        _row_of_idx[k] = row;
        _free_rooms[row] -= w;
        if (_free_rooms[row] < 0) {
            _overflows.insert(row);
        }
    }
    void unset(int k) {
        assert(_row_of_idx[k] != -1);
        int row = _row_of_idx[k];
        int w = _horz_partitions.calc_section_width(_day, k, row);
        _assignments[row].erase(k);
        _row_of_idx[k] = -1;
        _free_rooms[row] += w;
        if (_free_rooms[row] >= 0) {
            _overflows.erase(row);
        }
    }
    void move(int k, int row) {
        assert(_row_of_idx[k] != -1);
        unset(k);
        set(k, row);
    }
    void swap(int k, int l) {
        int i = _row_of_idx[k];
        int j = _row_of_idx[l];
        move(k, j);
        move(l, i);
    }
    int calc_penalty() const {
        int penalty = 0;
        for (auto row : _overflows) {
            int h = _horz_partitions.get_height(row);
            penalty += h*-_free_rooms[row]*problem.area_penalty;
        }
        for (int row = 0; row < _horz_partitions.nrow(); row++) {
            if (_assignments[row].empty()) {
                penalty += 2 * problem.w;
            }
            int n_sect = _assignments[row].size();
            int n_bar = std::max(0, n_sect - 1);
            int h = _horz_partitions.get_height(row);
            int weight = problem.get_weight(_day);
            penalty += weight * n_bar * h;
            /* penalty += weight * n_bar; */
        }
        return penalty;
    }
    auto calc_best_swap() const {
        int max_score = 0;
        int mk = -1;
        int ml = -1;
        for (auto i : _overflows) {
            for (auto k : _assignments[i]) {
                for (int l = 0; l < k; l++) {  // consider only smaller sections
                    int j = _row_of_idx[l];
                    auto calc_overflow = [&](int row, int k, int l) {
                        // calc overflow of row when replace section k with section l
                        int w1 = _horz_partitions.calc_section_width(_day, k, row);
                        int w2 = _horz_partitions.calc_section_width(_day, l, row);
                        int r = _free_rooms[row] + w1 - w2;
                        return std::max(0, -r);
                    };
                    auto calc_penalty = [&](int i, int j, int k, int l) {
                        int hi = _horz_partitions.get_height(i);
                        int hj = _horz_partitions.get_height(j);
                        return hi*calc_overflow(i, k, l) + hj*calc_overflow(j, l, k);
                    };
                    int penalty = calc_penalty(i, j, k, k);
                    int new_penalty = calc_penalty(i, j, k, l);
                    int score = -(new_penalty - penalty);
                    if (score > max_score) {
                        max_score = score;
                        mk = k;
                        ml = l;
                    }
                }
            }
        }
        return std::make_tuple(max_score, mk, ml);
    }
    void init_greedy() {
        using P = std::pair<int, size_t>;  // remaining room, row idx
        std::priority_queue<P> q;
        for (int r = 0; r < _horz_partitions.nrow(); r++) {
            int area = _horz_partitions.get_height(r) * problem.w;
            q.emplace(area, r);
        }
        for (int k = problem.n - 1; k >= 0; k--) {
            auto [s, r] = q.top();
            q.pop();
            set(k, r);
            int h = _horz_partitions.get_height(r);
            int w = _horz_partitions.calc_section_width(_day, k, r);
            q.emplace(s - h*w, r);
        }
    }
    void fill_empty() {
        for (int row = 0; row < _horz_partitions.nrow(); row++) {
            if (_assignments[row].empty()) {
                for (int i = 0; i < _horz_partitions.nrow(); i++) {
                    if (i == row) continue;
                    if (_assignments[i].size() == 1) continue;
                    for (auto k : _assignments[i]) {
                        int w = _horz_partitions.calc_section_width(_day, k, row);
                        if (w < _free_rooms[row]) {
                            move(k, row);
                            break;
                        }
                    }
                    if (!_assignments[row].empty()) break;
                }
            }
        }
    }
    void climb() {
        init_greedy();
        /* fill_empty(); */
        while (true) {
            auto [score, k, l] = calc_best_swap();
            if (score > 0) {
                swap(k,l);
            }
            else {
                // cannot improve the solution
                break;
            }
        }
        /* fill_empty(); */
    }
    long solve() {
        climb();
        return -calc_penalty();
    }
    auto get_assignments() const {
        return _assignments;
    }
    private:
    int _day;
    const HorzPartitions& _horz_partitions;
    std::vector<std::set<int>> _assignments;  // _assignments[row] = set of assigned sections
    std::vector<int> _free_rooms;  // remaining rooms (width) for each row
    std::vector<int> _row_of_idx;
    std::set<int> _overflows;  // overflowed rows
};

struct HorzSolution {
    HorzSolution() = default;
    HorzSolution(auto hs_, auto as_) :
        hs(hs_), assignments(problem.d)
    {
        for (int d = 0; d < problem.d; d++) {
            for (int row = 0; row < hs.nrow(); row++) {
                std::vector<int> vs;
                for (auto x : as_[d][row]) vs.push_back(x);
                assignments[d].push_back(std::move(vs));
            }
        }
    }
    auto get_partitions(int day) {
        std::vector<int> nsections(hs.nrow());
        std::vector<int> row_of_idx(problem.n);
        for (int row = 0; row < hs.nrow(); row++) {
            for (auto x : assignments[day][row]) {
                nsections[row]++;
                row_of_idx[x] = row;
            }
        }
        std::vector<std::set<int>> ps(hs.nrow());
        std::vector<int> cur(hs.nrow());
        for (int k = 0; k < problem.n; k++) {
            int r = row_of_idx[k];
            int w = hs.calc_section_width(day, k, r);
            if (cur[r] > 0) {
                ps[r].insert(cur[r]);
            }
            cur[r] += w;
        }
        return ps;
    }
    void print() {
        for (int d = 0; d < problem.d; d++) {
            auto ps = get_partitions(d);
            using Reservation = std::tuple<int, int, int, int, int>;  // k, x1, y1, x2, y2
            std::vector<Reservation> rsrvs;
            for (int row = 0; row < hs.nrow(); row++) {
                auto [x1, x2] = hs.get_pos(row);
                ps[row].insert(0);
                ps[row].insert(problem.w);
                std::vector<int> psr(ps[row].begin(), ps[row].end());
                for (int i = 0; i < psr.size() - 1; i++) {
                    int k = assignments[d][row][i];
                    int y1 = psr[i];
                    int y2 = psr[i+1];
                    rsrvs.emplace_back(k, x1, y1, x2, y2);
                }
            }
            std::sort(rsrvs.begin(), rsrvs.end());
            for (auto [_, x1, y1, x2, y2] : rsrvs) {
                std::cout << x1 << " " << y1 << " " << x2 << " " << y2 << std::endl;
            }
        }
    }
    HorzPartitions hs;
    using day_assignment = std::vector<std::vector<int>>;  // a[i][j] = section id of (row, col) = (i, j)
    std::vector<day_assignment> assignments;
};

namespace sa {

struct Shift {
    int idx;
    int delta;
};

struct Add {
    int pos;
    int org_idx;
};

struct Del {
    int idx;
    int org_pos;
};

using T = std::variant<Shift, Add, Del>;

}

struct HorzPartitionSolver {
    HorzPartitionSolver() {
        init();
    }
    void init() {
        _hs.init_random();
    }
    sa::T select_strategy() {
        int dice = mt() % 100;
        if (dice < 50) {
            int idx = 1 + mt()%(_hs.nrow() - 1);
            int h1 = _hs.get_height(idx - 1);
            int h2 = _hs.get_height(idx);
            int delta = mt()%(h1 + h2 - 1) - h1 + 1;
            return sa::Shift{idx, delta};
        }
        else if (dice < 90) {
            int pos = mt()%problem.w;
            return sa::Add{pos};
        }
        else {
            int idx = 1 + mt()%(_hs.nrow() - 1);
            auto [_, org_pos] = _hs.get_pos(idx-1);
            return sa::Del{idx, org_pos};
        }
    }
    bool try_strategy(sa::T& strategy) {
        if (std::holds_alternative<sa::Shift>(strategy)) {
            auto [idx, delta] = std::get<sa::Shift>(strategy);
            _hs.shift_partition(idx, delta);
            return true;
        }
        else if (std::holds_alternative<sa::Add>(strategy)) {
            auto& st = std::get<sa::Add>(strategy);
            st.org_idx = _hs.add_partition(st.pos);
            if (st.org_idx == -1) {
                return false;
            }
            return true;
        }
        else if (std::holds_alternative<sa::Del>(strategy)){
            auto [idx, _] = std::get<sa::Del>(strategy);
            _hs.remove_partition(idx);
            return true;
        }
        assert(false);
    }
    void restore(sa::T strategy) {
        if (std::holds_alternative<sa::Shift>(strategy)) {
            auto [idx, delta] = std::get<sa::Shift>(strategy);
            _hs.shift_partition(idx, -delta);
        }
        else if (std::holds_alternative<sa::Add>(strategy)) {
            auto st = std::get<sa::Add>(strategy);
            _hs.remove_partition(st.org_idx);
        }
        else if (std::holds_alternative<sa::Del>(strategy)){
            auto [_, org_pos] = std::get<sa::Del>(strategy);
            _hs.add_partition(org_pos);
        }
    }
    void climb() {
        int iter = 0;
        const int max_iter = 2000;
        long best_score = -2 * problem.area_penalty * problem.w * problem.w;
        while (iter++ < max_iter) {
            auto strategy = select_strategy();
            bool check = try_strategy(strategy);
            if (!check) continue;

            std::vector<OneDayArrangement> arrs;
            for (int d = 0; d < problem.d; d++) {
                arrs.emplace_back(d, _hs);
            }

            int score = 0;
            for (int d = 0; d < problem.d; d++) {
                int day_score = arrs[d].solve();
                score += day_score;
            }

            if (score > best_score) {
                /* std::cerr << "score: " << score << std::endl; */
                /* for (int d = 0; d < problem.d; d++) { */
                /*     int day_score = arrs[d].calc_penalty(); */
                /*     std::cerr << "  day " << d << ": " << day_score << std::endl; */
                /* } */
                best_score = score;
                using day_assignment = decltype(arrs[0].get_assignments());
                std::vector<day_assignment> ds;
                for (int d = 0; d < problem.d; d++) {
                    ds.emplace_back(arrs[d].get_assignments());
                }
                _best_solution = HorzSolution(_hs, ds);
            }
            else {
                restore(strategy);
            }
        }
    }
    HorzSolution solve() {
        climb();
        return _best_solution;
    }
    private:
    HorzPartitions _hs;
    HorzSolution _best_solution;
};

struct VertSolution {
    HorzPartitions _hs;
    std::vector<std::vector<std::vector<int>>> _ps;
    int ncol(int day, int row) const {
        return _ps[day][row].size() - 1;
    }
    int npartition(int day, int row) const {
        return std::max(0, ncol(day, row) - 1);
    }
    int get_partition_pos(int day, int row, int j) const {
        return _ps[day][row][j+1];
    }
    int calc_area_of_col(int day, int row, int col) const {
        int h = _hs.get_height(row);
        int w = _ps[day][row][col + 1] - _ps[day][row][col];
        return h * w;
    }
    auto get_raw() const {
        using P = std::tuple<int, int, int>;  // area, row, col
        std::vector<std::set<P>> sections(problem.d);
        for (int day = 0; day < problem.d; day++) {
            for (int row = 0; row < _hs.nrow(); row++) {
                for (int col = 0; col < ncol(day, row); col++) {
                    int s = calc_area_of_col(day, row, col);
                    sections[day].emplace(s, row, col);
                }
            }
        }

        using Section = std::pair<int, int>;  // row, col
        std::vector<std::vector<Section>> section_of_idx(problem.d, std::vector<Section>(problem.n));
        for (int day = 0; day < problem.d; day++) {
            auto it = sections[day].rbegin();
            for (int k = problem.n - 1; k >= 0; k--) {
                auto [_, row, col] = *it;
                section_of_idx[day][k] = std::make_pair(row, col);
                it++;
            }
        }
        return section_of_idx;
    }
    auto get_section_pos(int day, int row, int col) const {
        auto [x1, x2] = _hs.get_pos(row);
        int y1 = _ps[day][row][col];
        int y2 = _ps[day][row][col+1];
        return std::make_tuple(x1, y1, x2, y2);
    }
    void print() const {
        auto raw = get_raw();
        for (int day = 0; day < problem.d; day++) {
            for (int k = 0; k < problem.n; k++) {
                auto [row, col] = raw[day][k];
                auto [x1, y1, x2, y2] = get_section_pos(day, row, col);
                std::cout << x1 << " " << y1 << " " << x2 << " " << y2 << std::endl;
            }
        }
    }
};

struct VertPartitionSolver {
    VertPartitionSolver(HorzSolution solution) :
        _hs(solution.hs), _ps(problem.d, day_partitions(problem.n)), _areas(problem.d)
    {
        for (int day = 0; day < problem.d; day++) {
            auto psd = solution.get_partitions(day);
            for (int row = 0; row < _hs.nrow(); row++) {
                psd[row].insert(0);
                psd[row].insert(problem.w);
                _ps[day][row] = std::vector<int>(psd[row].begin(), psd[row].end());
            }
        }
        for (int day = 0; day < problem.d; day++) {
            for (int row = 0; row < _hs.nrow(); row++) {
                int h = _hs.get_height(row);
                for (int col = 0; col < ncol(day, row); col++) {
                    int w = _ps[day][row][col + 1] - _ps[day][row][col];
                    _areas[day].insert(h*w);
                }
            }
        }
    }
    int ncol(int day, int row) const {
        return _ps[day][row].size() - 1;
    }
    int npartition(int day, int row) const {
        return std::max(0, ncol(day, row) - 1);
    }
    int get_partition_pos(int day, int row, int j) const {
        return _ps[day][row][j+1];
    }
    int calc_area_of_col(int day, int row, int col) const {
        int h = _hs.get_height(row);
        int w = _ps[day][row][col + 1] - _ps[day][row][col];
        return h * w;
    }
    bool partition_exists(int day, int row, int pos) {
        auto& v = _ps[day][row];
        auto it = std::lower_bound(v.begin(), v.end(), pos);
        return *it == pos;
    }
    void insert(int day, int row, int pos) {
        auto& v = _ps[day][row];
        auto it = std::lower_bound(v.begin(), v.end(), pos);
        int prev = *(it - 1);
        int next = *it;
        assert(next != pos);
        v.insert(it, pos);
        int h = _hs.get_height(row);
        int s0 = h * (next - prev);
        int s1 = h * (pos - prev);
        int s2 = h * (next - pos);
        _areas[day].erase(_areas[day].find(s0));
        _areas[day].insert(s1);
        _areas[day].insert(s2);
    }
    void remove(int day, int row, int pos) {
        auto& v = _ps[day][row];
        auto it = std::lower_bound(v.begin(), v.end(), pos);
        assert(*it == pos);
        int prev = *(it - 1);
        int next = *(it + 1);
        v.erase(it);
        int h = _hs.get_height(row);
        int s0 = h * (pos - prev);
        int s1 = h * (next - pos);
        int s2 = h * (next - prev);
        _areas[day].erase(_areas[day].find(s0));
        _areas[day].erase(_areas[day].find(s1));
        _areas[day].insert(s2);
    }
    int calc_penalty() const {
        int penalty = 0;
        for (int day = 0; day < problem.d; day++) {
            int area_penalty = 0;
            auto it = _areas[day].rbegin();
            for (int k = problem.n - 1; k >= 0; k--) {
                int s0 = problem.get_area(day, k);
                int s = *it;
                if (s < s0) {
                    area_penalty += problem.area_penalty * (s0 - s);
                }
                it++;
            }

            int partition_penalty = 0;
            for (int row = 0; row < _hs.nrow(); row++) {
                int weight = problem.get_weight(day);
                int h = _hs.get_height(row);
                partition_penalty += weight * npartition(day, row) * h;
            }
            auto check_reuse = [&](int d1, int d2) {
                int penalty = 0;
                for (int row = 0; row < _hs.nrow(); row++) {
                    int h = _hs.get_height(row);
                    const auto& v = _ps[d2][row];
                    for (auto y : _ps[d1][row]) {
                        if (y == 0 || y == problem.w) continue;
                        auto it = std::lower_bound(v.begin(), v.end(), y);
                        if (*it == y) {
                            penalty -= h;
                        }
                    }
                }
                return penalty;
            };
            int reuse_penalty = 0;
            if (day - 1 >= 0) {
                reuse_penalty += check_reuse(day, day - 1);
            }
            if (day + 1 < problem.d) {
                reuse_penalty += check_reuse(day, day + 1);
            }

            int day_penalty = area_penalty + partition_penalty + reuse_penalty;
            /* std::cerr << "area, part, reuse: " << area_penalty << " " << partition_penalty << " " << reuse_penalty << std::endl; */
            /* std::cerr << "tot: " << day_penalty << std::endl; */
            penalty += day_penalty;
        }
        return penalty;
    }
    int random_next_day(int d) {
        int d2;
        if (d == 0) {
            d2 = d + 1;
        }
        else if (d == problem.d - 1) {
            d2 = d - 1;
        }
        else {
            int lot = mt() % 2;
            d2 = (lot == 0) ? d - 1 : d + 1;
        }
        return d2;
    }
    void climb() {
        int iter = 0;
        const int max_iter = 100000;

        int best_score = -calc_penalty();
        decltype(_ps) best_ps;
        decltype(_areas) best_areas;
        while (iter++ < max_iter) {
            int d = mt() % problem.d;
            int i = mt() % _hs.nrow();
            int j = mt() % _hs.nrow();
            if (npartition(d, i) == 0) continue;
            int col = mt() % npartition(d, i);
            int org_pos = get_partition_pos(d, i, col);
            int new_pos;
            int lot = mt() % 3;
            if (lot == 0) {
                new_pos = 1 + mt() % (problem.w - 1);
            }
            else {
                int d2 = random_next_day(d);
                if (npartition(d2, j) == 0) continue;
                int c = mt() % npartition(d2, j);
                new_pos = get_partition_pos(d2, j, c);
            }
            if (partition_exists(d, j, new_pos)) continue;
            remove(d, i, org_pos);
            insert(d, j, new_pos);

            int score = -calc_penalty();
            if (score > best_score) {
                std::cerr << "score: " << score << std::endl;
                best_score = score;
                best_ps = _ps;
                best_areas = _areas;
            }
            else {
                remove(d, j, new_pos);
                insert(d, i, org_pos);
            }
        }
        _ps = best_ps;
        _areas = best_areas;
    }
    VertSolution get_solution() const {
        return VertSolution{_hs, _ps};
    }
    VertSolution solve() {
        climb();
        return get_solution();
    }
    private:
    using day_partitions = std::vector<std::vector<int>>;
    using day_areas = std::multiset<int>;
    HorzPartitions _hs;
    std::vector<day_partitions> _ps;
    std::vector<day_areas> _areas;
};

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
}

int main() {
    init();
    HorzPartitionSolver hsolver;
    auto sol1 = hsolver.solve();
    /* sol1.print(); */

    VertPartitionSolver vsolver(sol1);
    auto sol2 = vsolver.solve();
    sol2.print();
}
