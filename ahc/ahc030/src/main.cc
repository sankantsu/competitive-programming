#include <cstdio>
#include <cmath>
#include <cassert>
#include <type_traits>
#include <ranges>
#include <iostream>
#include <numeric>
#include <random>
#include <algorithm>
#include <vector>
#include <string>
#include <queue>

namespace util {

void print_hline(std::ostream& ostream=std::cerr) {
    ostream << "--------------------------------" << std::endl;
}

template <typename T>
void print_vector(const std::vector<T>& v, std::ostream& ostream=std::cerr) {
    char delim = ' ';
    for (int i = 0; i < v.size(); i++) {
        ostream << v[i];
        if (i < v.size() - 1) {
            ostream << delim;
        }
    }
}

template <typename T>
auto index_sort(const std::vector<T>& v) {
    std::vector<std::size_t> indices(v.size());
    std::iota(indices.begin(), indices.end(), 0);
    auto comp = [&v](std::size_t i, std::size_t j) {
        return v[i] < v[j];
    };
    std::sort(indices.begin(), indices.end(), comp);
    return indices;
}

}

namespace ahc {

using oil_reserve_t = int;

namespace oil_reservation {
    constexpr oil_reserve_t undef = -1;
}

struct Point {
    int i;
    int j;
};

enum class Direction {
    Horizontal,
    Vertical
};

struct Polyomino {
    using projection_type = std::vector<oil_reserve_t>;
    Polyomino(int area, std::vector<Point> relative_positions)
        : _area(area), _relative_positions(relative_positions)
    {
        _horz_size = 0;
        _vert_size = 0;
        for (auto [i,j] : _relative_positions) {
            _horz_size = std::max(_horz_size, i+1);
            _vert_size = std::max(_vert_size, j+1);
        }
    }
    auto get_area() const { return _area; }
    auto get_relative_positions() const { return _relative_positions; }
    projection_type make_projection(Direction dir) const {
        int size = (dir == Direction::Horizontal) ? _horz_size : _vert_size;
        projection_type proj(size);
        for (auto [i,j] : _relative_positions) {
            if (dir == Direction::Horizontal) {
                proj[i]++;
            }
            else {
                proj[j]++;
            }
        }
        return proj;
    }
    bool contains(Point point) const {
        for (auto p : _relative_positions) {
            if (p.i == point.i && p.j == point.j) {
                return true;
            }
        }
        return false;
    }
    private:
    int _area;
    std::vector<Point> _relative_positions;
    int _horz_size;
    int _vert_size;
};

struct Problem {
    Problem() = default;
    void init(int board_size, int num_oilfield, double error_param, const std::vector<Polyomino>& oil_fields) {
        _board_size = board_size;
        _num_oilfield = num_oilfield;
        _error_param = error_param;
        _oil_fields = oil_fields;
        // sort oil fields by are size
        std::sort(_oil_fields.begin(), _oil_fields.end(),
                  [](const Polyomino& lhs, const Polyomino& rhs) {
                        return lhs.get_area() > rhs.get_area();
                  });
    }
    int get_board_size() const { return _board_size; }
    int get_num_oilfield() const { return _num_oilfield; }
    double get_error_param() const { return _error_param; }
    std::vector<Polyomino> get_polyominos() const { return _oil_fields; }
    private:
    int _board_size;
    int _num_oilfield;
    double _error_param;
    std::vector<Polyomino> _oil_fields;
};
static Problem problem;

struct Board {
    using value_type = oil_reserve_t;
    Board() : _board_size(problem.get_board_size()) {
        _data.resize(_board_size*_board_size);
    }
    int get_board_size() const { return _board_size; }
    void fill(value_type v) {
        for (int i = 0; i < _data.size(); i++) {
            _data[i] = v;
        }
    }
    value_type* operator[](std::size_t idx) { return _data.data() + _board_size*idx; }
    const value_type* operator[](std::size_t idx) const { return _data.data() + _board_size*idx; }
    void add_polyomino(const Polyomino& poly, int horz_off, int vert_off) {
        const auto relative_positions = poly.get_relative_positions();
        for (auto [rel_i,rel_j] : relative_positions) {
            int i = horz_off + rel_i;
            int j = vert_off + rel_j;
            _data[i*_board_size + j]++;
        }
    }
    std::vector<Point> make_answer() const {
        std::vector<Point> ans;
        for (int i = 0; i < _board_size; i++) {
            for (int j = 0; j < _board_size; j++) {
                int idx = i*_board_size + j;
                if (_data[idx] > 0) {
                    ans.push_back(Point{i,j});
                }
            }
        }
        return ans;
    }
    template <std::ranges::random_access_range Offsets>
    static Board from_offsets(Offsets&& horz_offsets, Offsets&& vert_offsets) {
        Board board;
        const auto& polyominos = problem.get_polyominos();
        for (int k = 0; k < polyominos.size(); k++) {
            const auto& poly = polyominos[k];
            int horz_off = horz_offsets[k];
            int vert_off = vert_offsets[k];
            board.add_polyomino(poly, horz_off, vert_off);
        }
        return board;
    }
    private:
    int _board_size;
    std::vector<value_type> _data;
};

struct Client {
    void init() {
        _cache = Board{};  // reinitialize the board size
        int n = problem.get_board_size();
        _cache.fill(oil_reservation::undef);
        _max_request_count = 2*n*n;
    }
    auto max_request_count() { return _max_request_count; }
    oil_reserve_t dig(Point p) {
        int cache_value = _cache[p.i][p.j];
        if (cache_value != oil_reservation::undef) {
            return cache_value;
        }
        _add_request_count();
        // construct query
        constexpr std::size_t bufsize = 32;
        char buf[bufsize];
        std::snprintf(buf, bufsize, "q 1 %d %d", p.i, p.j);

        // send query
        std::cout << buf << std::endl;

        // recieve answer
        oil_reserve_t v;
        std::cin >> v;
        _cache[p.i][p.j] = v;
        _known_points.emplace_back(p, v);
        return v;
    }
    template <std::ranges::range Range>
    oil_reserve_t predict(Range&& set) {
        _add_request_count();
        std::string query = construct_vector_query('q', set);
        std::cout << query << std::endl;

        oil_reserve_t v;
        std::cin >> v;
        return v;
    }
    oil_reserve_t predict_without_known_cells(const std::vector<Point>& set) {
        std::vector<Point> observe_set;
        for (auto p : set) {
            if (_cache[p.i][p.j] >= 0) continue;
            observe_set.push_back(p);
        }
        if (observe_set.empty()) {
            return 0;
        }
        _add_request_count();
        std::string query = construct_vector_query('q', observe_set);
        std::cout << query << std::endl;

        oil_reserve_t v;
        std::cin >> v;
        return v;
    }
    template <std::ranges::range Range>
    int answer(Range&& set) {
        _add_request_count();
        std::string query = construct_vector_query('a', set);
        std::cout << query << std::endl;

        int check;
        std::cin >> check;
        return check;
    }
    void visualize_board(const Board& board) {
        clear_colors();
        int n = problem.get_board_size();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                Point p {i, j};
                int v = board[i][j];
                if (v > 0) {
                    std::string color = oil_amount_to_color(v);
                    _write_color_query(p, color);
                }
            }
        }
    }
    void clear_colors() {
        int n = problem.get_board_size();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                Point p {i, j};
                std::string color = "#ffffff";  // default
                _write_color_query(p, color);
            }
        }
    }
    bool validate_board(const Board& board) {
        int n = problem.get_board_size();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                oil_reserve_t v = _cache[i][j];
                if (v != oil_reservation::undef && board[i][j] != v) {
                    return false;
                }
            }
        }
        return true;
    }
    template <typename Range>
    bool validate_solution(const Range& horz_offsets, const Range& vert_offsets) {
        const auto& polyominos = problem.get_polyominos();
        for (auto [p,v] : _known_points) {
            oil_reserve_t pred = 0;
            auto [i,j] = p;
            for (std::size_t k = 0; k < horz_offsets.size(); k++) {
                const auto& poly = polyominos[k];
                int rel_i = i - horz_offsets[k];
                int rel_j = j - vert_offsets[k];
                Point rel {rel_i, rel_j};
                if (poly.contains(rel)) {
                    pred++;
                }
            }
            if (pred != v) {
                return false;
            }
        }
        return true;
    }
    oil_reserve_t query_determined_sum(const std::vector<Point>& set) {
        oil_reserve_t sum = 0;
        for (auto [i,j] : set) {
            oil_reserve_t v = _cache[i][j];
            if (v >= 0) {
                sum += v;
            }
        }
        return sum;
    }
    private:
    void _add_request_count() {
        assert(_request_count < _max_request_count);
        _request_count++;
    }
    static std::string _construct_color_query(Point p, const std::string& color) {
        char query[32];
        std::snprintf(query, 32, "#c %d %d %s", p.i, p.j, color.c_str());
        return std::string(query);
    }
    static void _write_color_query(Point p, const std::string& color) {
        auto query = _construct_color_query(p, color);
        std::cout << query << std::endl;
    }
    template <std::ranges::range Range>
    static std::string construct_vector_query(char c, Range&& set) {
        static_assert(std::is_same_v<std::ranges::range_value_t<Range>, Point>);
        constexpr std::size_t bufsize = 16;
        char buf[16];
        std::snprintf(buf, bufsize, "%c %d", c, (int)set.size());
        std::string query(buf);
        query += point_vector_to_string(set);
        return query;
    }
    template <std::ranges::range Range>
    static std::string point_vector_to_string(Range&& set) {
        std::string s;
        for (Point p : set) {
            constexpr std::size_t bufsize = 16;
            char buf[bufsize];
            snprintf(buf, bufsize, " %d %d", p.i, p.j);
            s += std::string(buf);
        }
        return s;
    }
    static std::string oil_amount_to_color(int v) {
        int c = 255 - v*48;
        c = std::max(c, 0);
        char buf[8];
        std::snprintf(buf, 8, "#%x%x%x", c, c, c);
        return std::string(buf);
    }
    int _request_count = 0;
    int _max_request_count;
    using known_point = std::pair<Point, oil_reserve_t>;
    std::vector<known_point> _known_points;
    Board _cache;
};
static Client client;

struct BruteForceSolver {
    BruteForceSolver() {
        const int n = problem.get_board_size();
        for (int i = 0; i < n; i++) {
            _oil_reservations.push_back(std::vector<oil_reserve_t>(n));
        }
    }
    void dig(Point p) {
        std::cerr << "dig()" << std::endl;
        oil_reserve_t v = client.dig(p);
        std::cerr << "v: " << std::endl;
        _oil_reservations[p.i][p.j] = v;
    }
    void answer() {
        const int n = problem.get_board_size();
        std::vector<Point> oil_points;
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (_oil_reservations[i][j] > 0) {
                    Point p {i,j};
                    oil_points.push_back(p);
                }
            }
        }
        int check = client.answer(oil_points);
        assert(check == 1);
    }
    void solve() {
        std::cerr << "solve()" << std::endl;
        const int n = problem.get_board_size();
        std::cerr << "n: " << n << std::endl;
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                Point p {i,j};
                dig(p);
            }
        }
        answer();
    }
    private:
    std::vector<std::vector<oil_reserve_t>> _oil_reservations;
};

struct PredictModel {
    static double mean(int area, int n_oil) {
        double error_param = problem.get_error_param();
        return (area - n_oil)*error_param + n_oil*(1 - error_param);
    }
    static double variance(int area) {
        double error_param = problem.get_error_param();
        return area * error_param * (1 - error_param);
    }
    static double raw_expectation(int area, double observation) {
        double error_param = problem.get_error_param();
        double base = area*error_param;
        double slope = 1 - error_param;
        return (observation - base) / slope;
    }
    static int predict_n_oil(int area, double observation) {
        double raw = raw_expectation(area, observation);
        int pred = std::round(raw);
        int n = problem.get_board_size();
        pred = std::max(0, pred);
        return pred;
    }
};

auto make_line_coordinates(Direction dir, int board_size, int i) {
    std::vector<Point> set;
    for (int j = 0; j < board_size; j++) {
        if (dir == Direction::Horizontal) {
            set.push_back(Point{i,j});
        }
        else {
            set.push_back(Point{j,i});
        }
    }
    return set;
}

struct PointSetConstructor {
    static auto all_points() {
        std::vector<Point> set;
        int n = problem.get_board_size();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                set.push_back(Point{i,j});
            }
        }
        return set;
    }
};

struct NarrowingObserver {
    // TODO: do not make copy of point_set each time
    auto find_dense_area(const std::vector<Point>& point_set, std::size_t target_size) {
        std::size_t n = point_set.size();
        if (n <= target_size) {
            return point_set;
        }
        std::size_t n1 = n/2;
        std::size_t n2 = n - n1;
        auto mid_iter = std::next(point_set.begin(), n1);
        std::vector<Point> span1(point_set.begin(), mid_iter);
        std::vector<Point> span2(mid_iter, point_set.end());
        auto predict_density = [](const auto& span) {
            int area = span.size();
            oil_reserve_t v = client.predict(span);
            double raw_exp = PredictModel::raw_expectation(area, v);
            double pred = raw_exp - client.query_determined_sum(span);
            return pred / area;
        };
        double rho1 = predict_density(span1);
        double rho2 = predict_density(span2);
        if (rho1 > rho2) {
            return find_dense_area(span1, target_size);
        }
        else {
            return find_dense_area(span2, target_size);
        }
    }
};

// Observe lines (i is fixed)
struct ProjectionObserver {
    ProjectionObserver(Direction dir) : _direction(dir) {
        int n = problem.get_board_size();
        _num_observation.resize(n);
        _sum.resize(n);
        _var.resize(n);
    }
    void observe_all() {
        for (int i = 0; i < problem.get_board_size(); i++) {
            observe_line(i);
        }
    }
    void observe_line(int i) {
        auto board_size = problem.get_board_size();
        auto set = make_line_coordinates(_direction, board_size, i);
        int v = client.predict(set);
        _num_observation[i] += 1;
        _sum[i] += v;
        _var[i] += _model.variance(problem.get_board_size());
    }
    std::vector<int> get_predict_values() const {
        int n = problem.get_board_size();
        std::vector<int> res;
        for (int i = 0; i < n; i++) {
            double mean = _sum[i] / _num_observation[i];
            int pred = _model.predict_n_oil(n, mean);
            res.push_back(pred);
        }
        return res;
    }
    private:
    Direction _direction;
    PredictModel _model;
    std::vector<int> _num_observation;
    std::vector<double> _sum;  // sum of all observations
    std::vector<double> _var;
};

struct BfsSolver {
    static constexpr int delta[4][2] = {
        {-1, 0}, {0, 1}, {1, 0}, {0, -1}
    };
    BfsSolver() {
        _board.fill(oil_reservation::undef);
        for (const auto& poly : problem.get_polyominos()) {
            _total_reservation += poly.get_area();
        }
    }
    bool boundary_check(Point p) const {
        int n = problem.get_board_size();
        return 0 <= p.i && p.i < n && 0 <= p.j && p.j < n;
    }
    auto next_point(Point p, std::size_t direction) const {
        auto [di,dj] = delta[direction];
        int ni = p.i + di;
        int nj = p.j + dj;
        Point next {ni, nj};
        bool valid = boundary_check(next);
        return std::make_pair(valid, next);
    }
    bool is_marked(Point p) {
        return _board[p.i][p.j] >= 0;
    }
    // returs reservation amounts for the point
    oil_reserve_t mark_visited(Point p) {
        if (!is_marked(p)) {
            oil_reserve_t v = client.dig(p);
            _board[p.i][p.j] = v;
            _current_reservation += v;
        }
        return _board[p.i][p.j];
    }
    bool found_all() {
        return _current_reservation == _total_reservation;
    }
    Point pick_point() {
        if (_pick_queue.empty()) {
            auto all_points = PointSetConstructor::all_points();
            NarrowingObserver observer;
            const int target_size = 3;
            auto targets = observer.find_dense_area(all_points, target_size);
            for (auto p : targets) {
                _pick_queue.push(p);
            }
        }
        auto p = _pick_queue.front();
        _pick_queue.pop();
        return p;
    }
    void bfs() {
        std::queue<Point> queue;
        Point p = pick_point();
        if (is_marked(p)) {
            return;
        }
        oil_reserve_t v = mark_visited(p);
        if (v > 0) {
            queue.push(p);
        }
        while (!queue.empty()) {
            auto cur = queue.front();
            queue.pop();
            for (std::size_t dir = 0; dir < 4; dir++) {
                auto [valid, next] = next_point(cur, dir);
                if (valid && !is_marked(next)) {
                    oil_reserve_t v = mark_visited(next);
                    if (v > 0) {
                        queue.push(next);
                    }
                }
            }
            if (found_all()) {
                break;
            }
        };
    }
    bool solve() {
        int cnt = 0;
        while (!found_all()) {
            cnt++;
            if (cnt > 1000) {
                std::cerr << "infinite loop detected!" << std::endl;
                break;
            }
            bfs();
        }
        auto ans = _board.make_answer();
        bool check = client.answer(ans);
        return check;
    }
    private:
    oil_reserve_t _current_reservation = 0;
    oil_reserve_t _total_reservation = 0;
    Board _board;
    // number of non-determined points for each row/col
    std::queue<Point> _pick_queue;
};

// Solve 1d histgram to 1d arrangements
struct ProjectionSolver {
    using projection_type = Polyomino::projection_type;
    ProjectionSolver(const projection_type& pred, const std::vector<projection_type>& projections, const std::vector<oil_reserve_t>& determined_values)
        : _pred(pred), _projections(projections), _determined_values(determined_values)
    {}
    struct Solution {
        int penalty;
        std::vector<int> offsets;
        projection_type rest;
    };
    int calc_penalty(const std::vector<oil_reserve_t>& rest) const {
        int penalty = 0;
        for (int j = 0; j < rest.size(); j++) {
            penalty += rest[j]*(rest[j]-1);
        }
        return penalty;
    }
    Solution next_solution(const Solution& current, const projection_type& projection, int offset) const {
        Solution new_sol = current;
        new_sol.offsets.push_back(offset);
        for (int j = 0; j < projection.size(); j++) {
            new_sol.rest[offset+j] -= projection[j];
        }
        new_sol.penalty = calc_penalty(new_sol.rest);
        return new_sol;
    }
    bool validate_solution(const Solution& sol) const {
        int board_size = problem.get_board_size();
        std::vector<oil_reserve_t> hist(board_size);
        for (int k = 0; k < _projections.size(); k++) {
            const auto& proj = _projections[k];
            auto offset = sol.offsets[k];
            for (int j = 0; j < proj.size(); j++) {
                hist[offset+j] += proj[j];
            }
        }
        for (int j = 0; j < board_size; j++) {
            if (_determined_values[j] > 0 && hist[j] < _determined_values[j]) {
                return false;
            }
        }
        return true;
    }
    struct CompareSolution {
        bool operator()(const Solution& lhs, const Solution& rhs) {
            return lhs.penalty > rhs.penalty;
        }
    };
    struct BeamSearch {
        BeamSearch(const projection_type& pred, int n_cand, const ProjectionSolver& ref)
            : _n_cand(n_cand), _parent_ref(ref)
        {
            Solution initial_state;
            initial_state.rest = pred;
            _frontier.push_back(initial_state);
        }
        std::vector<Solution> beam_search(const std::vector<projection_type>& projections) {
            int n_polyominos = projections.size();
            for (int k = 0; k < n_polyominos; k++) {
                const auto& proj = projections[k];
                const int proj_size = proj.size();
                const int max_offset = problem.get_board_size() - proj_size;
                for (const auto& sol : _frontier) {
                    for (int off = 0; off <= max_offset; off++) {
                        Solution next = _parent_ref.next_solution(sol, proj, off);
                        if (k == n_polyominos - 1 && !_parent_ref.validate_solution(next)) {
                            continue;
                        }
                        _candidates.push(next);
                    }
                }
                select_top_cands();
            }
            return _frontier;
        }
        void select_top_cands() {
            _frontier.clear();
            for (int i = 0; i < _n_cand; i++) {
                if (_candidates.empty()) {
                    break;
                }
                Solution sol = _candidates.top();
                _candidates.pop();
                _frontier.push_back(sol);
            }
            _candidates = queue_type{};
        }
        private:
        using queue_type = std::priority_queue<Solution, std::vector<Solution>, CompareSolution>;
        const ProjectionSolver& _parent_ref;
        int _n_cand;
        std::vector<Solution> _frontier;
        queue_type _candidates;
    };
    // resolve offsets of all polyominos
    std::vector<Solution> solve() {
        const int n_cand = 1000;
        BeamSearch runner(_pred, n_cand, *this);
        auto cands = runner.beam_search(_projections);
        int n_debud_cand = std::min(10ul, cands.size());
        for (int i = 0; i < n_debud_cand; i++) {
            const auto& cand = cands[i];
            std::cerr << "penalty = " << cand.penalty;
            std::cerr << ", offsets = ";
            util::print_vector(cand.offsets);
            std::cerr << ", rest = ";
            util::print_vector(cand.rest);
            std::cerr << std::endl;
        }
        return cands;
    }
    private:
    projection_type _pred;
    std::vector<projection_type> _projections;
    std::vector<oil_reserve_t> _determined_values;
};

struct Solution {
    int horz_penalty;
    int vert_penalty;
    std::vector<int> horz_offsets;
    std::vector<int> vert_offsets;
    auto get_penalties() const {
        return std::make_pair(horz_penalty, vert_penalty);
    }
    auto get_offsets() const {
        return std::make_pair(horz_offsets, vert_offsets);
    }
};

struct SolutionPicker {
    using index_t = std::pair<int,int>;  // index to horz solutions and vert solutions
    using search_results_type = std::vector<ProjectionSolver::Solution>;
    SolutionPicker(search_results_type&& horz_solutions, search_results_type&& vert_solutions)
        : _horz_solutions(horz_solutions), _vert_solutions(vert_solutions)
    {
        _make_indices();
        _sort_indices();
        _filter_indices();
    }
    auto pick() {
        if (_indices.size() == 0) {
            return std::make_pair(false,Solution{});
        }
        auto [i,j] = _indices[0];
        int horz_penalty = _horz_solutions[i].penalty;
        int vert_penalty = _vert_solutions[j].penalty;
        const auto& horz_offsets = _horz_solutions[i].offsets;
        const auto& vert_offsets = _vert_solutions[j].offsets;
        Solution sol { horz_penalty, vert_penalty, horz_offsets, vert_offsets };
        return std::make_pair(true, std::move(sol));
    }
    // returns number of remaining candidates
    bool dig_pinpoint() {
        constexpr std::size_t num_max_compare = 1000;
        std::vector<Board> simulated_boards;
        std::size_t num_compare = std::min(num_max_compare, _indices.size());
        if (num_compare <= 1) {
            std::cerr << "No candidates for comparation!" << std::endl;
            return false;
        }
        for (std::size_t rank = 0; rank < num_compare; rank++) {
            auto [i,j] = _indices[rank];
            const auto& horz_offsets = _horz_solutions[i].offsets;
            const auto& vert_offsets = _vert_solutions[j].offsets;
            assert(client.validate_solution(horz_offsets, vert_offsets));
            Board b = Board::from_offsets(horz_offsets, vert_offsets);
            simulated_boards.push_back(std::move(b));
        }
        // minimum number of candidates which can be discarded after pinpoint observation
        int max_score = -1;
        Point dig_point;
        int n = problem.get_board_size();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                std::vector<int> hist(n+1);
                for (int rank = 0; rank < num_compare; rank++) {
                    oil_reserve_t v = simulated_boards[rank][i][j];
                    hist[v]++;
                }
                int score = num_compare - *std::max_element(hist.begin(), hist.end());
                if (score > max_score) {
                    max_score = score;
                    dig_point = Point{i,j};
                }
            }
        }
        if (max_score <= 0) {
            return false;
        }
        std::cerr << "dig: " << dig_point.i << ", " << dig_point.j << std::endl;
        oil_reserve_t v = client.dig(dig_point);
        // invalidate boards
        _filter_indices();
        return true;
    }
    void iter_dig_pinpoint() {
        const int threshold = 1;
        while (_indices.size() > threshold) {
            std::cerr << "size: " << _indices.size() << std::endl;
            bool success = dig_pinpoint();
            if (!success) {
                return;
            }
        }
    }
    private:
    void _make_indices() {
        _indices.clear();
        const int n = problem.get_board_size();
        const int m = problem.get_num_oilfield();
        const std::size_t num_cand_1d = n*m*m/2;
        const int num_horz_cand = std::min(num_cand_1d, _horz_solutions.size());
        const int num_vert_cand = std::min(num_cand_1d, _vert_solutions.size());
        for (int i = 0; i < num_horz_cand; i++) {
            for (int j = 0; j < num_vert_cand; j++) {
                _indices.emplace_back(i,j);
            }
        }
    }
    void _sort_indices() {
        auto compare = [this](index_t idx1, index_t idx2) {
            auto [i1,j1] = idx1;
            auto [i2,j2] = idx2;
            auto p1 = _horz_solutions[i1].penalty + _vert_solutions[j1].penalty;
            auto p2 = _horz_solutions[i2].penalty + _vert_solutions[j2].penalty;
            return p1 < p2;
        };
        std::sort(_indices.begin(), _indices.end(), compare);
    }
    void _filter_indices() {
        std::vector<index_t> valid_indices;
        for (auto [i,j] : _indices) {
            const auto& horz_offsets = _horz_solutions[i].offsets;
            const auto& vert_offsets = _vert_solutions[j].offsets;
            bool valid = client.validate_solution(horz_offsets, vert_offsets);
            if (valid) {
                valid_indices.emplace_back(i, j);
            }
        }
        _indices = valid_indices;
        std::cerr << "Number of remaining indices: " << _indices.size() << std::endl;
    }
    static std::vector<int> pick_large_variance(const search_results_type& solutions) {
        int num_cand = std::max(solutions.size(), solutions.size()/10);  // top 10%
        int board_size = problem.get_board_size();
        std::vector<int> sum(board_size);
        std::vector<int> ssum(board_size);  // square sum
        for (int i = 0; i < num_cand; i++) {
            const auto& rest = solutions[i].rest;
            for (int j = 0; j < board_size; j++) {
                sum[j] += rest[j];
                ssum[j] += rest[j]*rest[j];
            }
        }
        std::vector<int> non_normalized_var(board_size);
        for (int j = 0; j < board_size; j++) {
            non_normalized_var[j] = num_cand*ssum[j] - sum[j]*sum[j];
        }
        // index sort
        std::vector<int> indices(non_normalized_var.size());
        std::iota(indices.begin(), indices.end(), 0);
        std::sort(indices.begin(), indices.end(),
                  [&non_normalized_var](size_t i, size_t j){ return non_normalized_var[i] > non_normalized_var[j]; });
        int num_remeasurement = board_size*4/5;
        std::vector<int> res(num_remeasurement);
        std::copy(indices.begin(), std::next(indices.begin(), num_remeasurement), res.begin());
        return res;
    }
    std::vector<index_t> _indices;
    search_results_type _horz_solutions;
    search_results_type _vert_solutions;
};

struct ProjectionCombinationSolver {
    using projection_type = Polyomino::projection_type;
    ProjectionCombinationSolver()
        : _horz_observer(Direction::Horizontal), _vert_observer(Direction::Vertical)
    {
        // init projections
        for (const auto& poly : problem.get_polyominos()) {
            _horz_projections.push_back(poly.make_projection(Direction::Horizontal));
            _vert_projections.push_back(poly.make_projection(Direction::Vertical));
        }
    }
    SolutionPicker make_candidates(bool shuffle, std::size_t seed) {
        auto solve_1d = [shuffle, seed](Direction dir, auto& observer, auto& projections) {
            std::string dir_str = (dir == Direction::Horizontal) ? "horz" : "vert";
            auto pred = observer.get_predict_values();
            std::cerr << dir_str << " pred: ";
            util::print_vector(pred);
            std::cerr << std::endl;

            int n = problem.get_board_size();
            std::vector<oil_reserve_t> determined;
            for (int i = 0; i < n; i++) {
                auto line = make_line_coordinates(dir, n, i);
                oil_reserve_t v = client.query_determined_sum(line);
                determined.push_back(v);
            }
            std::cerr << "determined: "; util::print_vector(determined); std::cerr << std::endl;

            std::cerr << "search " << dir_str << " candidates" << std::endl;
            if (shuffle) {
                std::mt19937 mt(seed);
                std::vector<int> shuffle_idx(projections.size());
                std::iota(shuffle_idx.begin(), shuffle_idx.end(), 0);
                std::shuffle(shuffle_idx.begin(), shuffle_idx.end(), mt);
                std::vector<projection_type> shuffled_projections;
                for (std::size_t k = 0; k < projections.size(); k++) {
                    shuffled_projections.push_back(projections[shuffle_idx[k]]);
                }
                ProjectionSolver solver(pred, shuffled_projections, determined);
                auto solutions = solver.solve();
                // restore offset order
                auto shuffle_idx_rev = util::index_sort(shuffle_idx);
                for (auto& sol : solutions) {
                    std::vector<int> offsets;
                    for (std::size_t k = 0; k < sol.offsets.size(); k++) {
                        offsets.push_back(sol.offsets[shuffle_idx_rev[k]]);
                    }
                    sol.offsets = offsets;
                }
                return solutions;
            }
            else {
                ProjectionSolver solver(pred, projections, determined);
                auto solutions = solver.solve();
                return solutions;
            }
        };
        auto horz_solutions = solve_1d(Direction::Horizontal, _horz_observer, _horz_projections);
        auto vert_solutions = solve_1d(Direction::Vertical, _vert_observer, _vert_projections);
        SolutionPicker solution_picker(std::move(horz_solutions), std::move(vert_solutions));
        return solution_picker;
    }
    bool submit_answer(const Board& board) {
        std::cerr << "Submit an answer... ";
        client.clear_colors();
        auto ans = board.make_answer();
        int check = client.answer(ans);
        if (check == 1) {
            // successfully found all oils
            std::cerr << "success" << std::endl;
            return true;
        }
        else {
            std::cerr << "failed" << std::endl;
            client.visualize_board(board);
        }
        return false;
    }
    void solve() {
        const int board_size = problem.get_board_size();
        int max_iteration = 1000;
        for (int iteration = 0; iteration < max_iteration; iteration++) {
            util::print_hline();
            std::cerr << iteration << "th iter start" << std::endl;
            // observe all rows + cols
            observe_all();

            // generate valid solution candidates
            bool shuffle = (iteration == 0) ? false : true;
            std::size_t seed = iteration;
            auto solution_picker = make_candidates(shuffle, seed);

            // try submitting before digging (only for first iteration)
            if (iteration == 0) {
                auto [_, sol] = solution_picker.pick();
                debug_print_solution(sol);
                Board board = Board::from_offsets(sol.horz_offsets, sol.vert_offsets);
                bool success = submit_answer(board);
                if (success) {
                    std::cerr << "success on first try!" << std::endl;
                    return;
                }
            }

            // generate solution candidates and filter them by pinpoint observation
            solution_picker.iter_dig_pinpoint();
            constexpr int num_challenge = 1;
            for (int i = 0; i < num_challenge; i++) {
                std::cerr << "try: " << i+1 << std::endl;
                auto [valid, sol] = solution_picker.pick();
                if (valid) {
                    debug_print_solution(sol);
                    Board board = Board::from_offsets(sol.horz_offsets, sol.vert_offsets);
                    bool success = submit_answer(board);
                    if (success) return;
                }
            }
        }
        // Dirty hack to ensure coloring
        client.dig(Point{0,0});
    }
    private:
    void observe_all() {
        _horz_observer.observe_all();
        _vert_observer.observe_all();
    }
    void debug_print_solution(const Solution& sol) {
        auto [horz_penalty, vert_penalty] = sol.get_penalties();
        const auto& [horz_offsets, vert_offsets] = sol.get_offsets();
        auto total_penalty = horz_penalty + vert_penalty;
        std::cerr
            << "penalty: " << horz_penalty << " + " << vert_penalty
            << " = " << total_penalty << std::endl;
        std::cerr << "horz_offsets: ";
        util::print_vector(horz_offsets);
        std::cerr << std::endl;
        std::cerr << "vert_offsets: ";
        util::print_vector(vert_offsets);
        std::cerr << std::endl;
    }

    ProjectionObserver _horz_observer;
    ProjectionObserver _vert_observer;
    // xy projection of polyominos
    std::vector<projection_type> _horz_projections;
    std::vector<projection_type> _vert_projections;
};

void init_problem() {
    int board_size;
    int num_oilfield;
    double error_param;
    std::cin >> board_size >> num_oilfield >> error_param;

    std::vector<Polyomino> oil_fields;
    for (int k = 0; k < num_oilfield; k++) {
        int area;
        std::vector<Point> relative_positions;
        std::cin >> area;
        for (int l = 0; l < area; l++) {
            int i, j;
            std::cin >> i >> j;
            Point pos {i, j};
            relative_positions.push_back(pos);
        }
        Polyomino poly {area, relative_positions};
        oil_fields.push_back(poly);
    }

    problem.init(board_size, num_oilfield, error_param, oil_fields);
    client.init();
}

}  // namespace ahc

int main() {
    using ahc::problem;
    ahc::init_problem();
    int m = problem.get_num_oilfield();
    if (m < 9) {
        ahc::ProjectionCombinationSolver solver;
        solver.solve();
    }
    else {
        ahc::BfsSolver solver;
        solver.solve();
    }
}
