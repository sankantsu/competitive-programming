#include <cstdio>
#include <cmath>
#include <cassert>
#include <iostream>
#include <numeric>
#include <algorithm>
#include <vector>
#include <string>
#include <queue>
#include <map>

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
    std::vector<Point> make_answer() {
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
        return v;
    }
    oil_reserve_t predict(const std::vector<Point>& set) {
        _add_request_count();
        std::string query = construct_vector_query('q', set);
        std::cout << query << std::endl;

        oil_reserve_t v;
        std::cin >> v;
        return v;
    }
    int answer(const std::vector<Point>& set) {
        _add_request_count();
        std::string query = construct_vector_query('a', set);
        std::cout << query << std::endl;

        int check;
        std::cin >> check;
        return check;
    }
    void visualize_board(const Board& board) {
        int n = problem.get_board_size();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                int v = board[i][j];
                std::string color = "#ffffff";  // default
                if (v > 0) {
                    color = oil_amount_to_color(v);
                }
                char query[32];
                std::snprintf(query, 32, "#c %d %d %s", i, j, color.c_str());
                std::cout << query << std::endl;
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
    private:
    void _add_request_count() {
        assert(_request_count < _max_request_count);
        _request_count++;
    }
    static std::string construct_vector_query(char c, const std::vector<Point>& set) {
        constexpr std::size_t bufsize = 16;
        char buf[16];
        std::snprintf(buf, bufsize, "%c %d", c, (int)set.size());
        std::string query(buf);
        query += point_vector_to_string(set);
        return query;
    }
    static std::string point_vector_to_string(const std::vector<Point>& set) {
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
    static int predict_n_oil(int area, double observation) {
        double error_param = problem.get_error_param();
        double base = area*error_param;
        double slope = 1 - error_param;
        return std::round((observation - base) / slope);
    }
};

// Observe lines (i is fixed)
struct ProjectionObserver {
    ProjectionObserver(Direction dir) : _direction(dir) {
        int n = problem.get_board_size();
        _num_observation.resize(n);
        _sum.resize(n);
        _var.resize(n);
        _determined_sum.resize(n);
        for (int i = 0; i < n; i++) {
            _determined_sum[i] = oil_reservation::undef;
        }
    }
    void observe_all() {
        for (int i = 0; i < problem.get_board_size(); i++) {
            observe_line(i);
        }
    }
    auto make_line_coordinates(int i) {
        std::vector<Point> set;
        for (int j = 0; j < problem.get_board_size(); j++) {
            if (_direction == Direction::Horizontal) {
                set.push_back(Point{i,j});
            }
            else {
                set.push_back(Point{j,i});
            }
        }
        return set;
    }
    void observe_line(int i) {
        auto set = make_line_coordinates(i);
        int v = client.predict(set);
        _num_observation[i] += 1;
        _sum[i] += v;
        _var[i] += _model.variance(problem.get_board_size());
    }
    void dig_line(int i) {
        auto set = make_line_coordinates(i);
        std::vector<oil_reserve_t> values;
        oil_reserve_t sum = 0;
        for (auto point : set) {
            oil_reserve_t v = client.dig(point);
            values.push_back(v);
            sum += v;
        }
        _determined_sum[i] = sum;
        _determined_lines.emplace(i, std::move(values));
    }
    std::vector<int> get_predict_values() const {
        int n = problem.get_board_size();
        std::vector<int> res;
        for (int i = 0; i < n; i++) {
            if (_determined_sum[i] != oil_reservation::undef) {
                res.push_back(_determined_sum[i]);
            }
            else {
                double mean = _sum[i] / _num_observation[i];
                int pred = _model.predict_n_oil(n, mean);
                pred = std::max(0, std::min(n, pred));
                res.push_back(pred);
            }
        }
        return res;
    }
    std::vector<int> get_determined_values() const { return _determined_sum; }
    private:
    Direction _direction;
    PredictModel _model;
    std::vector<int> _num_observation;
    std::vector<double> _sum;  // sum of all observations
    std::vector<double> _var;
    std::vector<oil_reserve_t> _determined_sum;
    std::map<int, std::vector<oil_reserve_t>> _determined_lines;
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
        constexpr int inf_penalty = 1000000;
        int penalty = 0;
        for (int j = 0; j < rest.size(); j++) {
            if (rest[j] < 0 && _determined_values[j] >= 0) {
                penalty += inf_penalty;
            }
            else if (rest[j] < 0) {
                penalty += rest[j]*(rest[j]-1);
            }
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
        for (int j = 0; j < board_size; j++) {
            if (_determined_values[j] >= 0 && sol.rest[j] != 0) {
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
        }
        auto pick() {
            if (_current_rank >= _indices.size()) {
                return std::make_pair(false,Solution{});
            }
            auto [i,j] = _indices[_current_rank++];
            std::cerr << "rank, i,j: " << _current_rank << ", " << i << ", " << j << std::endl;
            int horz_penalty = _horz_solutions[i].penalty;
            int vert_penalty = _vert_solutions[j].penalty;
            const auto& horz_offsets = _horz_solutions[i].offsets;
            const auto& vert_offsets = _vert_solutions[j].offsets;
            Solution sol { horz_penalty, vert_penalty, horz_offsets, vert_offsets };
            return std::make_pair(true, std::move(sol));
        }
        int consider_dig_index(Direction dir) const {
            if (dir == Direction::Horizontal) {
                return pick_large_variance(_horz_solutions)[0];
            }
            else {
                return pick_large_variance(_vert_solutions)[0];
            }
        }
        private:
        void _make_indices() {
            _indices.clear();
            const int num_horz_cand = std::min(20ul, _horz_solutions.size());
            const int num_vert_cand = std::min(20ul, _vert_solutions.size());
            for (int i = 0; i < num_horz_cand; i++) {
                for (int j = 0; j < num_vert_cand; j++) {
                    _indices.emplace_back(i,j);
                }
            }
            std::cerr << "_indices.size(): " << _indices.size() << std::endl;
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
        std::size_t _current_rank = 0;
        index_t _idx;
        std::vector<index_t> _indices;
        search_results_type _horz_solutions;
        search_results_type _vert_solutions;
    };
    Board restore_board(const Solution& sol) {
        const auto& [horz_offsets, vert_offsets] = sol.get_offsets();
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
    SolutionPicker make_candidates() {
        auto horz_pred = _horz_observer.get_predict_values();
        auto vert_pred = _vert_observer.get_predict_values();
        std::cerr << "horz_pred = ";
        util::print_vector(horz_pred);
        std::cerr << ", vert_pred = ";
        util::print_vector(vert_pred);
        std::cerr << std::endl;
        auto horz_determined = _horz_observer.get_determined_values();
        auto vert_determined = _vert_observer.get_determined_values();
        auto polyominos = problem.get_polyominos();
        ProjectionSolver horz_solver(horz_pred, _horz_projections, horz_determined);
        ProjectionSolver vert_solver(vert_pred, _vert_projections, vert_determined);
        std::cerr << "search horizontal candidates" << std::endl;
        auto horz_solutions = horz_solver.solve();
        std::cerr << "search vertical candidates" << std::endl;
        auto vert_solutions = vert_solver.solve();
        SolutionPicker solution_picker(std::move(horz_solutions), std::move(vert_solutions));
        return solution_picker;
    }
    void solve() {
        const int board_size = problem.get_board_size();
        const int num_observe = 5;
        const int num_dig_line = (client.max_request_count() - 2*board_size*num_observe)/board_size;
        int dig_idx;
        Direction dig_dir = Direction::Horizontal;
        auto switch_direction = [&dig_dir] {
            dig_dir = (dig_dir == Direction::Horizontal) ? Direction::Vertical
                                                         : Direction::Horizontal;
        };
        for (int i = 0; i < num_observe + num_dig_line; i++) {
            util::print_hline();
            std::cerr << i << "th iter start" << std::endl;
            if (i < num_observe) {
                observe_all();
            }
            else {
                dig_line(dig_dir, dig_idx);
                switch_direction();
            }
            auto solution_picker = make_candidates();
            dig_idx = solution_picker.consider_dig_index(dig_dir);
            constexpr int num_challenge = 1;
            for (int i = 0; i < num_challenge; i++) {
                std::cerr << "try: " << i+1 << std::endl;
                Board board;
                bool check_ans = true;
                while (true) {
                    auto [valid, sol] = solution_picker.pick();
                    check_ans = check_ans & valid;
                    if (!valid) break;
                    board = restore_board(sol);
                    if (!client.validate_board(board)) {
                        continue;
                    }
                    break;
                }
                if (check_ans) {
                    std::cerr << "Submit an answer... ";
                    auto ans = board.make_answer();
                    int check = client.answer(ans);
                    if (check == 1) {  // successfully found all oils
                        std::cerr << "success" << std::endl;
                        return;
                    }
                    else {
                        std::cerr << "failed" << std::endl;
                        client.visualize_board(board);
                    }
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
    void dig_line(Direction dir, int idx) {
        if (dir == Direction::Horizontal) {
            _horz_observer.dig_line(idx);
        }
        else {
            _vert_observer.dig_line(idx);
        }
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
    if (m < 10) {
        ahc::ProjectionCombinationSolver solver;
        solver.solve();
    }
    else {
        ahc::BruteForceSolver solver;
        solver.solve();
    }
}
