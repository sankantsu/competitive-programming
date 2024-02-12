#include <cstdio>
#include <cmath>
#include <algorithm>
#include <cassert>
#include <iostream>
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

struct Board {
    using value_type = oil_reserve_t;
    Board(int board_size)
        : _board_size(board_size), _data(board_size*board_size)
    {}
    int get_board_size() const { return _board_size; }
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
    oil_reserve_t dig(Point p) {
        // construct query
        constexpr std::size_t bufsize = 32;
        char buf[bufsize];
        std::snprintf(buf, bufsize, "q 1 %d %d", p.i, p.j);

        // send query
        std::cout << buf << std::endl;

        // recieve answer
        oil_reserve_t v;
        std::cin >> v;
        return v;
    }
    oil_reserve_t predict(const std::vector<Point>& set) {
        std::string query = construct_vector_query('q', set);
        std::cout << query << std::endl;

        oil_reserve_t v;
        std::cin >> v;
        return v;
    }
    int answer(const std::vector<Point>& set) {
        std::string query = construct_vector_query('a', set);
        std::cout << query << std::endl;

        int check;
        std::cin >> check;
        return check;
    }
    void visualize_board(const Board& board) {
        int n = board.get_board_size();
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
    private:
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
};

struct Problem {
    Problem(int board_size, int num_oilfield, double error_param, std::vector<Polyomino> oil_fields)
        : _board_size(board_size), _num_oilfield(num_oilfield), _error_param(error_param),
          _oil_fields(oil_fields)
    {
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

struct BruteForceSolver {
    BruteForceSolver(const Problem& board)
    : _board(board)
    {
        const int n = _board.get_board_size();
        for (int i = 0; i < n; i++) {
            _oil_reservations.push_back(std::vector<oil_reserve_t>(n));
        }
    }
    void dig(Point p) {
        oil_reserve_t v = _client.dig(p);
        _oil_reservations[p.i][p.j] = v;
    }
    void answer() {
        const int n = _board.get_board_size();
        std::vector<Point> oil_points;
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (_oil_reservations[i][j] > 0) {
                    Point p {i,j};
                    oil_points.push_back(p);
                }
            }
        }
        int check = _client.answer(oil_points);
        assert(check == 1);
    }
    void solve() {
        const int n = _board.get_board_size();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                Point p {i,j};
                dig(p);
            }
        }
        answer();
    }
    private:
    Problem _board;
    Client _client;
    std::vector<std::vector<oil_reserve_t>> _oil_reservations;
};

struct PredictModel {
    PredictModel(double error_param) : _error_param(error_param) {}
    double mean(int area, int n_oil) {
        return (area - n_oil)*_error_param + n_oil*(1 - _error_param);
    }
    double variance(int area) {
        return area * _error_param * (1 - _error_param);
    }
    int predict_n_oil(int area, double observation) {
        double base = area*_error_param;
        double slope = 1 - _error_param;
        return std::round((observation - base) / slope);
    }
    private:
    double _error_param;
};

// Observe lines (i is fixed)
struct ProjectionObserver {
    ProjectionObserver(Direction dir, int board_size, double error_param)
        : _direction(dir), _board_size(board_size),
          _num_observation(board_size), _sum(board_size), _var(board_size), _model(error_param)
    {}
    void observe_all() {
        for (int i = 0; i < _board_size; i++) {
            observe_line(i);
        }
    }
    void observe_line(int i) {
        std::vector<Point> set;
        for (int j = 0; j < _board_size; j++) {
            if (_direction == Direction::Horizontal) {
                set.push_back(Point{i,j});
            }
            else {
                set.push_back(Point{j,i});
            }
        }
        int v = _client.predict(set);
        _num_observation[i] += 1;
        _sum[i] += v;
        _var[i] += _model.variance(_board_size);
    }
    std::vector<int> get_predict_values() {
        std::vector<int> res;
        for (int i = 0; i < _board_size; i++) {
            double mean = _sum[i] / _num_observation[i];
            int pred = _model.predict_n_oil(_board_size, mean);
            pred = std::max(0, std::min(_board_size, pred));
            res.push_back(pred);
        }
        return res;
    }
    private:
    Direction _direction;
    int _board_size;
    Client _client;
    PredictModel _model;
    std::vector<int> _num_observation;
    std::vector<double> _sum;  // sum of all observations
    std::vector<double> _var;
};

// Solve 1d histgram to 1d arrangements
struct ProjectionSolver {
    using projection_type = Polyomino::projection_type;
    ProjectionSolver(const projection_type& pred, const std::vector<projection_type>& projections)
        : _pred(pred), _projections(projections)
    {}
    struct Solution {
        int penalty;
        std::vector<int> offsets;
        projection_type rest;
        Solution next(const projection_type& projection, int offset) const {
            Solution new_sol = *this;
            new_sol.offsets.push_back(offset);
            for (int j = 0; j < projection.size(); j++) {
                new_sol.rest[offset+j] -= projection[j];
            }
            new_sol.penalty = new_sol.calc_penalty();
            return new_sol;
        }
        int calc_penalty() const {
            int penalty = 0;
            for (int j = 0; j < rest.size(); j++) {
                if (rest[j] < 0) {
                    penalty += rest[j]*(rest[j]-1);
                }
            }
            return penalty;
        }
    };
    struct CompareSolution {
        bool operator()(const Solution& lhs, const Solution& rhs) {
            return lhs.penalty > rhs.penalty;
        }
    };
    struct BeamSearch {
        BeamSearch(const projection_type& pred, int n_cand)
            : _board_size(pred.size()), _n_cand(n_cand)
        {
            Solution initial_state;
            initial_state.rest = pred;
            _frontier.push_back(initial_state);
        }
        std::vector<Solution> beam_search(const std::vector<projection_type>& projections) {
            for (int k = 0; k < projections.size(); k++) {
                const auto& proj = projections[k];
                const int proj_size = proj.size();
                const int max_offset = _board_size - proj_size;
                for (const auto& sol : _frontier) {
                    for (int off = 0; off <= max_offset; off++) {
                        Solution next = sol.next(proj, off);
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
        int _board_size;
        int _n_cand;
        std::vector<Solution> _frontier;
        queue_type _candidates;
    };
    // resolve offsets of all polyominos
    std::vector<Solution> solve() {
        const int n_cand = 1000;
        BeamSearch runner(_pred, n_cand);
        auto cands = runner.beam_search(_projections);
        return cands;
    }
    private:
    projection_type _pred;
    std::vector<projection_type> _projections;
};

struct ProjectionCombinationSolver {
    using projection_type = Polyomino::projection_type;
    ProjectionCombinationSolver(const Problem& problem)
        : _problem(problem),
          _horz_observer(Direction::Horizontal, problem.get_board_size(), problem.get_error_param()),
          _vert_observer(Direction::Vertical, problem.get_board_size(), problem.get_error_param())
    {
        // init projections
        for (const auto& poly : _problem.get_polyominos()) {
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
        {}
        Solution pick() {
            auto [i,j] = _idx;
            int horz_penalty = _horz_solutions[i].penalty;
            int vert_penalty = _vert_solutions[j].penalty;
            const auto& horz_offsets = _horz_solutions[i].offsets;
            const auto& vert_offsets = _vert_solutions[j].offsets;
            Solution sol { horz_penalty, vert_penalty, horz_offsets, vert_offsets };
            update_index();
            return sol;
        }
        private:
        void update_index() {
            auto [i,j] = _idx;
            int horz_penalty_delta = _horz_solutions[i+1].penalty - _horz_solutions[i].penalty;
            int vert_penalty_delta = _vert_solutions[j+1].penalty - _vert_solutions[j].penalty;
            if (horz_penalty_delta < vert_penalty_delta) {
                _idx = std::make_pair(i+1, j);
            }
            else {
                _idx = std::make_pair(i, j+1);
            }
        }
        index_t _idx;
        search_results_type _horz_solutions;
        search_results_type _vert_solutions;
    };
    Board restore_board(const Solution& sol) {
        const auto& [horz_offsets, vert_offsets] = sol.get_offsets();
        Board board(_problem.get_board_size());
        const auto& polyominos = _problem.get_polyominos();
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
        auto polyominos = _problem.get_polyominos();
        ProjectionSolver horz_solver(horz_pred, _horz_projections);
        ProjectionSolver vert_solver(vert_pred, _vert_projections);
        auto horz_solutions = horz_solver.solve();
        auto vert_solutions = vert_solver.solve();
        SolutionPicker solution_picker(std::move(horz_solutions), std::move(vert_solutions));
        return solution_picker;
    }
    void solve() {
        constexpr int num_observe = 5;
        for (int i = 0; i < num_observe; i++) {
            util::print_hline();
            std::cerr << i << "th iter start" << std::endl;
            observe_all();
            auto solution_picker = make_candidates();
            constexpr int num_challenge = 10;
            for (int i = 0; i < num_challenge; i++) {
                std::cerr << "try: " << i+1 << std::endl;
                auto sol = solution_picker.pick();
                debug_print_solution(sol);
                auto board = restore_board(sol);
                auto ans = board.make_answer();
                int check = _client.answer(ans);
                if (check == 1) {  // successfully found all oils
                    return;
                }
                else {
                    _client.visualize_board(board);
                }
            }
        }
        // Dirty hack to ensure coloring
        _client.dig(Point{0,0});
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

    Problem _problem;
    Client _client;
    ProjectionObserver _horz_observer;
    ProjectionObserver _vert_observer;
    // xy projection of polyominos
    std::vector<projection_type> _horz_projections;
    std::vector<projection_type> _vert_projections;
};

Problem parse_input() {
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

    Problem problem {board_size, num_oilfield, error_param, oil_fields};
    return problem;
}

}

int main() {
    ahc::Problem problem = ahc::parse_input();
    ahc::ProjectionCombinationSolver solver(problem);
    solver.solve();
}
