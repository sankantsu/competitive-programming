#include <cstdio>
#include <cmath>
#include <iostream>
#include <vector>
#include <string>

namespace util {

template <typename T>
void print_vector(const std::vector<T>& v, std::ostream& ostream=std::cerr) {
    for (auto x : v) {
        ostream << x << " ";
    }
    ostream << std::endl;
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
    auto get_relative_positions() const { return _relative_positions; }
    std::vector<int> make_projection(Direction dir) const {
        int size = (dir == Direction::Horizontal) ? _horz_size : _vert_size;
        std::vector<int> res(size);
        for (auto [i,j] : _relative_positions) {
            if (dir == Direction::Horizontal) {
                res[i]++;
            }
            else {
                res[j]++;
            }
        }
        return res;
    }
    private:
    int _area;
    std::vector<Point> _relative_positions;
    int _horz_size;
    int _vert_size;
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
};

struct Problem {
    Problem(int board_size, int num_oilfield, double error_param, std::vector<Polyomino> oil_fields)
        : _board_size(board_size), _num_oilfield(num_oilfield), _error_param(error_param),
          _oil_fields(oil_fields)
    { }
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
            res.push_back(_model.predict_n_oil(_board_size, mean));
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
    ProjectionSolver(const std::vector<int>& pred, const std::vector<std::vector<int>>& projections)
        : _pred(pred), _projections(projections)
    {}
    // resolve offsets of all polyominos
    std::vector<int> solve() {
        std::vector<int> offsets;
        for (const auto& proj : _projections) {
            int offset = find_best_offset(_pred, proj);
            offsets.push_back(offset);
        }
        return offsets;
    }
    static int find_best_offset(const std::vector<int>& pred, const std::vector<int>& projection) {
        constexpr int inf_penalty = 2*20*20*20;
        int min_penalty = inf_penalty;
        int best_offset = -1;
        int max_i = projection.size();
        int max_offset = pred.size() - max_i;
        std::cerr << "pred, proj:" << std::endl;
        util::print_vector(pred);
        util::print_vector(projection);
        for (int off = 0; off < max_offset; off++) {
            int penalty = calc_penalty(pred, projection, off);
            std::cerr << "off, penalty: " << off << ", " << penalty << std::endl;
            // TODO: consider multiple possibilities (not only first match)
            if (penalty < min_penalty) {
                best_offset = off;
                min_penalty = penalty;
            }
        }
        return best_offset;
    }
    static int calc_penalty(const std::vector<int>& pred, const std::vector<int>& projection, int offset) {
        int penalty = 0;
        for (int i = 0; i < projection.size(); i++) {
            int rest = pred[i+offset] - projection[i];
            if (rest < 0) {
                penalty += rest*rest;
            }
        }
        return penalty;
    }
    private:
    std::vector<int> _pred;
    std::vector<std::vector<int>> _projections;
};

struct ProjectionCombinationSolver {
    ProjectionCombinationSolver(const Problem& problem)
        : _problem(problem),
          _horz_observer(Direction::Horizontal, problem.get_board_size(), problem.get_error_param()),
          _vert_observer(Direction::Vertical, problem.get_board_size(), problem.get_error_param())
    {}
    auto restore_board(const std::vector<int>& horz_offsets, const std::vector<int>& vert_offsets) {
        std::vector<std::vector<int>> board;
        for (int i = 0; i < _problem.get_board_size(); i++) {
            board.push_back(std::vector<int>(_problem.get_board_size()));
        }
        const auto polyominos = _problem.get_polyominos();
        for (int k = 0; k < polyominos.size(); k++) {
            const auto& poly = polyominos[k];
            int horz_off = horz_offsets[k];
            int vert_off = vert_offsets[k];
            const auto relative_positions = poly.get_relative_positions();
            for (auto [i,j] : relative_positions) {
                board[horz_off+i][vert_off+j]++;
            }
        }
        return board;
    }
    void solve() {
        constexpr int num_observe = 4;
        for (int i = 0; i < num_observe; i++) {
            _horz_observer.observe_all();
            _vert_observer.observe_all();
        }
        auto horz_pred = _horz_observer.get_predict_values();
        auto vert_pred = _vert_observer.get_predict_values();
        /* std::cerr << "horz_pred: "; */
        /* util::print_vector(horz_pred); */
        auto polyominos = _problem.get_polyominos();
        std::vector<std::vector<int>> horz_projections;
        std::vector<std::vector<int>> vert_projections;
        for (const auto& poly : polyominos) {
            horz_projections.push_back(poly.make_projection(Direction::Horizontal));
            vert_projections.push_back(poly.make_projection(Direction::Vertical));
        }
        ProjectionSolver horz_solver(horz_pred, horz_projections);
        auto horz_offsets = horz_solver.solve();
        std::cerr << "horz_offsets: ";
        util::print_vector(horz_offsets);
        ProjectionSolver vert_solver(vert_pred, vert_projections);
        auto vert_offsets = vert_solver.solve();
        std::cerr << "vert_offsets: ";
        util::print_vector(vert_offsets);

        auto board = restore_board(horz_offsets, vert_offsets);
        int n = _problem.get_board_size();
        std::vector<Point> ans;
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (board[i][j] > 0) {
                    ans.push_back(Point{i,j});
                }
            }
        }
        _client.answer(ans);
    }
    private:
    Problem _problem;
    Client _client;
    ProjectionObserver _horz_observer;
    ProjectionObserver _vert_observer;
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
