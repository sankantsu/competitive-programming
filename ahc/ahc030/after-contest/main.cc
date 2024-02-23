#include <cstdio>
#include <cmath>
#include <cassert>
#include <cwchar>
#include <type_traits>
#include <ranges>
#include <iostream>
#include <numeric>
#include <algorithm>
#include <vector>
#include <string>

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

using value_t = int;

namespace oil_reservation {
    constexpr value_t undef = -1;
}

struct Point {
    int i;
    int j;
};

struct Polyomino {
    using projection_type = std::vector<value_t>;
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
    bool contains(Point point) const {
        for (auto p : _relative_positions) {
            if (p.i == point.i && p.j == point.j) {
                return true;
            }
        }
        return false;
    }
    auto bbox_size() const {
        int max_i = -1;
        int max_j = -1;
        for (auto p : _relative_positions) {
            if (p.i > max_i) {
                max_i = p.i;
            }
            if (p.j > max_j) {
                max_j = p.j;
            }
        }
        return std::make_pair(max_i+1, max_j+1);
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
        for (const auto& poly : _oil_fields) {
            _total_reservation += poly.get_area();
        }
    }
    int get_board_size() const { return _board_size; }
    int get_num_oilfield() const { return _num_oilfield; }
    double get_error_param() const { return _error_param; }
    const std::vector<Polyomino>& get_polyominos() const { return _oil_fields; }
    auto get_total_reservation() const { return _total_reservation; }
    private:
    int _board_size;
    int _num_oilfield;
    double _error_param;
    std::vector<Polyomino> _oil_fields;
    std::size_t _total_reservation;
};
static Problem problem;

struct Board {
    using value_type = value_t;
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
    value_t dig(Point p) {
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
        value_t v;
        std::cin >> v;
        _cache[p.i][p.j] = v;
        _known_points.emplace_back(p, v);
        return v;
    }
    template <std::ranges::range Range>
    value_t predict(Range&& set) {
        _add_request_count();
        std::string query = construct_vector_query('q', set);
        std::cout << query << std::endl;

        value_t v;
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
                value_t v = _cache[i][j];
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
            value_t pred = 0;
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
    value_t query_determined_sum(const std::vector<Point>& set) {
        value_t sum = 0;
        for (auto [i,j] : set) {
            value_t v = _cache[i][j];
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
    using known_point = std::pair<Point, value_t>;
    std::vector<known_point> _known_points;
    Board _cache;
};
static Client client;

struct PredictModel {
    void init() {
        _error_param = problem.get_error_param();
        auto n = problem.get_board_size();
        _likelihood_cache.resize(n*n*n*n*n*n);
        std::fill(_likelihood_cache.begin(), _likelihood_cache.end(), -1.0);
    }
    double mean(int area, int hypo) {
        return (area - hypo)*_error_param + hypo*(1 - _error_param);
    }
    double variance(int area) {
        return area * _error_param * (1 - _error_param);
    }
    double prediction(int area, int observation) {
        double base = area*_error_param;
        double slope = 1 - _error_param;
        return (observation - base) / slope;
    }
    // Given area size and hypothetical reservation amount,
    // calculates the likelihood that observation yields a value x
    double likelihood(int area, int hypo, int x) {
        double& cached_value = _likelihood_cache_at(area, hypo, x);
        if (cached_value != _cache_empty) {
            return cached_value;
        }
        double mu = mean(area, hypo);
        double sigma = std::sqrt(variance(area));
        auto cdf = [mu, sigma](double x) {
            return cdf_normal(mu, sigma, x);
        };
        if (x == 0) {
            cached_value = cdf(0.5);
        }
        else {
            cached_value = cdf(x + 0.5) - cdf(x - 0.5);
        }
        return cached_value;
    }
    // https://cpprefjp.github.io/reference/cmath/erf.html
    static double cdf_normal(double mu, double sigma, double x) {
        constexpr double sqrt_2 = 1.4142135623730951;
        double x_normalized = (x - mu) / (sqrt_2 * sigma);
        return 0.5 * (1 + std::erf(x_normalized));
    }
    private:
    static constexpr double _cache_empty = -1.;
    double& _likelihood_cache_at(int area, int hypo, int x) {
        auto n = problem.get_board_size();
        return _likelihood_cache[n*n*n*n*area + n*n*hypo + x];
    }
    double _error_param;
    std::vector<double> _likelihood_cache;
};
static PredictModel predict_model;

struct Hypothesis {
    double prob;
    std::vector<Point> offsets;
    Board to_board() const {
        Board board;
        const auto& polyominos = problem.get_polyominos();
        for (std::size_t k = 0; k < polyominos.size(); k++) {
            const auto& poly = polyominos[k];
            auto [i,j] = offsets[k];
            board.add_polyomino(poly, i, j);
        }
        return board;
    }
};
using Pool = std::vector<Hypothesis>;

struct Observation {
    static double likelihood(const std::vector<Point>& point_set, const Hypothesis& hypo, int x) {
        Board board = hypo.to_board();
        value_t hypo_reservation = 0;
        for (auto [i,j] : point_set) {
            hypo_reservation += board[i][j];
        }
        return predict_model.likelihood(point_set.size(), hypo_reservation, x);
    }
    static double mutual_information(const std::vector<Point>& point_set, const Pool& pool, const std::vector<value_t>& count) {
        /* value_t max_y = problem.get_total_reservation(); */
        value_t max_y = problem.get_total_reservation();
        std::vector<std::vector<double>> likelihoods(pool.size(), std::vector<double>(max_y + 1));
        // calculates marginal distribution of observation value y
        std::vector<double> prob_y(max_y+1);
        for (std::size_t k = 0; k < pool.size(); k++) {
            const auto& hypo = pool[k];
            for (value_t y = 0; y <= max_y; y++) {
                double lh = predict_model.likelihood(point_set.size(), count[k], y);
                likelihoods[k][y] = lh;
                prob_y[y] += hypo.prob * lh;
            }
        }
        // calculates mutual information
        double mi = 0;
        for (std::size_t k = 0; k < pool.size(); k++) {
            const auto& hypo = pool[k];
            for (value_t y = 0; y <= max_y; y++) {
                double lh = likelihoods[k][y];
                double delta = hypo.prob * lh * std::log(lh / prob_y[y]);
                if (!std::isnan(delta)) {
                    mi += delta;
                }
            }
        }
        return mi;
    }
};

namespace _detail {

void _enumerate_helper(int depth, std::vector<Point> acc, double prob, const std::vector<std::vector<Point>>& possible_offsets, std::vector<Hypothesis>& hypotheses) {
    if (depth == possible_offsets.size()) {
        hypotheses.push_back(Hypothesis {prob, acc});
        return;
    }
    for (auto off : possible_offsets[depth]) {
        std::vector<Point> v = acc;
        v.push_back(off);
        _enumerate_helper(depth+1, std::move(v), prob, possible_offsets, hypotheses);
    }
}

std::vector<Point> _possible_offsets(const Polyomino& poly) {
    auto [height, width] = poly.bbox_size();
    int n = problem.get_board_size();
    int max_i = n - height;
    int max_j = n - width;
    std::vector<Point> offsets;
    for (int i = 0; i <= max_i; i++) {
        for (int j = 0; j <= max_j; j++) {
            offsets.push_back(Point{i,j});
        }
    }
    return offsets;
}

}

Pool enumerate_all_arrangement() {
    std::vector<std::vector<Point>> possible_offsets;
    for (const auto& poly : problem.get_polyominos()) {
        possible_offsets.push_back(_detail::_possible_offsets(poly));
    }
    std::size_t num_hypothesis = 1;
    for (const auto& po : possible_offsets) {
        num_hypothesis *= po.size();
    }
    const double prob = 1.0 / num_hypothesis;
    std::vector<Hypothesis> hypotheses;
    _detail::_enumerate_helper(0, std::vector<Point>{}, prob, possible_offsets, hypotheses);
    return Pool {hypotheses};
}

auto make_observation_set(const Pool& pool) {
    std::cerr << "make_observation_set()" << std::endl;
    std::vector<Board> boards;
    boards.reserve(pool.size());
    for (const auto& h : pool) {
        boards.push_back(h.to_board());
    }

    std::vector<int> count(pool.size());
    auto add_point = [&count, &boards](Point p) {
        for (std::size_t k = 0; k < count.size(); k++) {
            count[k] += boards[k][p.i][p.j];
        }
    };
    auto remove_point = [&count, &boards](Point p) {
        for (std::size_t k = 0; k < count.size(); k++) {
            count[k] -= boards[k][p.i][p.j];
        }
    };

    std::vector<std::pair<Point,double>> point_vec;
    int n = problem.get_board_size();
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            Point p {i,j};
            std::vector<Point> singleton {p};
            add_point(p);
            double mi = Observation::mutual_information(singleton, pool, count);
            remove_point(p);
            point_vec.emplace_back(p, mi);
        }
    }
    std::sort(point_vec.begin(), point_vec.end(),
              [](auto lhs, auto rhs) { return lhs.second > rhs.second; });

    double max_score = 0;
    std::vector<Point> observation_set;
    for (auto [p,_] : point_vec) {
        observation_set.push_back(p);
        add_point(p);
        /* util::print_vector(count); std::cerr << std::endl; */
        double mi = Observation::mutual_information(observation_set, pool, count);
        double cost_inv = std::sqrt(observation_set.size());
        double score = mi * cost_inv;
        if (score < max_score) {
            remove_point(p);
            observation_set.pop_back();
        }
        else {
            max_score = score;
        }
    }
    for (auto [i,j] : observation_set) {
        std::cerr << "(" << i << "," << j << ")" << " ";
    }
    std::cerr << std::endl;
    std::cerr << "score: " << max_score << std::endl;
    return observation_set;
}

void update_probabilities(Pool& pool, const std::vector<Point>& set, value_t v) {
    double sum = 0;
    for (auto& hypo : pool) {
        double likelihood = Observation::likelihood(set, hypo, v);
        hypo.prob *= likelihood;
        sum += hypo.prob;
    }
    // normalize
    for (auto& hypo : pool) {
        hypo.prob = hypo.prob / sum;
    }
    std::sort(pool.begin(), pool.end(),
              [](const auto& lhs, const auto& rhs) { return lhs.prob > rhs.prob; });
}

struct Solver {
    void solve() {
        constexpr double threshold = 0.95;
        auto pool = enumerate_all_arrangement();
        while (true) {
            auto set = make_observation_set(pool);
            value_t v = client.predict(set);
            update_probabilities(pool, set, v);
            double prob = pool[0].prob;
            std::cerr << "best prob: " << prob << std::endl;
            if (prob > threshold) {
                std::cerr << "Submitting an answer... ";
                Board b = pool[0].to_board();
                bool success = client.answer(b.make_answer());
                if (success) {
                    std::cerr << "success!" << std::endl;
                    return;
                }
                else {
                    std::cerr << "failed" << std::endl;
                }
            }
        }
    }
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
    predict_model.init();
}

}  // namespace ahc

int main() {
    using ahc::problem;
    ahc::init_problem();
    ahc::Solver solver;
    solver.solve();
}
