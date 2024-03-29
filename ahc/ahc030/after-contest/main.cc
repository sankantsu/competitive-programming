#include <cstdio>
#include <cmath>
#include <cassert>
#include <type_traits>
#include <ranges>
#include <iostream>
#include <numeric>
#include <algorithm>
#include <vector>
#include <unordered_set>
#include <string>
#include <random>

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

static std::mt19937 _mt(0);
uint32_t gen_random() {
    return _mt();
}

double gen_random_double() {
    static std::uniform_real_distribution dist(0.,1.);
    return dist(_mt);
}

namespace oil_reservation {
    constexpr value_t undef = -1;
}

struct Point {
    int i;
    int j;
};
Point add_point(Point x, Point y) {
    return Point{x.i + y.i, x.j + y.j};
}

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

        _history.push_back(HistoryEntry{std::forward<Range>(set), v});
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
    const auto& get_history() { return _history; }
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
    struct HistoryEntry {
        std::vector<Point> set;
        value_t v;
    };
    std::vector<known_point> _known_points;
    std::vector<HistoryEntry> _history;
    Board _cache;
};
static Client client;

struct PredictModel {
    void init() {
        _error_param = problem.get_error_param();
        auto n = problem.get_board_size();
        _init_table();
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
    auto pred_range(int area, int v) {
        int min_pred = _min_pred[area][v];
        int max_pred = min_pred + _likelihoods[area][v].size() - 1;
        return std::make_pair(min_pred, max_pred);
    }
    double likelihood(int area, int v, int x) {
        auto [min, max] = pred_range(area, v);
        assert(min <= x && x <= max);
        return _likelihoods[area][v][x - min].first;
    }
    double log_likelihood(int area, int v, int x) {
        auto [min, max] = pred_range(area, v);
        assert(min <= x && x <= max);
        return _likelihoods[area][v][x - min].second;
    }
    // https://cpprefjp.github.io/reference/cmath/erf.html
    static double cdf_normal(double mu, double sigma, double x) {
        constexpr double sqrt_2 = 1.4142135623730951;
        double x_normalized = (x - mu) / (sqrt_2 * sigma);
        return 0.5 * (1 + std::erf(x_normalized));
    }
    private:
    static constexpr double _cache_empty = -1.;
    void _init_table() {
        int n = problem.get_board_size();
        int total_rsv = problem.get_total_reservation();

        // init shape of _min_pred
        _min_pred.resize(n*n+1);
        for (auto& v : _min_pred) {
            v.resize(total_rsv+1);
        }

        // init _likelihoods
        for (int area = 0; area <= n*n; area++) {
            std::vector<std::vector<prob_pair>> vs;
            for (int v = 0; v <= total_rsv; v++) {
                std::vector<prob_pair> vec;
                int mu = std::round(mean(area, v));
                constexpr double eps = 1e-6;
                // init likelihoodds lower than mean
                for (int pred = mu; pred >= 0; pred--) {
                    double lh = _calc_likelihood(area, v, pred);
                    if (lh < eps) {
                        _min_pred[area][v] = pred + 1;
                        break;
                    }
                    vec.push_back(std::make_pair(lh, std::log(lh)));
                }
                std::reverse(vec.begin(), vec.end());
                // init likelihoodds higher than mean
                for (int pred = mu+1; ; pred++) {
                    double lh = _calc_likelihood(area, v, pred);
                    if (lh < eps) {
                        break;
                    }
                    vec.push_back(std::make_pair(lh, std::log(lh)));
                }
                vs.push_back(std::move(vec));
            }
            _likelihoods.push_back(std::move(vs));
        }
    }
    // Given area size and hypothetical reservation amount,
    // calculates the likelihood that observation yields a value x
    double _calc_likelihood(int area, int hypo, int x) {
        double mu = mean(area, hypo);
        double sigma = std::sqrt(variance(area));
        auto cdf = [mu, sigma](double x) {
            return cdf_normal(mu, sigma, x);
        };
        if (x == 0) {
            return cdf(0.5);
        }
        else {
            return cdf(x + 0.5) - cdf(x - 0.5);
        }
    }
    double _error_param;
    using prob_pair = std::pair<double, double>;  // raw prob and log prob
    std::vector<std::vector<std::vector<prob_pair>>> _likelihoods;  // area, actual value, pred value -> prob
    std::vector<std::vector<int>> _min_pred;
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
        auto [min, max] = predict_model.pred_range(point_set.size(), hypo_reservation);
        if (x < min || max < x) {
            return 0.;
        }
        return predict_model.likelihood(point_set.size(), hypo_reservation, x);
    }
    static double mutual_information(const std::vector<Point>& point_set, const Pool& pool, const std::vector<value_t>& count) {
        const int area = point_set.size();
        // calculates marginal distribution of observation value y
        std::vector<double> prob_y(problem.get_total_reservation()*2);
        for (std::size_t k = 0; k < pool.size(); k++) {
            const auto& hypo = pool[k];
            auto [min_y, max_y] = predict_model.pred_range(area, count[k]);
            for (value_t y = min_y; y <= max_y; y++) {
                double lh = predict_model.likelihood(point_set.size(), count[k], y);
                prob_y[y] += hypo.prob * lh;
            }
        }
        std::vector<double> prob_y_ln;
        for (auto prob : prob_y) {
            prob_y_ln.push_back(std::log(prob));
        }
        // calculates mutual information
        double mi = 0;
        for (std::size_t k = 0; k < pool.size(); k++) {
            const auto& hypo = pool[k];
            auto [min_y, max_y] = predict_model.pred_range(area, count[k]);
            for (value_t y = min_y; y <= max_y; y++) {
                double lh = predict_model.likelihood(point_set.size(), count[k], y);
                double lh_ln = predict_model.log_likelihood(point_set.size(), count[k], y);
                double delta = hypo.prob * lh * (lh_ln - prob_y_ln[y]);
                if (!std::isinf(delta) && !std::isnan(delta)) {
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

Pool generate_random_arrangement() {
    constexpr std::size_t n_sample = 1000;
    const int n = problem.get_board_size();
    const double prob = 1. / n_sample;
    Pool pool;
    for (int l = 0; l < n_sample; l++) {
        std::vector<Point> offsets;
        for (int k = 0; k < problem.get_polyominos().size(); k++) {
            auto [size_i, size_j] = problem.get_polyominos()[k].bbox_size();
            int i = gen_random() % (n - size_i);
            int j = gen_random() % (n - size_j);
            offsets.push_back(Point{i,j});
        }
        Hypothesis h {prob, std::move(offsets)};
        pool.push_back(std::move(h));
    }
    return pool;
}

struct State {
    double ln_prob;
    std::vector<Point> offsets;
    Board board;
    void move(int k, Point to) {
        auto [i,j] = offsets[k];
        auto [ni,nj] = to;
        for (auto [rel_i,rel_j] : problem.get_polyominos()[k].get_relative_positions()) {
            board[i + rel_i][j + rel_j]--;
            board[ni + rel_i][nj + rel_j]++;
        }
        offsets[k] = to;
        ln_prob = log_likelihood(board);
    }
    bool swap(int k, int l) {
        auto p = offsets[k];
        auto q = offsets[l];
        auto p2 = add_point(q, _swap_table[l][k]);
        auto q2 = add_point(p, _swap_table[k][l]);
        bool check = boundary_check(k, p2) && boundary_check(l, q2);
        if (!check) {
            return false;
        }
        move(k, p2);
        move(l, q2);
        return true;
    }
    std::size_t hash() {
        int n = problem.get_board_size();
        std::size_t v = 0;
        for (std::size_t k = 0; k < offsets.size(); k++) {
            auto [i,j] = offsets[k];
            v ^= _hashes[k][n*i + j];
        }
        return v;
    }
    static bool boundary_check(int k, Point p) {
        int n = problem.get_board_size();
        auto [size_i, size_j] = problem.get_polyominos()[k].bbox_size();
        auto [ni,nj] = p;
        if (ni < 0 || (n - size_i) < ni || nj < 0 || (n - size_j) < nj) {
            return false;
        }
        return true;
    }
    static State from_hypothesis(const auto& h) {
        State s;
        s.offsets = h.offsets;
        s.board = h.to_board();
        s.ln_prob = log_likelihood(s.board);
        return s;
    }
    static double log_likelihood(const Board& b) {
        double ln_prob = 0;
        for (const auto& [set,y] : client.get_history()) {
            value_t v = 0;
            for (auto [i,j] : set) {
                v += b[i][j];
            }
            auto [min_y,max_y] = predict_model.pred_range(set.size(), v); 
            if (y < min_y || max_y < y) {
                return -std::numeric_limits<double>::infinity();
            }
            double ln_lh = predict_model.log_likelihood(set.size(), v, y);
            ln_prob += ln_lh;
        }
        return ln_prob;
    }
    static void init_hash() {
        int n = problem.get_board_size();
        int m = problem.get_num_oilfield();
        for (std::size_t k = 0; k < m; k++) {
            std::vector<std::size_t> vec;
            for (std::size_t ij = 0; ij < n*n; ij++) {
                std::size_t u1 = gen_random();
                std::size_t u2 = gen_random();
                vec.push_back((u1 << 32) | u2);
            }
            _hashes.push_back(std::move(vec));
        }
    }
    static void init_swap() {
        int m = problem.get_num_oilfield();
        auto count_common = [](size_t k, size_t l, int i, int j) {
            const auto& poly_k = problem.get_polyominos()[k];
            const auto& poly_l = problem.get_polyominos()[l];
            int cnt = 0;
            for (auto p : poly_k.get_relative_positions()) {
                for (auto q : poly_l.get_relative_positions()) {
                    int ni = q.i + i;
                    int nj = q.j + j;
                    if (p.i == ni && p.j == nj) {
                        cnt++;
                    }
                }
            }
            return cnt;
        };
        _swap_table = decltype(_swap_table)(m, std::vector<Point>(m));
        for (std::size_t k = 0; k < m; k++) {
            for (std::size_t l = 0; l < m; l++) {
                auto size_k = problem.get_polyominos()[k].bbox_size();
                auto size_l = problem.get_polyominos()[l].bbox_size();
                using entry = std::pair<Point, int>;
                std::vector<entry> counts;
                for (int i = -size_l.first; i < size_k.first; i++) {
                    for (int j = -size_l.second; j < size_k.second; j++) {
                        counts.emplace_back(Point{i,j}, count_common(k,l,i,j));
                    }
                }
                std::sort(counts.begin(), counts.end(),
                          [](const entry& lhs, const entry& rhs) { return lhs.second > rhs.second; });
                _swap_table[k][l] = counts[0].first;
                auto [i,j] = _swap_table[k][l];
                /* std::cerr << "swap_table[" << k << "][" << l << "] = (" << i << "," << j << ")" << std::endl; */
            }
        }
    }
    private:
    static inline std::vector<std::vector<std::size_t>> _hashes;
    static inline std::vector<std::vector<Point>> _swap_table;
};

Pool annealing(Hypothesis h) {
    const int n = problem.get_board_size();
    const int m = problem.get_num_oilfield();
    State s = State::from_hypothesis(h);
    std::vector<State> states;
    std::unordered_set<std::size_t> saved;
    constexpr std::size_t n_iteration = 20000;
    double start_temp = 10;
    double end_temp = 0;
    auto try_move = [&s, &states, &saved](int k, Point to, double temp) {
        double ln_prob = s.ln_prob;
        Point org = s.offsets[k];
        s.move(k, to);  // move to destination
        if (std::isinf(s.ln_prob)) {  // restore
            s.move(k, org);
            return;
        }
        if (!saved.contains(s.hash())) {  // try inserting new state
            states.push_back(s);
            saved.insert(s.hash());
        }
        double accept_prob = std::exp((s.ln_prob - ln_prob) / temp);
        double r = gen_random_double();
        if (r > accept_prob) {  // restore
            s.move(k, org);
        }
    };
    auto try_swap = [&s, &states, &saved](int k, int l, double temp) {
        auto ans = s.board.make_answer();
        Point org_k = s.offsets[k];
        Point org_l = s.offsets[l];
        auto restore = [&s, k, l, org_k, org_l]{
            s.move(k, org_k);
            s.move(l, org_l);
        };
        double ln_prob = s.ln_prob;
        bool success = s.swap(k, l);  // move to destination
        if (!success) {
            return;
        }
        if (std::isinf(s.ln_prob)) {  // restore
            restore();
            return;
        }
        if (!saved.contains(s.hash())) {  // try inserting new state
            states.push_back(s);
            saved.insert(s.hash());
        }
        double accept_prob = std::exp((s.ln_prob - ln_prob) / temp);
        double r = gen_random_double();
        if (r > accept_prob) {  // restore
            restore();
        }
    };
    constexpr double prob_move_once = 0.6;
    constexpr double prob_move_random = 0.2;
    constexpr double prob_swap = 1 - prob_move_once - prob_move_random;
    for (std::size_t iteration = 0; iteration < n_iteration; iteration++) {
        double temp = start_temp + (end_temp - start_temp) * iteration / n_iteration;
        double r = gen_random_double();
        // move one square
        if (r < prob_move_once) {
            constexpr int delta[4][2] = {{1,0}, {0,-1}, {-1,0}, {0,1}};
            int k = gen_random() % m;
            int dir = gen_random() % 4;
            auto [i,j] = s.offsets[k];
            auto [di,dj] = delta[dir];
            auto [size_i,size_j] = problem.get_polyominos()[k].bbox_size();
            int ni = i + di;
            int nj = j + dj;
            if (ni < 0 || (n - size_i) < ni || nj < 0 || (n - size_j) < nj) {
                continue;
            }
            try_move(k, Point{ni,nj}, temp);
        }
        // move random
        else if (r < prob_move_once + prob_move_random) {
            int k = gen_random() % m;
            auto [size_i,size_j] = problem.get_polyominos()[k].bbox_size();
            int ni = gen_random() % (n - size_i);
            int nj = gen_random() % (n - size_j);
            try_move(k, Point{ni,nj}, temp);
        }
        // swap two
        else {
            int k = gen_random() % m;
            int l = gen_random() % m;
            if (k == l) continue;
            try_swap(k, l, temp);
        }
    }
    std::sort(states.begin(), states.end(),
              [](const State& lhs, const State& rhs) {
                return lhs.ln_prob > rhs.ln_prob;
              });
    const std::size_t n_sample = std::min(1000ul, states.size());
    states.resize(n_sample);
    double prob_sum = 0;
    for (const auto& s : states) {
        prob_sum += std::exp(s.ln_prob);
    }
    Pool pool;
    for (auto& s : states) {
        Hypothesis h;
        h.prob = std::exp(s.ln_prob) / prob_sum;
        h.offsets = std::move(s.offsets);
        pool.push_back(std::move(h));
    }
    return pool;
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
    std::cerr << "score: " << max_score << std::endl;
    return observation_set;
}

bool update_probabilities(Pool& pool, const std::vector<Point>& set, value_t v) {
    std::cerr << "update_probabilities()" << std::endl;
    double sum = 0;
    for (auto& hypo : pool) {
        double likelihood = Observation::likelihood(set, hypo, v);
        hypo.prob *= likelihood;
        sum += hypo.prob;
    }
    if (sum == 0.) {
        return false;
    }
    std::cerr << "sum: " << sum << std::endl;
    // normalize
    for (auto& hypo : pool) {
        hypo.prob = hypo.prob / sum;
    }
    std::sort(pool.begin(), pool.end(),
              [](const auto& lhs, const auto& rhs) { return lhs.prob > rhs.prob; });
    return true;
}

struct Solver {
    void solve_m_2() {
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
    void solve_large() {
        int fail_cnt = 0;
        constexpr double threshold = 0.95;
        auto pool = generate_random_arrangement();
        constexpr std::size_t max_iter = 1000;
        for (std::size_t iter = 0; iter < max_iter; iter++) {
            if (iter != 0) {
                pool = annealing(pool[0]);
            }
            auto set = make_observation_set(pool);
            value_t v = client.predict(set);
            bool check = update_probabilities(pool, set, v);
            double prob = pool[0].prob;
            std::cerr << "best prob: " << prob << std::endl;
            for (auto [i,j] : pool[0].offsets) {
                std::cerr << i << "," << j << " ";
            }
            std::cerr << std::endl;
            if (prob > threshold) {
                std::cerr << "Submitting an answer... ";
                Board b = pool[0].to_board();
                bool success = client.answer(b.make_answer());
                if (success) {
                    std::cerr << "success!" << std::endl;
                    return;
                }
                else {
                    fail_cnt++;
                    std::cerr << "failed" << std::endl;
                    if (fail_cnt >= 5) {
                        return;
                    }
                }
            }
        }
    }
    void solve() {
        int m = problem.get_num_oilfield();
        if (false) {
            solve_m_2();
        }
        else {
            solve_large();
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
    State::init_hash();
    State::init_swap();
}

}  // namespace ahc

int main() {
    using ahc::problem;
    ahc::init_problem();
    ahc::Solver solver;
    solver.solve();
}
