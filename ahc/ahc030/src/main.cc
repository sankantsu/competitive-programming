#include <cstdio>
#include <iostream>
#include <vector>
#include <string>

namespace ahc {

using oil_reserve_t = int;

namespace oil_reservation {
    constexpr oil_reserve_t undef = -1;
}

struct Point {
    int i;
    int j;
};

struct Polyomino {
    Polyomino(int area, std::vector<Point> relative_positions)
        : _area(area), _relative_positions(relative_positions)
    {}
    private:
    int _area;
    std::vector<Point> _relative_positions;
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

struct Board {
    Board(int board_size, int num_oilfield, double error_param, std::vector<Polyomino> oil_fields)
        : _board_size(board_size), _num_oilfield(num_oilfield), _error_param(error_param),
          _oil_fields(oil_fields)
    { }
    int get_board_size() const { return _board_size; }
    int get_num_oilfield() const { return _num_oilfield; }
    private:
    int _board_size;
    int _num_oilfield;
    double _error_param;
    std::vector<Polyomino> _oil_fields;
};

struct Solver {
    Solver(const Board& board)
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
    Board _board;
    Client _client;
    std::vector<std::vector<oil_reserve_t>> _oil_reservations;
};

Board parse_input() {
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
    }

    Board board {board_size, num_oilfield, error_param, oil_fields};
    return board;
}

}

int main() {
    ahc::Board board = ahc::parse_input();
    ahc::Solver solver(board);
    solver.solve();
}
