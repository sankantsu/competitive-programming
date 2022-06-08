#pragma once
#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <string>
#include <queue>
#include <stack>
#include <cassert>
#include <cmath>
#include <boost/container_hash/hash.hpp>
#include "util.h"

struct Board {
    static constexpr int dx[4] = {0,-1,0,1};
    static constexpr int dy[4] = {-1,0,1,0};
    Board(int n_, std::vector<std::vector<int>> &&tiles_)
        : n(n_),
          tiles(std::move(tiles_)),
          tiles_id(n,std::vector<int>(n,-1))
    {
        t = 2*n*n*n;
        for (int i = 0; i < n; i++) for (int j = 0; j < n; j++) {
            if (get_tile(i,j) == 0) {
                blank_pos = std::make_pair(i,j);
            }
        }
        manhattan_distance = 0;
    }
    Board(const Board&) = default;
    Board& operator=(const Board&) = default;
    void print() const {
        std::cout << std::dec << n << " " << t << std::endl;
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) std::cout << std::hex << get_tile(i,j);
            std::cout << std::endl;
        }
    }
    void print_id() const {
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) std::cerr << std::dec << std::setw(2) << tiles_id[i][j] << " ";
            std::cerr << std::endl;
        }
    }
    auto count() const {
        std::vector<int> cnt(16);
        for (int i = 0; i < n; i++) for (int j = 0; j < n; j++) cnt[get_tile(i,j)]++;
        return cnt;
    }
    bool check_border(int i, int j) const {
        return 0 <= i && i < n && 0 <= j && j < n;
    }
    auto get_instructions() const{
        return instructions;
    }
    void resize_instructions() {
        if (static_cast<int>(instructions.size()) > t) {
            std::cerr << "resized" << std::endl;
            instructions.resize(t);
        }
    }
    void clear_instructions() {
        instructions.clear();
    }
    void eliminate_wasted_instructions();
    void move_once(int dir);
    void execute_instructions(std::string ins) {
        for (auto c : ins) {
            int dir = char2dir(c);
            move_once(dir);
        }
    }
    auto max_tree_size() const;
    auto eval() const;
    int get_board_size() const {
        return n;
    }
    int get_tile(int i, int j) const {
        return tiles[i][j];
    }
    int get_tile_id(int i, int j) const {
        return tiles_id[i][j];
    }
    void set_tile_id(int i, int j, int id) {
        tiles_id[i][j] = id;
    }
    void reset_tile_id(int i, int j) {
        set_tile_id(i,j,-1);
    }
    void swap_tile(int i, int j, int i2, int j2) {
        std::swap(tiles[i][j],tiles[i2][j2]);
        std::swap(tiles_id[i][j],tiles_id[i2][j2]);
    }
    void add_edge(int i, int j, int dir) {
        int i2 = (dir == 0) ? i+1 : i;
        int j2 = (dir == 0) ? j : j+1;
        if (dir == 0) {
            tiles[i][j]   |= 8;
            tiles[i2][j2] |= 2;
        }
        else if (dir == 1) {
            tiles[i][j]   |= 4;
            tiles[i2][j2] |= 1;
        }
    }
    void del_edge(int i, int j, int dir) {
        int i2 = (dir == 0) ? i+1 : i;
        int j2 = (dir == 0) ? j : j+1;
        if (dir == 0) {
            tiles[i][j]   &= ~8;
            tiles[i2][j2] &= ~2;
        }
        else if (dir == 1) {
            tiles[i][j]   &= ~4;
            tiles[i2][j2] &= ~1;
        }
    }
    auto get_blank_pos() const {
        return blank_pos;
    }
    auto get_prev_blank_pos() const;
    int get_manhattan_distance() const {
        return manhattan_distance;
    }
    void calc_manhattan_distance();
    auto search_piece(int id) const;
    int inversion_number();
    bool is_solvable();
    friend std::size_t hash_value(const Board&);
    friend bool operator==(const Board&, const Board&);
    private:
    int tile_distance(int i, int j, int id);
    int n;
    int t;
    int manhattan_distance;
    std::pair<int,int> blank_pos;
    std::string instructions;
    std::vector<std::vector<int>> tiles;
    std::vector<std::vector<int>> tiles_id;
};

std::size_t hash_value(const Board& bd) {
    std::size_t seed = 0;
    for (auto row : bd.tiles) for (auto id : row) {
        boost::hash_combine(seed,id);
    }
    return seed;
}

bool operator==(const Board& lhs, const Board& rhs) {
    return lhs.tiles == rhs.tiles;
}

void Board::eliminate_wasted_instructions() {
    std::stack<char> st;
    for (char c : instructions) {
        int numc = char2dir(c);
        if (st.empty()) {
            st.push(c);
        }
        else {
            int numt = char2dir(st.top());
            if (abs(numc - numt) == 2) {
                st.pop();
            }
            else {
                st.push(c);
            }
        }
    }
    instructions.clear();
    while (!st.empty()) {
        instructions.push_back(st.top());
        st.pop();
    }
    std::reverse(instructions.begin(),instructions.end());
}

auto Board::get_prev_blank_pos() const {
    if (instructions.empty()) throw;
    auto [bi,bj] = blank_pos;
    char c = *instructions.crbegin();
    int dir = char2dir(c);
    dir = (dir+2)%4;
    int pi = bi + dx[dir];
    int pj = bj + dy[dir];
    return std::make_pair(pi,pj);
}

int Board::tile_distance(int i, int j, int id) {
    int id_x = id/n;
    int id_y = id - n*id_x;
    int dist = abs(i-id_x) + abs(j-id_y);
    return dist;
}

int Board::inversion_number() {
    BIT bit(n*n);
    int inv_n = 0;
    std::vector<int> v;
    for (int i = 0; i < n; i++) for (int j = 0; j < n; j++) {
        int id = tiles_id[i][j]+1;
        if (id != 0) {
            v.push_back(id);
        }
    }
    for (std::size_t i = 0; i < v.size(); i++) {
        int id = v[i];
        inv_n += i - bit.sum(id);
        bit.add(id,1);
    }
    return inv_n;
}

bool Board::is_solvable() {
    int inv_n = inversion_number();
    int parity;
    if (n%2 == 0) {
        auto [i,j] = blank_pos;
        parity = (inv_n+i)%2;
    }
    else {
        parity = (inv_n+1)%2;
    }
    return static_cast<bool>(parity);
}

void Board::calc_manhattan_distance() {
    manhattan_distance = 0;
    for (int i = 0; i < n; i++) for (int j = 0; j < n; j++) {
        int id = tiles_id[i][j];
        if (id != -1) {
            manhattan_distance += tile_distance(i,j,id);
        }
    }
}

auto Board::search_piece(int id) const {
    for (int i = 0; i < n; i++) for (int j = 0; j < n; j++) {
        if (id == tiles_id[i][j]) {
            return std::make_pair(i,j);
        }
    }
    std::cerr << "(search_piece) id : " << id << std::endl;
    print_id();
    assert(false);
}

void Board::move_once(int dir) {
    auto [i,j] = blank_pos;
    int ni = i + dx[dir];
    int nj = j + dy[dir];
    if (ni < 0 || n <= ni || nj < 0 || n <= nj) throw;
    int id = tiles_id[ni][nj];
    if (id != -1) {
        int delta_d = tile_distance(i,j,id) - tile_distance(ni,nj,id);
        manhattan_distance += delta_d;
    }
    swap_tile(i,j,ni,nj);
    blank_pos = std::make_pair(ni,nj);
    instructions.push_back(dir2char(dir));
}

auto Board::max_tree_size() const {
    std::vector<std::vector<bool>> visited(n,std::vector<bool>(n,false));
    int max_size = -1;
    for (int i = 0; i < n; i++) for (int j = 0; j < n; j++) {
        if (visited[i][j]) continue;
        auto bfs = [&visited,this](int i, int j) {
            using P = std::tuple<int,int,int,int>;
            std::queue<P> q;
            q.emplace(i,j,-1,-1);
            int s = 0;
            while(!q.empty()) {
                auto [i,j,pi,pj] = q.front(); q.pop();
                if (visited[i][j]) {
                    return -1;
                }
                visited[i][j] = true; s++;
                int tile = get_tile(i,j);
                for (int dir = 0; dir < 4; dir++) {
                    if ((tile>>dir)&1) {
                        int ni = i + dx[dir];
                        int nj = j + dy[dir];
                        if (ni == pi && nj == pj) continue;
                        if (!check_border(ni,nj)) continue;
                        int ndir = (dir+2)%4;
                        int ntile = get_tile(ni,nj);
                        if ((ntile>>ndir)&1) {
                            q.emplace(ni,nj,i,j);
                        }
                    }
                }
            }
            return s;
        };
        int s = bfs(i,j);
        max_size = std::max(max_size,s);
    }
    std::cerr << "max_size: " << max_size << std::endl;
    return max_size;
}

auto Board::eval() const {
    const int base_score = 500000;
    int sz = max_tree_size();
    double score;
    if (sz == n*n-1) {
        int n_move = instructions.size();
        assert(n_move <= t);
        score = base_score*(2-static_cast<double>(n_move)/t);
    }
    else {
        score = base_score*(static_cast<double>(sz)/(n*n-1));
    }
    return static_cast<int>(std::round(score));
}
