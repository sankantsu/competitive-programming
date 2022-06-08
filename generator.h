#pragma once
#include <iostream>
#include <vector>
#include <algorithm>
#include <random>
#include "util.h"

struct Generator {
    struct edge {
        int i;
        int j;
        int dir;
    };
    Generator(int n_) : n(n_) {}
    void gen();
    void print_board () const;
    void shuffle_board();
    auto get_board() const {
        return board;
    }
    private:
    void make_edges();
    void make_tree();
    void gen_board();
    int n;
    std::vector<edge> edges;
    std::vector<edge> tree;
    std::vector<std::vector<int>> board;
};

void Generator::gen() {
    std::random_device rd;
    std::mt19937 mt{rd()};

    make_edges();
    std::shuffle(edges.begin(),edges.end(),mt);
    make_tree();
    gen_board();
}

void Generator::print_board() const {
    if (board.empty()) throw;
    int t = 2*n*n*n;
    std::cout << n << " " << t << std::endl;
    for (auto row : board) {
        for (auto x : row) {
            std::cout << std::hex << x;
        }
        std::cout << std::dec << std::endl;
    }
}

void Generator::make_edges() {
    std::vector<edge> v;
    for (int i = 0; i < n-1; i++) for (int j = 0; j < n; j++) {
        if (i == n-2 && j == n-1) break;
        v.push_back(edge{i,j,0});
    }
    for (int i = 0; i < n; i++) for (int j = 0; j < n-1; j++) {
        if (i == n-1 && j == n-2) break;
        v.push_back(edge{i,j,1});
    }
    edges = std::move(v);
}

void Generator::make_tree() {
    uftree uf(n*n-1);
    std::vector<edge> v;
    for (auto e : edges) {
        auto [i,j,dir] = e;
        int k = n*i+j;
        int l = (dir == 0) ? n*(i+1)+j : n*i+(j+1);
        if (uf.same(k,l)) continue;
        uf.unite(k,l);
        v.push_back(e);
    }
    tree = std::move(v);
}

void Generator::gen_board() {
    std::vector<std::vector<int>> bd(n,std::vector<int>(n,0));
    for (auto e : tree) {
        auto [i,j,dir] = e;
        if (dir == 0) {
            bd[i][j] |= 8;
            bd[i+1][j] |= 2;
        }
        else {
            bd[i][j] |= 4;
            bd[i][j+1] |= 1;
        }
    }
    board = std::move(bd);
}

void Generator::shuffle_board() {
    constexpr int dx[4] = {0,-1,0,1};
    constexpr int dy[4] = {-1,0,1,0};
    std::random_device rd;
    std::mt19937 mt{rd()};
    int t = 2*n*n*n;
    int i = n-1, j = n-1;
    int pi = -1, pj = -1;
    std::string ins;
    for (int k = 0; k < t; k++) {
        while (true) {
            int dir = mt()%4;
            int ni = i + dx[dir];
            int nj = j + dy[dir];
            if (ni == pi && nj == pj) continue;
            if (ni < 0 || n <= ni || nj < 0 || n <= nj) continue;
            std::swap(board[i][j],board[ni][nj]);
            pi = i; pj = j;
            i = ni; j = nj;
            ins.push_back(dir2char(dir));
            break;
        }
    }
    std::reverse(ins.begin(),ins.end());
    for (auto &c : ins) {
        c = reverse_dir(c);
    }
    /* std::cerr << ins << std::endl; */
}
