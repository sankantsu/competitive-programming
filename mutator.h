#pragma once
#include <iostream>
#include <vector>
#include <algorithm>
#include <random>
#include <deque>
#include <cassert>
#include "generator.h"
#include "board.h"

struct Mutator {
    Mutator(int n_, const Board &board_, const std::vector<int>& target_count_)
        : n(n_),
          board(board_),
          target_count(target_count_)
    {}
    void mutate();
    auto get_diff() {
        return diff_sum(board.count());
    }
    auto get_board() {
        return board;
    }
    private:
    int diff_sum(std::vector<int> count) {
        int diff = 0;
        for (std::size_t i = 0; i < count.size(); i++) {
            diff += abs(count[i]-target_count[i]);
        }
        return diff;
    }
    int n;
    Board board;
    std::vector<int> target_count;
};

void Mutator::mutate() {
    std::random_device rd;
    std::mt19937 mt{rd()};
    auto gen_random_sample = [&mt,this]() {
        int i = mt()%n;
        int j = mt()%n;
        int d = mt()%2;
        return std::make_tuple(i,j,d);
    };
    auto check_sample = [this](int i, int j, int d) {
        if (d == 0) {
            if (i == n-1 || (i == n-2 && j == n-1)) {
                return false;
            }
            else if ((board.get_tile(i,j)&8) || (board.get_tile(i+1,j)&2)) {
                return false;
            }
            else {
                return true;
            }
        }
        else if (d == 1) {
            if (j == n-1 || (i == n-1 && j == n-2)) {
                return false;
            }
            else if ((board.get_tile(i,j)&4) || (board.get_tile(i,j+1)&1)) {
                return false;
            }
            else {
                return true;
            }
        }
        assert(false);
    };
    auto [i,j,dir] = gen_random_sample();
    while (!check_sample(i,j,dir)) {
        std::tie(i,j,dir) = gen_random_sample();
    }
    int diff = diff_sum(board.count());
    Board bd = board;
    bd.add_edge(i,j,dir);
    auto find_path = [this](int si, int sj, int gi, int gj) {
        using pos = std::pair<int,int>;
        std::vector<std::vector<pos>> prev(n,std::vector<pos>(n));
        std::queue<pos> q;
        q.emplace(si,sj);
        prev[si][sj] = std::make_pair(-1,-1);
        while(!q.empty()) {
            auto [i,j] = q.front(); q.pop();
            int t = board.get_tile(i,j);
            if (i == gi && j == gj) break;
            for (int dir = 0; dir < 4; dir++) {
                if ((t>>dir)&1) {
                    int ni = i + Board::dx[dir];
                    int nj = j + Board::dy[dir];
                    if (std::make_pair(ni,nj) == prev[i][j]) continue;
                    if (!board.check_border(ni,nj)) continue;
                    prev[ni][nj] = std::make_pair(i,j);
                    q.emplace(ni,nj);
                }
            }
        }
        std::deque<pos> path;
        path.emplace_front(gi,gj);
        while(true) {
            auto [i,j] = path[0];
            auto [pi,pj] = prev[i][j];
            path.emplace_front(pi,pj);
            if (pi == si && pj == sj) break;
        }
        return path;
    };
    int i2 = (dir == 0) ? i+1 : i;
    int j2 = (dir == 0) ? j : j+1;
    auto path = find_path(i,j,i2,j2);
    for (std::size_t k = 0; k < path.size()-1; k++) {
        auto [i,j] = path[k];
        auto [i2,j2] = path[k+1];
        int dir = ((j2 - j) == 0) ? 0 : 1;
        i = std::min(i,i2); j = std::min(j,j2);
        bd.del_edge(i,j,dir);
        if (diff_sum(bd.count()) < diff) {
            board = std::move(bd);
            return;
        }
        else {
            bd.add_edge(i,j,dir); // backtrack
        }
    }
}
