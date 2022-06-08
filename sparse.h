#pragma once
#include <iostream>
#include <iomanip>
#include <utility>
#include <stdexcept>
#include <chrono>
#include <random>
#include <vector>
#include <queue>
#include <unordered_set>
#include <algorithm>
#include <cassert>
#include <boost/container_hash/hash.hpp>
#include "util.h"

struct SparseBoard {
    using Index = int8_t;
    using Position = std::pair<Index,Index>;
    using Container = std::vector<Position>;
    using Direction = int8_t;
    using Instruction = std::string;
    using Pred = std::function<bool(Position)>;

    static constexpr Position position_diff(Direction dir) {
        constexpr Index dx[4] = {0,-1,0,1};
        constexpr Index dy[4] = {-1,0,1,0};
        return std::make_pair(dx[dir],dy[dir]);
    }

    SparseBoard(Index n_, Position blank_pos_, const Container& cur_, const Container& goal_,
            Pred is_fixed_ = [](Position) { return false; })
        : n(n_),
          n_tile(cur_.size()),
          blank_pos(blank_pos_),
          cur(cur_),
          goal(goal_),
          is_fixed(is_fixed_)
    {
        assert(cur.size() == goal.size());
    }
    SparseBoard(const SparseBoard&) = default;
    SparseBoard& operator=(const SparseBoard&) = default;

    Index board_size() const {
        return n;
    }
    std::size_t num_tile() const {
        return n_tile;
    }
    Position get_blank_pos() const {
        return blank_pos;
    }
    Instruction get_instruction() const {
        return ins;
    }

    void print() const;
    bool check_border(Position pos) const {
        auto [i,j] = pos;
        return 0 <= i && i < n && 0 <= j && j < n && !is_fixed(pos);
    }
    bool is_solved() const;
    auto neighbor_directions(Index i, Index j) const;
    void move(Direction dir);
    int heuristic_distance() const;
    friend bool operator==(const SparseBoard& lhs, const SparseBoard& rhs);
    friend std::size_t hash_value(const SparseBoard& bd);
    private:
    Index n;
    std::size_t n_tile;
    Position blank_pos;
    Container cur;
    Container goal;
    Pred is_fixed;
    Instruction ins;
};

struct SparseSolver {
    SparseSolver(const SparseBoard& bd) 
        : current_board(bd)
    {}
    auto solve();
    private:
    auto A_star();
    SparseBoard current_board;
};

bool operator==(const SparseBoard& lhs, const SparseBoard& rhs) {
    if (lhs.get_blank_pos() != rhs.get_blank_pos()) {
        return false;
    }
    if (lhs.cur != rhs.cur) {
        return false;
    }
    /* if (lhs.goal != rhs.goal) { */
    /*     return false; */
    /* } */
    return true;
}

std::size_t hash_value(const SparseBoard& bd) {
    std::size_t seed = 0;
    boost::hash_combine(seed,bd.get_blank_pos());
    for (auto pos : bd.cur) {
        boost::hash_combine(seed,pos);
    }
    for (auto gpos : bd.goal) {
        boost::hash_combine(seed,gpos);
    }
    return seed;
}

void SparseBoard::print() const {
    std::cerr << "(print)" << std::endl;
    int wd = 3;
    for (Index i = 0; i < n; i++) {
        for (Index j = 0; j < n; j++) {
            Position pos = std::make_pair(i,j);
            if (is_fixed(pos)) {
                std::cerr << std::setw(wd) << "##";
            }
            else if (pos == get_blank_pos()) {
                std::cerr << std::setw(wd) << 0;
            }
            else {
                bool flag = false;
                for (std::size_t k = 0; k < num_tile(); k++) {
                    auto tile_pos = cur[k];
                    int id = k+1;
                    if (pos == tile_pos) {
                        flag = true;
                        std::cerr << std::setw(wd) << id;
                        break;
                    }
                }
                if (!flag) {
                    std::cerr << std::setw(wd) << "..";
                }
            }
        }
        std::cerr << std::endl;
    }
}

bool SparseBoard::is_solved() const {
    for (std::size_t k = 0; k < num_tile(); k++) {
        if (cur[k] != goal[k]) {
            return false;
        }
    }
    return true;
}

auto SparseBoard::neighbor_directions(Index i, Index j) const {
    std::vector<Direction> nb;
    for (Direction dir = 0; dir < 4; dir++) {
        auto [dx,dy] = position_diff(dir);
        Index ni = i + dx;
        Index nj = j + dy;
        Position npos{ni,nj};
        if (check_border(npos)) {
            nb.push_back(dir);
        }
    }
    return nb;
}

void SparseBoard::move(Direction dir) {
    auto [bi,bj] = get_blank_pos();
    auto [dx,dy] = position_diff(dir);
    Index ni = bi + dx;
    Index nj = bj + dy;
    Position npos{ni,nj};
    if (!check_border(npos)) {
        throw std::logic_error("tried to move out of board");
    }
    blank_pos = Position{ni,nj};
    for (auto& [i,j] : cur) {
        if (ni == i && nj == j) {
            i = bi;
            j = bj;
        }
    }
    ins.push_back(dir2char(dir));
}

int SparseBoard::heuristic_distance() const {
    // manhattan distance
    int md = 0;
    for (std::size_t k = 0; k < num_tile(); k++) {
        auto [i,j] = cur[k];
        auto [gi,gj] = goal[k];
        md += std::abs(i-gi) + std::abs(j-gj);
    }
    // max distance from current blank position to unsolved tile (call "tile distance")
    int td = 0;
    {
        auto [bi,bj] = get_blank_pos();
        for (std::size_t k = 0; k < num_tile(); k++) {
            auto [i,j] = cur[k];
            auto [gi,gj] = goal[k];
            if (i == gi && j == gj) continue;
            int d = std::abs(i-bi) + std::abs(j-bj);
            td = std::max(td,d);
        }
    }
    // linear conflict
    int ld = 0;
    {
        for (std::size_t k = 0; k < num_tile(); k++) {
            for (std::size_t l = k+1; l < num_tile(); l++) {
                auto [i,j] = cur[k];
                auto [i2,j2] = cur[l];
                auto [gi,gj] = goal[k];
                auto [gi2,gj2] = goal[l];
                if (i == i2 && gi == gi2 && i == gi && (j < j2) != (gj < gj2)) {
                    ld += 2;
                }
                if (j == j2 && gj == gj2 && j == gj && (i < i2) != (gi < gi2)) {
                    ld += 2;
                }
            }
        }
    }
    int dist_sum = md + td + ld;
    return dist_sum;
}

auto SparseSolver::A_star() {
    int time_limit = 200;
    auto start = std::chrono::system_clock::now();
    auto comp = [](const SparseBoard& lhs, const SparseBoard& rhs) {
        constexpr int coef = 2;
        int d1 = lhs.get_instruction().size() + coef*lhs.heuristic_distance();
        int d2 = rhs.get_instruction().size() + coef*rhs.heuristic_distance();
        return d1 > d2;
    };
    auto finish = [](const SparseBoard& bd) {
        return bd.is_solved();
    };
    std::priority_queue<SparseBoard,std::vector<SparseBoard>,std::function<bool(const SparseBoard&,const SparseBoard&)>> q(comp);
    std::unordered_set<SparseBoard,boost::hash<SparseBoard>> hash_set;
    hash_set.insert(current_board);
    q.emplace(current_board);
    while(!q.empty()) {
        auto end = std::chrono::system_clock::now();
        auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(end-start).count();
        if (elapsed > time_limit) {
            return false;
        }
        auto bd = q.top(); q.pop();
        if (finish(bd)) {
            current_board = bd;
            return true;
        }
        auto [bi,bj] = bd.get_blank_pos();
        auto nb = bd.neighbor_directions(bi,bj);
        for (auto dir : nb) {
            auto nextbd = bd;
            nextbd.move(dir);
            if (hash_set.find(nextbd) == hash_set.end()) {
                hash_set.insert(nextbd);
                q.emplace(nextbd);
            }
        }
    }
    throw std::runtime_error("A* search couldn't find solution");
}

auto SparseSolver::solve() {
    bool solved = A_star();
    if (solved) {
        return current_board.get_instruction();
    }
    throw std::runtime_error("A* couldn't find solution in time");
}
