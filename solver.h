#pragma once
#include <iostream>
#include <iomanip>
#include <vector>
#include <queue>
#include <functional>
#include <boost/container_hash/hash.hpp>
#include <unordered_map>
#include <random>
#include "util.h"
#include "generator.h"
#include "board.h"
#include "mutator.h"
#include "sparse.h"

struct Solver {
    Solver(int n_, Board ini)
        : n(n_), generator(n),
          initial_board(ini),
          current_board(ini),
          target_board(ini),
          min_instruction_length(3*n*n*n)
    {}
    auto solve();
    private:
    template <typename Func>
    void bfs(int i, int j, Func fun);
    void gen_target_tree();
    auto assign_id(int i, int j);
    auto solve_htiles(int gi, int& gj, bool reverse, int& num_fixed, int ntile);
    auto solve_line(int row, bool reverse);
    auto solve_vtiles(int gj, int ntile);
    auto solve_last_three_lines();
    auto assign_last_twelve_ids();
    auto solve_last_tweleve_tiles();
    int n;
    static constexpr int time_limit = 2500;
    Generator generator;
    Board initial_board;
    Board current_board;
    Board target_board;
    int min_instruction_length;
    std::string best_instruction;
    Timer timer;
    static constexpr int beam_width = 200;
};

void Solver::gen_target_tree() {
    auto cnt_ini = current_board.count();
    while (true) {
        generator.gen();
        target_board = Board{n,generator.get_board()};
        Mutator mu(n,target_board,cnt_ini);
        const int num_mutate = 100;
        for (int i = 0; i < num_mutate; i++) {
            if (mu.get_diff() == 0) {
                target_board = mu.get_board();
                return;
            }
            mu.mutate();
        }
    }
}

template <typename Func>
void Solver::bfs(int si, int sj, Func fun) {
    constexpr int dx[4] = {0,-1,0,1};
    constexpr int dy[4] = {-1,0,1,0};
    std::vector<std::vector<bool>> visited(n,std::vector<bool>(n,false));
    std::queue<std::pair<int,int>> q;
    q.emplace(si,sj);
    while(!q.empty()) {
        auto [i,j] = q.front(); q.pop();
        if (visited[i][j]) continue;
        if (fun(i,j)) {
            break;
        }
        visited[i][j] = true;
        for (int dir = 0; dir < 4; dir++) {
            int ni = i + dx[dir];
            int nj = j + dy[dir];
            if (ni < 0 || n <= ni || nj < 0 || n <= nj) continue;
            q.emplace(ni,nj);
        }
    }
    return;
}

auto Solver::assign_id(int i, int j) { // (i,j) is the position on the target board
    int id = n*i+j;
    int ti = -1, tj = -1;
    int tile = target_board.get_tile(i,j);
    if (tile == 0) {
        throw std::runtime_error("tried to assign id to blank tile");
    }
    auto fun = [this,&ti,&tj,tile](int i, int j) {
        if (current_board.get_tile(i,j) == tile && current_board.get_tile_id(i,j) < 0) {
            ti = i;
            tj = j;
            return true;
        }
        return false;
    };
    bfs(i,j,fun);
    assert(ti != -1); // always success to assign id
    current_board.set_tile_id(ti,tj,id);
    return std::make_pair(ti,tj);
}

auto Solver::solve_htiles(int gi, int& gj, bool reverse, int& num_fixed, int ntile) {
    const int n = current_board.get_board_size();
    int dj = (reverse) ? -1 : 1;
    auto is_fixed = [n,num_fixed,reverse](SparseBoard::Position pos) {
        const SparseBoard::Index fixed_row = num_fixed/n;
        const SparseBoard::Index fixed_col = num_fixed - n*fixed_row;
        auto [i,j] = pos;
        bool row_constraint = (i < fixed_row);
        bool col_constraint = (reverse) ? (j > n-1-fixed_col) : (j < fixed_col);
        return row_constraint || (i == fixed_row && col_constraint);
    };
    auto make_cur = [this,gi,gj,dj,ntile] {
        SparseBoard::Container cur;
        for (int k = 0; k < ntile; k++) {
            cur.push_back(assign_id(gi,gj+k*dj));
        }
        return cur;
    };
    auto make_goals = [gi,gj,dj,ntile] {
        SparseBoard::Container goal;
        for (int k = 0; k < ntile; k++) {
            goal.push_back({gi,gj+k*dj});
        }
        return goal;
    };
    auto blank_pos = current_board.get_blank_pos();
    auto cur = make_cur();
    auto goal = make_goals();
    SparseBoard bd(n,blank_pos,cur,goal,is_fixed);
    SparseSolver solver(bd);
    try {
        auto ins = solver.solve();
        current_board.execute_instructions(ins);
        gj += ntile*dj;
        num_fixed += ntile;
        return true;
    }
    catch (const std::exception& e) {
        std::cerr << "Failed while solving a line" << std::endl;
        for (auto [i,j] : cur) {
            current_board.reset_tile_id(i,j);
        }
        return false;
    }
}

auto Solver::solve_line(int row, bool reverse) {
    const int n = current_board.get_board_size();
    const int gi = row;
    int num_fixed = n*row;
    int gj = (reverse) ? n-1 : 0;
    for (int k = 0; k < (n+2)/3; k++) {
        auto elapsed = timer.elapsed_milliseconds();
        if (elapsed > time_limit) {
            return false;
        }
        int ntile = 3;
        if (k == 0 && n%3 != 0) {
            ntile = 2;
        }
        else if (k == 1 && n%3 == 1) {
            ntile = 2;
        }
        bool solved = solve_htiles(gi,gj,reverse,num_fixed,ntile);
        if (!solved) {
            elapsed = timer.elapsed_milliseconds();
            if (elapsed > time_limit) {
                return false;
            }
            // if failed again give up to solve this board
            if(!solve_htiles(gi,gj,reverse,num_fixed,1)) {
                return false;
            }
            if(!solve_htiles(gi,gj,reverse,num_fixed,ntile-1)) {
                return false;
            }
        }
    }
    return true;
}

auto Solver::assign_last_twelve_ids() {
    SparseBoard::Container cur;
    for (int i = n-3; i < n; i++) {
        for (int j = n-4; j < n; j++) {
            if (!target_board.get_tile(i,j) == 0) {
                cur.push_back(assign_id(i,j));
            }
        }
    }
    auto swap_id = [this,&cur] {
        for (std::size_t k = 0; k < cur.size(); k++) {
            for (std::size_t l = k+1; l < cur.size(); l++) {
                auto [i,j] = cur[k];
                auto [i2,j2] = cur[l];
                auto t1 = current_board.get_tile(i,j);
                auto t2 = current_board.get_tile(i2,j2);
                if (t1 == t2) {
                    std::swap(cur[k],cur[l]);
                    current_board.swap_tile(i,j,i2,j2);
                    return;
                }
            }
        }
    };
    if (!current_board.is_solvable()) {
        swap_id();
    }
    current_board.calc_manhattan_distance();
    return cur;
}

auto Solver::solve_last_tweleve_tiles() {
    const int n = current_board.get_board_size();
    auto cur = assign_last_twelve_ids();
    if (!current_board.is_solvable()) {
        return false;
    }
    auto make_goals = [n,this] {
        SparseBoard::Container goal;
        for (int i = n-3; i < n; i++) {
            for (int j = n-4; j < n; j++) {
                if (!target_board.get_tile(i,j) == 0) {
                    goal.push_back({i,j});
                }
            }
        }
        return goal;
    };
    auto is_fixed = [n](auto pos) {
        auto [i,j] = pos;
        return i < n-3 || j < n-4;
    };
    SparseBoard::Position blank_pos = current_board.get_blank_pos();
    SparseBoard::Container goal = make_goals();
    SparseBoard bd(n,blank_pos,cur,goal,is_fixed);
    SparseSolver solver(bd);
    auto ins = solver.solve();
    current_board.execute_instructions(ins);
    return true;
}

auto Solver::solve_vtiles(int gj, int ntile) {
    const int n = current_board.get_board_size();
    auto is_fixed = [n,gj,ntile](SparseBoard::Position pos) {
        int row1 = n-3;
        auto [i,j] = pos;
        auto cns = (ntile == 2) ? (i == row1 && j == gj) : false;
        return i < row1 || j < gj || cns;
    };
    auto make_cur = [this,n,gj,ntile] {
        int row = (ntile == 2) ? n-2 : n-3;
        SparseBoard::Container cur;
        for (int k = 0; k < ntile; k++) {
            cur.push_back(assign_id(row++,gj));
        }
        return cur;
    };
    auto make_goals = [n,gj,ntile] {
        int row = (ntile == 2) ? n-2 : n-3;
        SparseBoard::Container goal;
        for (int k = 0; k < ntile; k++) {
            goal.push_back({row++,gj});
        }
        return goal;
    };
    SparseBoard::Position blank_pos = current_board.get_blank_pos();
    SparseBoard::Container cur = make_cur();
    SparseBoard::Container goal = make_goals();
    SparseBoard bd(n,blank_pos,cur,goal,is_fixed);
    SparseSolver solver(bd);
    try {
        auto ins = solver.solve();
        current_board.execute_instructions(ins);
        return true;
    }
    catch (const std::exception& e) {
        std::cerr << "Failed while solving last three lines. Reset ids..." << std::endl;
        for (auto [i,j] : cur) {
            current_board.reset_tile_id(i,j);
        }
        return false;
    }
}

auto Solver::solve_last_three_lines() {
    const int n = current_board.get_board_size();
    for (auto gj = 0; gj < n-4; gj++) {
        auto elapsed = timer.elapsed_milliseconds();
        if (elapsed > time_limit) {
            return false;
        }
        int ntile = 3;
        bool solved = solve_vtiles(gj,ntile);
        if (!solved) {
            elapsed = timer.elapsed_milliseconds();
            if (elapsed > time_limit) {
                return false;
            }
            // if failed again give up to solve this board
            if(!solve_vtiles(gj,1)) {
                return false;
            }
            if(!solve_vtiles(gj,ntile-1)) {
                return false;
            }
        }
    }
    if (!solve_last_tweleve_tiles()) {
        return false;
    }
    assert(current_board.get_manhattan_distance() == 0);
    return true;
}

auto Solver::solve() {
    while(true) {
        auto elapsed = timer.elapsed_milliseconds();
        if (elapsed > time_limit) {
            break;
        }
        current_board = initial_board;
        gen_target_tree();
        {
            bool flag = true;
            for (int row = 0; row < n-3; row++) {
                bool reverse = static_cast<bool>((n+row)%2);
                if(!solve_line(row,reverse)) {
                    std::cerr << "Give up to solve line. I will retry" << std::endl;
                    flag = false;
                    break;
                }
            }
            if (!flag) {
                continue;
            }
        }
        if (!solve_last_three_lines()) {
            std::cerr << "Ran into impossible board!!! I will retry." << std::endl;
            continue;
        }
        auto& bd = current_board;
        bd.eliminate_wasted_instructions();
        bd.resize_instructions();
        int instruction_length = bd.get_instructions().size();
        std::cerr << "instruction length: " << instruction_length << std::endl;
        if (instruction_length < min_instruction_length) {
            min_instruction_length = instruction_length;
            best_instruction = bd.get_instructions();
        }
    }
    std::cerr << "-------------------" << std::endl;
    std::cerr << "instruction length: " << min_instruction_length << std::endl;
    std::cout << best_instruction << std::endl;
}
