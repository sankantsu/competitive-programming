#include "sparse.h"

int main() {
    using Index = SparseBoard::Index;
    using Position = SparseBoard::Position;
    using Container = SparseBoard::Container;

    std::random_device rd;
    std::mt19937 mt{rd()};

    // params
    const bool vertical = false;
    const Index n = 10;
    const std::size_t n_tile = 3;
    const int num_fixed = 0;

    auto is_fixed = [n,num_fixed](Position pos) {
        const Index fixed_row = num_fixed/n;
        const Index fixed_col = num_fixed - n*fixed_row;
        auto [i,j] = pos;
        return i < fixed_row || (i == fixed_row && j < fixed_col);
    };
    auto gen_random = [&mt](Index n) {
        Index i = mt()%n;
        Index j = mt()%n;
        return std::make_pair(i,j);
    };
    auto gen_random_positions = [n,&mt,gen_random,is_fixed](std::size_t size) {
        std::vector<std::pair<Index,Index>> v;
        while (v.size() < size) {
            auto p = gen_random(n);
            bool flag = true;
            if (is_fixed(p)) {
                flag = false;
            }
            else {
                for (auto q : v) {
                    if (p == q) {
                        flag = false;
                        break;
                    }
                }
            }
            if (flag) {
                v.push_back(p);
            }
        }
        return v;
    };
    auto make_goals = [n,n_tile,num_fixed,vertical]() {
        Container goal;
        for (std::size_t k = 0; k < n_tile; k++) {
            int offset = (vertical)? n*k : k;
            int id = offset + num_fixed;
            int i = id/n;
            int j = id - n*i;
            goal.push_back(Position{i,j});
        }
        return goal;
    };
    /* Position blank_pos = {1,2}; */
    /* Container cur = {{0,0},{0,2},{0,1}}; */
    /* Container goal = {{0,0},{0,1},{0,2}}; */
    auto rpos = gen_random_positions(1+n_tile);
    Position blank_pos = *rpos.crbegin();
    rpos.pop_back();
    Container cur = rpos;
    Container goal = make_goals();
    SparseBoard bd(n,blank_pos,cur,goal,is_fixed);
    bd.print();
    /* bd.heuristic_distance(); */
    SparseSolver slv(bd);
    auto ins = slv.solve();
    std::cerr << ins << std::endl;
    std::cerr << ins.size() << std::endl;
}
