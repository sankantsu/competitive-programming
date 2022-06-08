#include <iostream>
#include "solver.h"

auto input() {
    int n,t;
    std::cin >> n >> t;
    std::vector<std::string> s(n);
    for (int i = 0; i < n; i++) std::cin >> s[i];
    std::vector<std::vector<int>> tiles(n,std::vector<int>(n));
    for (int i = 0; i < n; i++) for (int j = 0; j < n; j++) {
        tiles[i][j] = hexchar2int(s[i][j]);
    }
    Board bd(n,std::move(tiles));
    return std::make_pair(n,bd);
}

int main() {
    auto [n,bd] = input();
    Solver slv(n,bd);
    slv.solve();
}
