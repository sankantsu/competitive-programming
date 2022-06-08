#include <iostream>
#include "board.h"

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
    return bd;
}

int main() {
    auto bd = input();
    std::string ins;
    std::cin >> ins;
    bd.execute_instructions(ins);

    std::cout << std::dec << bd.eval() << std::endl;
}
