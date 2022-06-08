#include <iostream>
#include <vector>
#include <algorithm>
#include "generator.h"
#include "board.h"

template <typename Vec>
void print(const Vec& v) {
    using std::cout, std::endl;
    for (auto x : v) {
        cout << x << endl;
    }
}

template <typename Vec>
int calc_diff(const Vec& lhs, const Vec& rhs) {
    int sum = 0;
    for (int i = 0; i < lhs.size(); i++) {
        if (lhs[i] > rhs[i]) {
            sum += lhs[i]-rhs[i];
        }
    }
    return sum;
}

int main() {
    const int n = 6;
    Generator ge(n);
    ge.gen();
    Board bd(n,ge.get_board());
    auto cnt = bd.count();
    std::vector<int> v;
    for (int i = 0; i < 100; i++) {
        Generator g(n);
        g.gen();
        Board b(n,g.get_board());
        auto c = b.count();
        v.push_back(calc_diff(c,cnt));
    }
    std::sort(v.begin(),v.end());
    print(v);
}
