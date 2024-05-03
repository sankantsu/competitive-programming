#include <iostream>
#include <ranges>
#include <atcoder/dsu.hpp>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n;
    cin >> n;

    atcoder::dsu uf(n);
    rep(i,n-1) {
        int u, v;
        cin >> u >> v;
        u--; v--;
        if (u != 0 && v != 0) uf.merge(u, v);
    }

    auto ss = uf.groups() | views::transform([](auto& group) { return group.size(); });
    cout << n - ranges::max(ss) << endl;
}
