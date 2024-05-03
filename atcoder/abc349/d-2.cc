#include <iostream>
#include <vector>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long l, r;
    cin >> l >> r;

    using P = pair<long, long>;
    vector<P> ans;
    while (l < r) {
        int i = 0;
        while (l % (1L<<(i+1)) == 0 && l + (1L<<(i+1)) <= r) i++;
        ans.emplace_back(l, l + (1L<<i));
        l += 1L<<i;
    }

    cout << ans.size() << endl;
    for (auto [x, y] : ans) {
        cout << x << " " << y << endl;
    }
}
