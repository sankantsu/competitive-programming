#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int k, g, m;
    cin >> k >> g >> m;

    int x = 0, y = 0;
    auto op = [&](int& x, int& y) {
        if (x == g) {
            x = 0;
        }
        else if (y == 0) {
            y = m;
        }
        else {
            int z = min(g - x, y);
            x += z;
            y -= z;
        }
    };
    rep(i,k) {
        op(x, y);
    }

    cout << x << " " << y << endl;
}
