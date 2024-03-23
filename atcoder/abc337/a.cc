#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    long xs = 0, ys = 0;
    rep(i,n) {
        long x, y;
        cin >> x >> y;
        xs += x;
        ys += y;
    }
    
    if (xs == ys) {
        cout << "Draw" << endl;
    }
    else if (xs > ys) {
        cout << "Takahashi" << endl;
    }
    else {
        cout << "Aoki" << endl;
    }
}
