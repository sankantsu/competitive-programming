#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

// d: c の popcount
// x1 + x2 = d
// x1 + w = a
// x2 + w = b
// x1 + x2 + 2*w = a + b = d + 2*w
// -> a + b と d の偶奇は一致
// 2*w = a + b - d

void print(size_t x) {
    rep(i,60) {
        long j = 59 - i;
        cerr << ((x>>j) & 0x1);
    }
    cerr << endl;
}

size_t popcount(size_t x) {
    size_t pc = 0;
    rep(i,60) {
        if ((x>>i) & 0x1) pc++;
    }
    return pc;
}

int main() {
    size_t a, b, c;
    cin >> a >> b >> c;

    size_t pc = popcount(c);
    print(c);
    cerr << "pc: " << pc << endl;

    size_t w = a + b - pc;
    if (w%2 == 1) {
        cout << -1 << endl;
        return 0;
    }
    w = w/2;
    if (a < w || b < w || 60 - pc < w) {
        cout << -1 << endl;
        return 0;
    }
    else {
        size_t x = 0, y = 0, z = 0;
        size_t nx = 0, ny = 0, nz = 0;
        rep(i,60) {
            if ((c>>i) & 0x1) {
                if (nx < a - w) {
                    x |= (0x1L << i);
                    nx++;
                }
                else if (nx + ny < pc) {
                    y |= (0x1L << i);
                    ny++;
                }
            }
            else if (nz < w) {
                z |= (0x1L << i);
                nz++;
            }
        }
        /* print(x); */
        /* print(y); */
        /* print(z); */
        x |= z;
        y |= z;
        cout << x << " " << y << endl;
        /* cerr << "w: " << w << endl; */
        cerr << "popcount of x: " << popcount(x) << endl;
        cerr << "popcount of y: " << popcount(y) << endl;
        cerr << "x^y: " << (x^y) << endl;
    }
}
