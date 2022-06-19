#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int h[3];
int w[3];

int main() {
    rep(i,3) cin >> h[i];
    rep(i,3) cin >> w[i];

    long res = 0;
    for (int b11 = 1; b11 < 30; b11++) {
        for (int b12 = 1; b12 < 30; b12++) {
            for (int b21 = 1; b21 < 30; b21++) {
                for (int b22 = 1; b22 < 30; b22++) {
                    int b13 = h[0] - b11 - b12;
                    int b23 = h[1] - b21 - b22;
                    int b31 = w[0] - b11 - b21;
                    int b32 = w[1] - b12 - b22;
                    int b33_1 = h[2] - b31 - b32;
                    int b33_2 = w[2] - b13 - b23;
                    if (b33_1 != b33_2) continue;
                    int b33 = b33_1;
                    if (b13 > 0 && b23 > 0 && b31 > 0 && b32 > 0 && b33 > 0) {
                        res++;
                    }
                }
            }
        }
    }
    cout << res << endl;
}
