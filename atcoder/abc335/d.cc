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

    vector<vector<long>> map(n, vector<long>(n));
    long p = 0;
    long k = n - 1;
    long x = 0;
    long y = 0;
    while (p < n*n-1) {
        rep(i, k) map[x++][y] = ++p;
        rep(i, k) map[x][y++] = ++p;
        rep(i, k) map[x--][y] = ++p;
        rep(i, k) map[x][y--] = ++p;
        x++; y++;
        k -= 2;
    }

    long mid = (n-1)/2;
    rep(i,n) {
        rep(j,n) {
            if (i == mid && j == mid) cout << 'T' << " ";
            else {
                cout << map[i][j] << " ";
            }
        }
        cout << endl;
    }
}
