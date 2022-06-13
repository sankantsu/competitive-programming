#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    int r,c;
    int a[2][2];
    cin >> r >> c; r--; c--;
    rep(i,2) rep(j,2) cin >> a[i][j];
    cout << a[r][c] << endl;
}
