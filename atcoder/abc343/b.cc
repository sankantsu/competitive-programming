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

    vector<vector<long>> a(n, vector<long>(n));
    rep(i,n) {
        rep(j,n) {
            cin >> a[i][j];
        }
    }
    
    rep(i,n) {
        rep(j,n) {
            if (a[i][j] == 1) {
                cout << j + 1 << " ";
            }
        }
        cout << endl;
    }
}
