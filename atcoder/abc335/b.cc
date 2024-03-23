#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    int n;
    cin >> n;

    rep(i,n+1) rep(j,n+1) rep(k,n+1) {
        int x = i + j + k;
        if (x <= n) {
            cout << i << " " << j << " " << k << endl;
        }
    }
}
