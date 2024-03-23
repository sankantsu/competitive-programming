#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long t;
    cin >> t;

    rep(i,t) {
        long n, a, b;
        cin >> n >> a >> b;
        long n_pawn;
        if (a > n) {
            n_pawn = -1;
        }
        else if (a <= n/2) {
            n_pawn = (n - a)*((n+1)/2);
        }
        else {
            n_pawn = (n - a)*(n - a);
        }
        if (b <= n_pawn) {
            cout << "Yes" << endl;
        }
        else {
            cout << "No" << endl;
        }
    }
}
