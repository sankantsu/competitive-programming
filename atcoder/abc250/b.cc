#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,a,b;

int main() {
    cin >> n >> a >> b;
    rep(i,a*n) {
        rep(j,b*n) {
            int ii = i/a;
            int jj = j/b;
            if ((ii+jj)%2) {
                cout << '#';
            }
            else {
                cout << '.';
            }
        }
        cout << endl;
    }
}
