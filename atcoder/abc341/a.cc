#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    int n;
    cin >> n;
    cout << 1;
    rep (i,n) {
        cout << 0;
        cout << 1;
    }
    cout << endl;
}
