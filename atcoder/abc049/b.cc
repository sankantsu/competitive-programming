#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long h, w;
    cin >> h >> w;

    vector<string> c(h);
    rep(i,h) {
        cin >> c[i];
    }

    rep(i,h) {
        cout << c[i] << endl;
        cout << c[i] << endl;
    }
}
