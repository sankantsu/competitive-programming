#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    long n, c;
    cin >> n >> c;

    long m;
    cin >> m;

    vector<long> t(m), p(m);
    rep(i,m) {
        cin >> t[i] >> p[i];
    }
}
