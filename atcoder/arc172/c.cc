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

    string c;
    cin >> c;

    vector<long> v;
    rep(i,n) {
        if (c[i] == 'A') v.push_back(1);
        else if (c[i] == 'B') v.push_back(-1);
    }

    long ans = 1;
    long s = 0;
    rep(i,n-1) {
        long s1 = s + v[0];
        s = s + v[i+1];
        if (s1 == 0 && s == 0) continue;
        if (s1*s <= 0) ans++;
    }
    cout << ans << endl;
}
