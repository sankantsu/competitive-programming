// E - Sequence Decomposing
// https://atcoder.jp/contests/abc134/tasks/abc134_e
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 100000;

int n;
long a[max_n+10];

int main() {
    cin >> n;
    rep(i,n) cin >> a[i];

    vector<long> v;
    rep(i,n) {
        auto it = upper_bound(v.begin(),v.end(),-a[i]);
        if (it == v.end()) {
            v.push_back(-a[i]);
        }
        else {
            *it = -a[i];
        }
    }
    cout << v.size() << endl;
}
