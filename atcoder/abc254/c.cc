#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    int n,k;
    cin >> n >> k;
    vector<int> a(n);
    rep(i,n) cin >> a[i];

    vector<vector<int>> b(k);
    rep(i,n) {
        b[i%k].push_back(a[i]);
    }
    rep(j,k) {
        sort(b[j].begin(),b[j].end());
    }

    vector<int> c;
    rep(i,n) {
        int j = i%k;
        int l = i/k;
        c.push_back(b[j][l]);
    }
    int x = c[0];
    bool ans = true;
    rep(i,n) {
        if (x > c[i]) {
            ans = false;
            break;
        }
        x = c[i];
    }
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
