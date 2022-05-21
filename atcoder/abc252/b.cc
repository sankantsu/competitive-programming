#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,k;
int a[200];

int main() {
    set<int> b;
    cin >> n >> k;
    rep(i,n) cin >> a[i];
    rep(i,k) {
        int x;
        cin >> x; x--;
        b.insert(x);
    }

    int mx = -1;
    rep(i,n) {
        mx = max(mx,a[i]);
    }

    int res = false;
    rep(i,n) {
        if (a[i] == mx && b.find(i) != b.end()) {
            res = true;
        }
    }
    if (res) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
