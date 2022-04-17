#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,q;
int a[200000];
int l[200000];
int r[200000];
int x[200000];

vector<int> idx[200001];

int main() {
    cin >> n;
    rep(i,n) cin >> a[i];
    cin >> q;
    rep(i,q) {
        cin >> l[i] >> r[i] >> x[i];
        l[i]--; r[i]--;
    }
    rep(i,n+1) {
        idx[a[i]].push_back(i); // already sorted !!
    }
    rep(i,q) {
        auto& v = idx[x[i]];
        auto lb = lower_bound(v.begin(),v.end(),l[i]);
        auto ub = upper_bound(v.begin(),v.end(),r[i]);
        int res = distance(lb,ub);
        cout << res << endl;
    }
}
