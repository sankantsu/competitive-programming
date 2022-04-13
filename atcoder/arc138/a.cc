#include <iostream>
#include <vector>
#include <algorithm>
#include <map>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,k;
long a[500000];

int main() {
    cin >> n >> k;
    rep(i,n) {
        cin >> a[i];
    }
    map<long,int,greater<long>> mp;
    long mn = 1L<<60;
    vector<pair<long,int>> vs;
    rep(i,k) {
        mn = min(mn,a[i]);
        vs.emplace_back(a[i],i);
    }
    sort(vs.begin(),vs.end());
    vector<pair<long,int>> vs2;
    int right = -1;
    for (auto p : vs) {
        if (p.second > right) {
            right = p.second;
            vs2.push_back(p);
        }
    }
    const long inf = 1L<<60;
    long ans = inf;
    for (int i = k; i < n; i++) {
        if (a[i] > mn) {
            long res = 0;
            /* for (int j = k-1; j >= 0; j--) { */
            /*     if (a[i] > a[j]) { */
            /*         res = i-j; */
            /*         break; */
            /*     } */
            /* } */
            int j = prev(lower_bound(vs2.begin(),vs2.end(),make_pair(a[i],-1)))->second;
            res = i-j;
            /* cout << "i,res: " << i << " " << res << endl; */
            ans = min(ans,res);
        }
    }
    if (ans == inf) ans = -1;
    cout << ans << endl;
}
