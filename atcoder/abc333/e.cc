#include <iostream>
#include <vector>
#include <map>
#include <algorithm>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n;
    cin >> n;

    vector<int> t(n), x(n);
    rep(i,n) cin >> t[i] >> x[i];

    int s = 0;
    int ans = 0;
    map<int, int> mp;
    vector<int> pick;
    for (int i = n-1; i >= 0; i--) {
        if (t[i] == 2) {
            mp[x[i]]++;
            s++;
            ans = max(ans, s);
        }
        else if (mp[x[i]] > 0) {
            mp[x[i]]--;
            s--;
            pick.push_back(1);
        }
        else {
            pick.push_back(0);
        }
    }
    for (auto [x, cnt] : mp) if (cnt > 0) ans = -1;

    if (ans == -1) {
        cout << -1 << endl;
    }
    else {
        reverse(pick.begin(), pick.end());
        cout << ans << endl;
        for (auto x : pick) cout << x << " "; cout << endl;
    }
}
