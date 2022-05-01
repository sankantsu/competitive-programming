#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    int n,k;
    cin >> n >> k;
    vector<string> s(n);
    rep(i,n) cin >> s[i];

    int mx = -1;
    for (int t = 0; t < (1<<n); t++) {
        int res = 0;
        for (char c = 'a'; c <= 'z'; c++) {
            int cnt = 0;
            rep(i,n) {
                if (!((t>>i)&1)) continue;
                if (s[i].find(c) != string::npos) cnt++;
            }
            if (cnt == k) {
                res++;
            }
        }
        mx = max(mx,res);
    }
    cout << mx << endl;
}
