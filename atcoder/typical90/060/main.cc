// 独自解法 AC

#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

constexpr int inf = 300001;

int n;
int a[300000];

/* dp0[i]: 長さi+1の増加中の部分列に現れる末尾の数字として最も小さいもの */
/* dp1[i]: 長さi+1の減少中の部分列に現れる末尾の数字として最も大きいもの */
int dp0[300000];
int dp1[300000];

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> a[i];

    for (int i = 0; i < n; i++) {
        dp0[i] = inf;
        dp1[i] = -1;
    }
    int mx = -1;
    int mx2 = -1;
    for (int j = 0; j < n; j++) {
        int k = distance(dp0,lower_bound(dp0,dp0+n,a[j]));
        dp0[k] = a[j];
        if (mx < k) {
            mx = k;
            dp1[k] = a[j];
        }
        else {
            int l = distance(dp1,lower_bound(dp1+mx,dp1+n,a[j],greater<int>{}));
            dp1[l] = a[j];
            if (mx2 < l) {
                mx2 = l;
            }
        }
    }
    /* for (int i = 0; i < n; i++) { */
    /*     cout << dp0[i] << " "; */
    /* } cout << endl; */
    /* for (int i = 0; i < n; i++) { */
    /*     cout << dp1[i] << " "; */
    /* } cout << endl; */
    cout << 1+max(mx,mx2) << endl;
}
