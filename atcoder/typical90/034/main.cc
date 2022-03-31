#include <iostream>
#include <unordered_map>
#include <algorithm>

using namespace std;

constexpr int max_n = 100000;

int n,k;
int a[max_n];

unordered_map<int,int> m;

int main() {
    cin >> n >> k;
    for (int i = 0; i < n; i++) cin >> a[i];

    int s = 0;
    int t = 0;
    int cur = 0;
    int res = -1;
    while (true) {
        res = max(res,t-s);
        int x = a[t++];
        if (t > n) break;
        m[x]++;
        if (m[x] == 1) {
            cur++;
        }
        while (cur > k) {
            x = a[s++];
            m[x]--;
            if (m[x] == 0) {
                cur--;
            }
        }
    }

    cout << res << endl;
}
