#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n,q;
int s[100000];
int t[50000];

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> s[i];
    cin >> q;
    for (int i = 0; i < q; i++) cin >> t[i];

    int cnt = 0;
    for (int i = 0; i < q; i++) {
        /* cout << "t[i]: " << t[i] << endl; */
        int ok = 0;
        int ng = n;
        while (ng - ok > 1) {
            /* cout << "ok,ng: " << ok << " " << ng << endl; */
            int c = (ok+ng)/2;
            if (s[c] <= t[i]) ok = c;
            else ng = c;
        }
        if (s[ok] == t[i]) {
            /* cout << "found " << t[i] << endl; */
            cnt++;
        }
    }
    cout << cnt << endl;
}
