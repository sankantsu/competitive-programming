#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    string s;
    string t;
    cin >> n;
    cin >> s;
    cin >> t;

    long l = 0;
    long r = n - 1;
    bool check = true;
    while (l < n && t[l] == 'B') {
        if (s[l] != t[l]) {
            check = false;
            break;
        }
        l++;
    }
    while (0 <= r && t[r] == 'A') {
        if (s[r] != t[r]) {
            check = false;
            break;
        }
        r--;
    }
    if (!check) {
        cout << -1 << endl;
        return 0;
    }
    cerr << "l,r: " << l << " " << r << endl;

    priority_queue<long> a;
    priority_queue<long> b;
    for (long i = l; i <= r; i++) {
        if (s[i] == 'B' && t[i] == 'A') {
            a.push(-i);
        }
        if (s[i] == 'A' && t[i] == 'B') {
            b.push(-i);
        }
    }

    long ans = 0;
    while (!a.empty() || !b.empty()) {
        long i, j;
        if (!b.empty()) {
            j = -b.top();
            b.pop();
        }
        else {
            j = r;
        }
        if (!a.empty() && -a.top() < j) {
            i = -a.top();
            a.pop();
        }
        else {
            i = l;
        }
        ans++;
    }
    cout << ans << endl;
}
