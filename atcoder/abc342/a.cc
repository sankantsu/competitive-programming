#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    string s;
    cin >> s;
    long n = s.size();
    long ans = -1;
    rep(i,n) {
        long cnt = 0;
        for (long j = -2; j <= 2; j++) {
            long k = i + j;
            if (0 <= k && k < n && s[k] != s[i]) {
                cnt++;
            }
        }
        if (cnt >= 2) {
            ans = i;
        }
    }
    cout << ans+1 << endl;
}
