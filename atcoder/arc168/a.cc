#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    string s;
    cin >> s;
    s.push_back('<');

    long ans = 0;
    long cnt = 0;
    rep(i,n) {
        if (s[i] == '>') cnt++;
        else {
            ans += cnt*(cnt+1)/2;
            cnt = 0;
        }
        /* cerr << "i,cnt,ans: " << i << " " << cnt << " " << ans << endl; */
    }
    cout << ans << endl;
}
