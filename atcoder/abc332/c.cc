#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int n, m;
    cin >> n >> m;

    string s;
    cin >> s;
    s.push_back('0');

    int ans = 0;
    int x = 0, y = 0;
    rep(i,n+1) {
        /* cerr << "i,x,y: " << i << " " << x << " " << y << endl; */
        if (s[i] == '0') {
            ans = max(ans, max(x - m, 0) + y);
            x = 0;
            y = 0;
        }
        else if (s[i] == '1') x++;
        else if (s[i] == '2') y++;
    }
    cout << ans << endl;
}
