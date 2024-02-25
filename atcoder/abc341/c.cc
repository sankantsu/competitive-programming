#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

auto move_once(long i, long j, char c) {
    switch(c) {
        case ('L'):
            return make_pair(i, j-1);
        case ('R'):
            return make_pair(i, j+1);
        case ('U'):
            return make_pair(i-1, j);
        case ('D'):
            return make_pair(i+1, j);
        default:
            assert(false);
    }
}

int main() {
    long h, w, n;
    cin >> h >> w >> n;

    string t;
    vector<string> s(h);
    cin >> t;
    rep (i,h) {
        cin >> s[i];
    }

    long cnt = 0;
    rep(i,h) {
        rep(j,w) {
            if (i == 0 || i == h-1 || j == 0 || j == w-1) continue;  // start from edge
            long ci = i;
            long cj = j;
            rep(k,n+1) {
                if (s[ci][cj] != '.') break;
                if (k == n) {
                    cnt++;
                    break;
                }
                char c = t[k];
                auto [ni, nj] = move_once(ci,cj,c);
                ci = ni;
                cj = nj;
            }
        }
    }
    cout << cnt << endl;
}
