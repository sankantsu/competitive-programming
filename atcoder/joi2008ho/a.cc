// A - 碁石ならべ
// https://atcoder.jp/contests/joi2008ho/tasks/joi2008ho_a
#include <iostream>
#include <vector>
#include <algorithm>
#include <stack>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
stack<pair<int,int>> s; // color, length

int main() {
    cin >> n;
    rep(i,n) {
        int c;
        cin >> c;
        if (s.empty()) {
            s.emplace(c,1);
            continue;
        }
        auto [prev,l] = s.top(); s.pop();
        if ((i+1)%2 == 1) {
            if (c == prev) {
                s.emplace(c,l+1);
            }
            else {
                s.emplace(prev,l);
                s.emplace(c,1);
            }
        }
        else {
            if (c == prev) {
                s.emplace(c,l+1);
            }
            else {
                if (!s.empty() && s.top().first == c) {
                    auto [tmp,l2] = s.top();
                    s.pop();
                    s.emplace(c,l2+l+1);
                }
                else {
                    s.emplace(c,l+1);
                }
            }
        }
    }
    int res = 0;
    while(!s.empty()) {
        auto [c,l] = s.top(); s.pop();
        /* cout << c << " " << l << endl; */
        if (c == 0) {
            res += l;
        }
    }
    cout << res << endl;
}
