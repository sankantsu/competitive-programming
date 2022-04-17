// A - 碁石ならべ
// https://atcoder.jp/contests/joi2008ho/tasks/joi2008ho_a
#include <iostream>
#include <stack>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
stack<int> s;

int main() {
    cin >> n;
    int cur = -1;
    rep(i,n) {
        int c;
        cin >> c;
        if (s.empty()) {
            cur = c;
            s.push(1);
            continue;
        }
        int l = s.top();
        if (c == cur) {
            s.pop();
            s.push(l+1);
        }
        else {
            cur = c;
            if ((i+1)%2 == 1) {
                s.push(1);
            }
            else {
                s.pop();
                if (s.empty()) {
                    s.push(l+1);
                }
                else {
                    int l2 = s.top();
                    s.pop();
                    s.push(l2+l+1);
                }
            }
        }
    }
    int res = 0;
    bool flag = (cur == 0);
    while(!s.empty()) {
        int l = s.top(); s.pop();
        if (flag) {
            res += l;
        }
        flag = !flag;
    }
    cout << res << endl;
}
