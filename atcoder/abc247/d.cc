#include <iostream>
#include <vector>
#include <algorithm>
#include <deque>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;

using P = pair<long,int>; // num, cnt
deque<P> q;

int main() {
    cin >> n;
    rep(i,n) {
        int type;
        cin >> type;
        if (type == 1) {
            long x,c;
            cin >> x >> c;
            q.push_back(make_pair(x,c));
        }
        else if (type == 2) {
            int c;
            cin >> c;
            int cnt = 0;
            long sum = 0;
            while (cnt < c) {
                auto [x,cc] = q.front(); q.pop_front();
                if (cnt + cc > c) {
                    sum += x*(c-cnt);
                    cc -= (c-cnt);
                    q.push_front(make_pair(x,cc));
                    break;
                }
                else {
                    sum += x*cc;
                    cnt += cc;
                }
            }
            cout << sum << endl;
        }
    }
}
