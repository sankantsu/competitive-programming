#include <iostream>
#include <deque>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n, q;
    cin >> n >> q;
    /* cerr << "n,q: " << n << " " << q << endl; */

    using P = pair<long, long>;
    deque<P> pos;
    rep(i,n) pos.emplace_back(i+1, 0);

    rep(_, q) {
        long t;
        cin >> t;
        if (t == 1) {
            char c;
            cin >> c;
            auto [x, y] = pos.front();
            if (c == 'U') {
                pos.emplace_front(x, y+1);
            }
            if (c == 'R') {
                pos.emplace_front(x+1, y);
            }
            if (c == 'D') {
                pos.emplace_front(x, y-1);
            }
            if (c == 'L') {
                pos.emplace_front(x-1, y);
            }
            pos.pop_back();
        }
        else {
            long p;
            cin >> p;
            auto [x,y] = pos[p-1];
            cout << x << " " << y << endl;
        }
    }
}
