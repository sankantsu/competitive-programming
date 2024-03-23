#include <iostream>
#include <vector>
#include <deque>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long k;
    cin >> k;

    constexpr long inf = 1L<<60;
    vector<long> dist(k, inf);

    using P = pair<long, long>;  // mod, dist
    deque<P> q;
    long ans;
    q.emplace_front(1, 1);
    while (true) {
        auto [r, d] = q.front();
        /* cerr << "r,d: " << r << " " << d << endl; */
        q.pop_front();
        if (d >= dist[r]) {
            continue;
        }
        dist[r] = d;
        if (r == 0) {
            ans = d;
            break;
        }
        q.emplace_front((r * 10) % k, d);
        q.emplace_back((r+1) % k, d+1);
    }
    cout << ans << endl;
}
