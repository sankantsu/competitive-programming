#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long h, w, n;
    cin >> h >> w >> n;
    
    vector<long> a(n);
    rep(i,n) cin >> a[i];
    sort(a.begin(), a.end(), greater<int>{});

    using P = pair<long, long>;
    priority_queue<P> q;

    auto push = [&](long x, long y) {
        if (x > y) swap(x, y);
        q.emplace(x, y);
    };
    push(h, w);

    bool ans = true;
    rep(i,n) {
        auto [x, y] = q.top();
        q.pop();

        long size = 1 << a[i];
        /* cerr << "i,x,y,size: " << i << " " << x << " " << y << " " << size << endl; */
        if (x < size) {
            ans = false;
            break;
        }
        push(x - size, size);
        push(size, y - size);
        push(x - size, y - size);
    }
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
