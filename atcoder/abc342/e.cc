#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

struct Edge {
    long start;
    long interval;
    long k;
    long cost;
    long from;
    long to;
};

long last_train_time(Edge e) {
    return e.start + e.interval*(e.k - 1);
}

int main() {
    long n, m;
    cin >> n >> m;
    
    vector<vector<Edge>> g(n);
    rep(i,m) {
        long l,d,k,c,a,b;
        cin >> l >> d >> k >> c >> a >> b;
        a--; b--;
        Edge e{l,d,k,c,a,b};
        g[b].push_back(e);
    }

    constexpr long inf = 1L<<60;
    using time_t = long;
    using state_t = std::pair<time_t, long>;
    
    vector<long> times(n, -inf);
    priority_queue<state_t> queue;
    queue.emplace(inf, n-1);
    while (!queue.empty()) {
        auto [t, to] = queue.top();
        queue.pop();
        if (t < times[to]) {
            continue;
        }
        times[to] = t;
        for (auto e : g[to]) {
            if (t < e.start + e.cost) continue;  // cannot use this train
            long last = last_train_time(e);
            if (last + e.cost <= t) {
                queue.emplace(last, e.from);
            }
            else {
                long nth = (t - e.cost - e.start) / e.interval;
                long t_dep = e.start + nth * e.interval;
                queue.emplace(t_dep, e.from);
            }
        }
    }
    for (long i = 0; i < n-1; i++) {
        if (times[i] == -inf) {
            cout << "Unreachable" << endl;
        }
        else {
            cout << times[i] << endl;
        }
    }
}
