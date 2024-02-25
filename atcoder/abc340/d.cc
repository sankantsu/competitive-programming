#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

struct Edge {
    long to;
    long cost;
};

int main() {
    long n;
    cin >> n;
    
    vector<vector<Edge>> g(n);
    rep(i,n-1) {
        long a, b, x;
        cin >> a >> b >> x;
        x--;
        g[i].push_back(Edge{i+1, a});
        g[i].push_back(Edge{x, b});
    }

    // dijkstra
    using Pair = std::pair<long, long>;  // cost, node
    long inf = 1l<<60;
    vector<long> distance(n, inf);
    priority_queue<Pair, vector<Pair>, greater<Pair>> queue;
    queue.emplace(0, 0);
    while (!queue.empty()) {
        auto [d, node] = queue.top();
        queue.pop();
        if (distance[node] <= d) {
            continue;
        }
        distance[node] = d;
        for (auto [to, c] : g[node]) {
            queue.emplace(d+c, to);
        }
    }
    cout << distance[n-1] << endl;
}
