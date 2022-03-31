#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

constexpr long inf = pow(10,12);

struct edge {
    int from;
    int to;
    long cost;
};

template <typename Edges>
auto warshall_floyd(int n, const Edges &edges) {
    vector<vector<long>> d(n,vector<long>(n,inf));
    for (auto e : edges) {
        int from = e.from;
        int to = e.to;
        d[from][to] = e.cost;
        d[to][from] = e.cost;
    }
    for (int k = 0; k < n; k++) {
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                d[i][j] = min(d[i][j],d[i][k]+d[k][j]);
            }
        }
    }
    return d;
}


template <typename Edges, typename Distance>
int solve(int n, const Edges &edges, const Distance &d) {
    int cnt = 0;
    // find removable edges
    for (auto e : edges) {
        int i = e.from;
        int j = e.to;
        bool removable = false;
        if (d[i][j] < e.cost) {
            removable = true;
        }
        else { // d[i][j] == e.cost;
            for (int k = 0; k < n; k++) {
                if (i == k || k == j) {
                    continue;
                }
                // kを経由するルートで最小コストのルートをつくれる
                if (d[i][j] == d[i][k] + d[k][j]) {
                    removable = true;
                }
            }
        }
        if (removable) {
            cnt++;
        }
    }
    return cnt;
}

int main() {
    int n,m;
    cin >> n >> m;
    vector<edge> v;
    for (int i = 0; i < m; i++) {
        int from,to;
        long cost;
        cin >> from >> to >> cost;
        v.push_back(edge{from-1,to-1,cost});
    }

    auto d = warshall_floyd(n,v);
    auto ans = solve(n,v,d);

    cout << ans << endl;
}
