#include <iostream>
#include <atcoder/maxflow>

using namespace std;
using namespace atcoder;

constexpr int max_n = 200000;

int n,m;
int a[max_n];
int b[max_n];
int c[max_n];
int d[max_n];

int main() {
    cin >> n >> m;
    for (int i = 0; i < n; i++) cin >> a[i];
    for (int i = 0; i < n; i++) cin >> b[i];
    for (int i = 0; i < m; i++) cin >> c[i];
    for (int i = 0; i < m; i++) cin >> d[i];

    mf_graph<int> g(n+m+2);

    for (int i = 0; i < n; i++) {
        g.add_edge(n+m,i,1);
    }
    for (int i = 0; i < m; i++) {
        g.add_edge(n+m+1,n+i,1);
    }
}
