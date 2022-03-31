#include <cstdio>
#include <vector>

using namespace std;

#define inf 10000.0

int n; // number of tickets
int m; // number of cities
int p; // number of roads
int a; // starting city
int b; // destination city

int tickets[8]; // tickets

struct edge {
    int to;
    int cost;
    edge() : to(0), cost(0) {}
    edge(int t, int c) : to(t), cost(c) {}
};

// edge g[30][31]; // roads
vector<edge> g[30];
/* int num_edge[30]; */

double dp[1<<8][30]; // minimum time with remaining tickets
bool reached[1<<8][30];

inline double min(double a, double b) {
    return a < b ? a : b;
}

double rec(int set, int v) {
    if (reached[set][v]) {
        return dp[set][v];
    }

    reached[set][v] = true;
    if (v == b) {
        dp[set][v] = 0.0;
        return 0.0;
    }
    double res = inf;
    for (int k = 0; k < n; k++) {
        int t = 1<<k;
        if (!(set&t)) {
            continue;
        }
        /* for (int i = 0; i < num_edge[v]; i++) { */
        for (int i = 0; i < g[v].size(); i++) {
            edge e = g[v][i];
            int s = (set & ~t);
            double r = rec(s,e.to) + (double)e.cost/tickets[k];
            res = min(res,r);
        }
    }
    dp[set][v] = res;
    return res;
}

bool solve() {
    scanf("%d %d %d %d %d",&n,&m,&p,&a,&b);
    if (n == 0) {
        return false;
    }
    a--; b--;
    for (int i = 0; i < n; i++) {
        scanf("%d",&tickets[i]);
    }

    /* for (int i = 0; i < m; i++) { */
    /*     num_edge[i] = 0; */
    /* } */
    for (int i = 0; i < 30; i++) {
        g[i].erase(g[i].begin(),g[i].end());
        /* printf("size: %d\n",g[i].size()); */
    }
    
    for (int i = 0; i < p; i++) {
        int x,y,z;
        scanf("%d %d %d",&x,&y,&z);
        x--; y--;
        /* g[x][num_edge[x]++] = edge(y,z); */
        /* g[y][num_edge[y]++] = edge(x,z); */
        g[x].push_back(edge(y,z));
        g[y].push_back(edge(x,z));
    }

    for (int s = 0; s < (1<<n); s++) {
        for (int i = 0; i < 30; i++) {
            dp[s][i] = inf;
            reached[s][i] = false;
        }
    }

    double ans = rec((1<<n)-1,a);

    if (ans == inf) {
        printf("Impossible\n");
    }
    else {
        printf("%.5f\n",ans);
    }

    return true;
}

int main() {
    while (solve()) {
    }
    return 0;
}
