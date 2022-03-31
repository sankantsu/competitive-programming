#include <iostream>
#include <vector>
#include <list>
#include <utility>
#include <cmath>

using namespace std;

constexpr int max_n = pow(10,5);

int n;
vector<int> g[max_n];

int depth[max_n];

void init_depth() {
    for (int i = 0; i < n; i++) {
        depth[i] = -1;
    }
}

void calc_depth(int i, int d) {
    depth[i] = d;
    for (auto adj : g[i]) {
        if (depth[adj] >= 0) {
            continue;
        }
        calc_depth(adj,d+1);
    }
}

int main() {
    cin >> n;
    for (int i = 0; i < n-1; i++) {
        int u,v;
        cin >> u >> v;
        u--; v--;
        g[u].push_back(v);
        g[v].push_back(u);
    }
    
    // calc depth from 0
    init_depth();
    calc_depth(0,0);
    // find farthest vertex from 0
    int x;
    int max_depth = -1;
    for (int i = 0; i < n; i++) {
        if (depth[i] > max_depth) {
            max_depth = depth[i];
            x = i;
        }
    }
    // calc depth from x
    init_depth();
    calc_depth(x,0);
    // distance between x and farthest vertest from x is the diameter
    int d = -1;
    for (int i = 0; i < n; i++) {
        if (depth[i] > d) {
            d = depth[i];
        }
    }
    
    cout << (d + 1) << endl;
}
