#include <iostream>
#include <vector>
#include <set>
#include <functional>
#include <algorithm>

using namespace std;

template <typename Graph>
auto scc(const Graph& g) {
    size_t n = g.size();
    Graph ginv(n);
    for (int i = 0; i < n; i++) {
        for (auto v : g[i]) {
            ginv[v].insert(i);
        }
    }
    vector<bool> used(n,false);
    vector<int> vs;
    auto dfs = [&g,&vs,&used](const auto &self, int v) -> void {
        used[v] = true;
        for (auto adj : g[v]) {
            if (!used[adj]) {
                self(self,adj);
            }
        }
        vs.push_back(v);
    };
    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            dfs(dfs,i);
        }
    }
    fill(used.begin(),used.end(),false);
    auto rdfs = [&ginv,&used](const auto &self, int v, set<int> &s) -> void {
        used[v] = true;
        s.insert(v);
        for (auto adj : ginv[v]) {
            if (!used[adj]) {
                self(self,adj,s);
            }
        }
    };
    vector<set<int>> components;
    for (auto it = vs.rbegin(); it != vs.rend(); it++) {
        int v = *it;
        if (!used[v]) {
            set<int> s;
            rdfs(rdfs,v,s);
            components.push_back(move(s));
        }
    }
    return components;
}

int main() {
    int n,m;
    cin >> n >> m;

    vector<set<int>> g(n);
    for (int i = 0; i < m; i++) {
        int a,b;
        cin >> a >> b; a--; b--;
        g[a].insert(b);
    }

    auto cmp = scc(g);

    long ans = 0;
    for (auto s : cmp) {
        size_t sz = s.size();
        ans += sz*(sz-1)/2;
    }
    cout << ans << endl;
}
