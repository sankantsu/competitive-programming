#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;
int a[200010];
int b[200010];

using edge = pair<int,int>; // to, id
vector<edge> g[200010];

char s[200010];
bool used[200010];

void dfs(int v) {
    used[v] = true;
   for (auto [u,id] : g[v]) {
       // skip already used edge
       if (s[id] != '\0') continue;
       // otherwise direct edge as v -> u
       auto direct_edge = [&]() {
           s[id] = (v == a[id]) ? '0' : '1';
       };
       if (used[u]) {
           direct_edge();
       }
       else {
           direct_edge();
           dfs(u);
       }
    }
}

int main() {
    cin >> n >> m;
    rep(i,m) {
        cin >> a[i];
        a[i]--;
    }
    rep(i,m) {
        cin >> b[i];
        b[i]--;
    }

    rep(i,m) {
        g[a[i]].emplace_back(b[i],i);
        g[b[i]].emplace_back(a[i],i);
    }

    rep(i,n) {
        dfs(i);
    }
    cout << s << endl;
}
