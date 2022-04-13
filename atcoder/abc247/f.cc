#include <iostream>
#include <vector>
#include <algorithm>
#include <atcoder/modint>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;
using mint = atcoder::modint998244353;
// using mint = atcoder::modint1000000007;

int n;
int p[300000];
int q[300000];
vector<int> num_to_id[300000];

bool used[300000];

int dfs(int i, int start, int cnt) {
    used[i] = true;
    int j1 = num_to_id[q[i]][0];
    int j2 = num_to_id[q[i]][1];
    int j = i^j1^j2;
    if (j == start) {
        return cnt;
    }
    else {
        return dfs(j,start,cnt+1);
    }
}

mint f[300000];
mint g[300000];

void init() {
    f[1] = 2;
    f[2] = 3;
    f[3] = f[1]+f[2];
    g[1] = 1;
    g[2] = 3;
    g[3] = 4;
    for (int i = 4; i < 300000; i++) {
        f[i] = f[i-1] + f[i-2];
        g[i] = f[i-1] + f[i-3];
    }
    /* rep(i,10) { */
    /*     cout << f[i].val() << " "; */
    /* } */
    /* cout << endl; */
    /* rep(i,10) { */
    /*     cout << g[i].val() << " "; */
    /* } */
    /* cout << endl; */
}

int main() {
    cin >> n;
    rep(i,n) {
        cin >> p[i];
        num_to_id[p[i]].push_back(i);
    }
    rep(i,n) {
        cin >> q[i];
        num_to_id[q[i]].push_back(i);
    }
    init();
    mint res = 1;
    rep(i,n) {
        if (used[i]) continue;
        int cnt = dfs(i,i,1);
        /* cout << cnt << endl; */
        res *= g[cnt];
    }
    cout << res.val() << endl;
}
