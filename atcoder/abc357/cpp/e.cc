#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

// 各頂点から到達可能な頂点の数を x_n とすると、x_n の和を求めれば良い。
//
// functional graph では、どの頂点からも最終的にサイクルに到達する
// https://drken1215.hatenablog.com/entry/2023/05/20/200517
//
// サイクルに含まれる頂点から到達可能な点の数は、サイクルの大きさに等しい。
// サイクルに含まれない頂点から到達可能な点の数は、サイクルに到達するまでに経由する頂点の数 + サイクルの大きさ
//
// 以下の 2 ステップでできそう
// 1. DFS でサイクルを検出
// 2. もう一回 DFS で到達可能頂点求める

using namespace std;

void dfs1(int u, int from, const vector<int>& a, vector<int>& start, vector<long>& cycle) {
    if (start[u] == from) {  // new cycle
        // calc cycle size
        int s = 0;
        int v = u;
        while (true) {
            s++;
            v = a[v];
            if (v == u) {
                break;
            }
        }
        // mark cycle size
        v = u;
        while (true) {
            cycle[v] = s;
            v = a[v];
            if (v == u) break;
        }
        return;
    }
    start[u] = from;
    dfs1(a[u], from, a, start, cycle);
}

// returns number of reachable vertices
long dfs2(int u, const vector<int>& a, const vector<long>& cycle, vector<long>& memo) {
    if (memo[u] != -1) {
        return memo[u];
    }
    if (cycle[u] != -1) {
        return cycle[u];
    }
    int res = 1 + dfs2(a[u], a, cycle, memo);
    memo[u] = res;
    return res;
}

int main() {
    int n;
    cin >> n;

    vector<int> a(n);
    rep(i,n) {
        cin >> a[i];
        a[i]--;
    }

    /*rep(i,n) {*/
    /*    cerr << a[i] << " ";*/
    /*}*/
    /*cerr << endl;*/

    vector<int> start(n, -1);
    vector<long> cycle(n, -1);
    rep(i,n) {
        if (start[i] != -1) continue;
        dfs1(i, i, a, start, cycle);
    }
    /*rep(i,n) {*/
    /*    cerr << "i, cycle[i]: " << i << " " << cycle[i] << endl;*/
    /*}*/

    vector<long> memo(n, -1);
    long s = 0;
    rep(i,n) {
        long res = dfs2(i, a, cycle, memo);
        s += res;
    }
    cout << s << endl;
}
