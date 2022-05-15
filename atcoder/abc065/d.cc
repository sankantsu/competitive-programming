// D - Built?
// https://atcoder.jp/contests/abc065/tasks/arc076_b

#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;

struct point {
    int num;
    long x;
    long y;
};
vector<point> vp;

struct edge {
    int from;
    int to;
    long cost;
};

bool operator>(const edge l, const edge r) {
    return l.cost > r.cost;
}

priority_queue<edge,vector<edge>,greater<edge>> qe;

struct uftree {
    uftree(int n) : par(n) {
        rep(i,n) par[i] = i;
    }
    int find(int x) {
        if (par[x] == x) return x;
        return par[x] = find(par[x]);
    }
    int same(int x, int y) {
        return find(x) == find(y);
    }
    void unite(int x, int y) {
        x = find(x); y = find(y);
        if (x == y) return;
        par[x] = y;
    }
    vector<int> par;
};

int main() {
    cin >> n;
    rep(i,n) {
        long x,y;
        cin >> x >> y;
        vp.push_back(point{(int)i,x,y});
    }

    auto dist = [](point l, point r) { return min(abs(l.x - r.x),abs(l.y - r.y)); };

    sort(vp.begin(),vp.end(),[](point l, point r) { return l.x < r.x; });
    rep(i,n-1) {
        qe.push(edge{vp[i].num,vp[i+1].num,dist(vp[i],vp[i+1])});
    }

    sort(vp.begin(),vp.end(),[](point l, point r) { return l.y < r.y; });
    rep(i,n-1) {
        qe.push(edge{vp[i].num,vp[i+1].num,dist(vp[i],vp[i+1])});
    }

    uftree uf(n);
    long res = 0;
    while(!qe.empty()) {
        auto [i,j,cost] = qe.top(); qe.pop();
        if (uf.same(i,j)) continue;
        else {
            uf.unite(i,j);
            res += cost;
        }
    }
    cout << res << endl;
}
