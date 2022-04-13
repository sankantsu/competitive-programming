// G - Revenge of Traveling Salesman Problem
// https://atcoder.jp/contests/s8pc-1/tasks/s8pc_1_g
#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n,m;

using edge = tuple<int,long,long>; // to, cost, time
vector<edge> g[20];

long dp[1<<20][20];
long cnt[1<<20][20];

int main() {
    cin >> n >> m;
    rep(i,m) {
        int s,t;
        long d,time;
        cin >> s >> t >> d >> time;
        s--; t--;
        /* cout << "s,t,d,time: " << s << " " << t << " " << d << " " << time << endl; */
        g[s].emplace_back(t,d,time);
        g[t].emplace_back(s,d,time);
    }
    constexpr long inf = 1L<<60;
    rep(i,1<<n) {
        rep(j,n) {
            dp[i][j] = inf;
            cnt[i][j] = 0;
        }
    }
    dp[0][0] = 0;
    cnt[0][0] = 1;
    rep(i,1<<n) {
        rep(j,n) {
            if (dp[i][j] == inf) continue;
            for (auto [nj,cost,time] : g[j]) {
                if ((i>>nj)&1) continue;
                int ni = i | (1<<nj);
                long nd = dp[i][j] + cost;
                /* cout << "i,j,ni,nj: " << i << " " << j << " " << ni << " " << nj << endl; */
                /* cout << "cost,time: " << cost << " " << time << endl; */
                /* cout << "nd,time: " << nd << " " << time << endl; */
                if (nj == 0 && ni != (1<<n)-1) continue;
                if (nd <= time) {
                    if (nd < dp[ni][nj]) {
                        dp[ni][nj] = nd;
                        cnt[ni][nj] = cnt[i][j];
                    }
                    else if (nd == dp[ni][nj]) {
                        cnt[ni][nj] += cnt[i][j];
                    }
                }
            }
        }
    }
    /* rep(i,1<<n) { */
    /*     rep(j,n) { */
    /*         cout << dp[i][j] << " "; */
    /*     } */
    /*     cout << endl; */
    /* } */
    if (dp[(1<<n)-1][0] == inf) {
        cout << "IMPOSSIBLE" << endl;
    }
    else {
        cout << dp[(1<<n)-1][0] << " " << cnt[(1<<n)-1][0] << endl;
    }
}
