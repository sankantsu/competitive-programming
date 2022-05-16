#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
int d[5010];
int c[5010];
long s[5010];

bool check(int set) {
    using task = pair<int,int>; // d,c
    priority_queue<task,vector<task>,greater<task>> q;
    rep(k,n) {
        if ((set>>k)&1) {
            q.emplace(d[k],c[k]);
        }
    }
    bool ans = true;
    int cur = 0;
    while(!q.empty()) {
        auto [nd,nc] = q.top(); q.pop();
        cur += nc;
        if (cur > nd) {
            ans = false;
            break;
        }
    }
    return ans;
}

long score(int set) {
    long res = 0;
    rep(k,n) {
        if ((set>>k)&1) {
            res += s[k];
        }
    }
    return res;
}

int main() {
    cin >> n;
    rep(i,n) cin >> d[i] >> c[i] >> s[i];

    long res = 0;
    rep(set,1<<n) {
        if(check(set)) {
            res = max(res,score(set));
        }
    }
    cout << res << endl;
}
