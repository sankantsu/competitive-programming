#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;

struct task {
    int d;
    int c;
    int s;
};

bool operator<(const task lhs, const task rhs) {
    return lhs.d < rhs.d;
}

long dp[5010];

int main() {
    cin >> n;
    vector<task> ts;
    rep(i,n) {
        int d,c,s;
        cin >> d >> c >> s;
        ts.push_back(task{d,c,s});
    }
    ts.push_back(task{0,0,0});
    sort(ts.begin(),ts.end());

    for (int i = ts.size()-1; i >= 0; i--) {
        for (int start = 0; start <= ts[i].d - ts[i].c; start++) {
            dp[start] = max(dp[start],dp[start+ts[i].c]+ts[i].s);
        }
    }
    cout << dp[0] << endl;
}
