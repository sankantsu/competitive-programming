#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
int X[300000];
long C[300000];

long hate[300000];

int main() {
    cin >> n;
    rep(i,n) {
        cin >> X[i];
        X[i]--;
    }
    rep(i,n) cin >> C[i];

    rep(i,n) {
        hate[X[i]] += C[i];
    }

    using P = pair<long,int>;
    priority_queue<P,vector<P>,greater<P>> q;
    rep(i,n) {
        q.emplace(hate[i],i);
    }

    long res = 0;
    vector<bool> used(n,false);
    while (!q.empty()) {
        auto [h,i] = q.top(); q.pop();
        /* cerr << "h,i: " << h << " " << i << endl; */
        if (used[i]) continue;
        if (hate[i] != h) continue;
        used[i] = true;
        res += h;
        // update hate value
        hate[X[i]] -= C[i];
        q.emplace(hate[X[i]],X[i]);
    }
    cout << res << endl;
}
