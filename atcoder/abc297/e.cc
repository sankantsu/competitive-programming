#include <iostream>
#include <vector>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n, k;
    cin >> n >> k;
    
    vector<long> a(n);
    rep(i,n) {
        cin >> a[i];
    }

    vector<long> ans {0};
    priority_queue<long> frontier;

    rep(i,n) {
        frontier.push(-a[i]);
    }

    while (!frontier.empty()) {
        long v = (-1)*frontier.top();
        /* cerr << v << endl; */
        frontier.pop();

        if (ans[ans.size() - 1] == v) continue;

        ans.push_back(v);
        if (ans.size() == k + 1) {
            cout << ans[k] << endl;
            break;
        }
        rep(i,n) {
            frontier.push(-v - a[i]);
        }
    }
}
