#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const long max_n = 200000;

long n,k;
long t[max_n+1];
long y[max_n+1];

int main() {
    cin >> n >> k;
    t[0] = 1; y[0] = 0;
    rep(i,n) cin >> t[i+1] >> y[i+1];

    const long inf = 1L<<60;
    long res = -inf;
    priority_queue<long> q;
    long score = 0;
    for (int i = n; i >= 0; i--) {
        if (t[i] == 1) {
            res = max(res,y[i]+score);
            if (k == 0) break;
            if (q.size() == k) {
                score += q.top();
                q.pop();
            }
            k--;
        }
        else if (t[i] == 2) {
            if (y[i] < 0 && q.size() < k) {
                q.push(y[i]);
            }
            else if (y[i] < 0 && q.size() == k) {
                if (!q.empty() && y[i] < q.top()) {
                    score += q.top();
                    q.pop();
                    q.push(y[i]);
                }
                else {
                    score += y[i];
                }
            }
            else {
                score += y[i];
            }
        }
    }
    cout << res << endl;
}
