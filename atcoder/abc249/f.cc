#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

/* #include <contest/debug> */

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 200000;

long n,k;
long t[max_n+1];
long y[max_n+1];

long pos[max_n+1];
long sum[max_n+1];

int main() {
    cin >> n >> k;
    rep(i,n) cin >> t[i] >> y[i];

    int s = 0;
    rep(i,n) {
        if (t[i] == 1) {
            sort(y+s,y+i);
            s = i+1;
        }
    }
    sort(y+s,y+n);

    long cnt = 1;
    pos[0] = -1;
    rep(i,n) {
        if (t[i] == 1) {
            pos[cnt++] = i;
        }
    }
    cnt--;

    rep(i,n) {
        if (t[i] == 2) {
            sum[i+1] = sum[i]+y[i];
        }
        else {
            sum[i+1] = sum[i];
        }
    }

    priority_queue<int> q;
    long score = 0;
    long inf = 1L<<60;
    long res = -inf;
    rep(i,min(k,cnt)+1) {
        int p = pos[cnt-i];
        long x = (p < 0) ? 0 : y[p];
        long s = sum[n]-sum[p];
        /* print("i,p,x,s:",i,p,x,s); */
        x += s;
        int j = p+1;
        if (i+q.size() > k) {
            /* print("q.top():",q.top()); */
            score += q.top();
            q.pop();
        }
        while (i+q.size() < k && j < n && t[j] == 2 && y[j] < 0) {
            score -= y[j];
            q.push(y[j]);
            j++;
        }
        if (i+q.size() == k && !q.empty()) {
            while (j < n && t[j] == 2 && y[j] < q.top()) {
                /* print("q.top():",q.top()); */
                score += q.top();
                q.pop();
                score -= y[j];
                q.push(y[j]);
                j++;
            }
        }
        x += score;
        /* print("x:",x); */
        res = max(res,x);
    }
    cout << res << endl;
}
