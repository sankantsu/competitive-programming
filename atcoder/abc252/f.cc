#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

const int max_n = 200000;

long n,l;
long a[max_n+10];

int main() {
    cin >> n >> l;

    long sum = 0;
    rep(i,n) {
        cin >> a[i];
        sum += a[i];
    }
    a[n] = l-sum;

    priority_queue<long,vector<long>,greater<long>> q;
    rep(i,n+1) {
        if (a[i] != 0) q.push(a[i]);
    }

    long res = 0;
    while (q.size() > 1) {
        long l1 = q.top(); q.pop();
        long l2 = q.top(); q.pop();
        res += l1+l2;
        q.push(l1+l2);
    }
    cout << res << endl;
}
