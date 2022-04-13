#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

long n,k,p;
long a[40];
pair<long,long> sum1[1<<20];
// pair<long,long> sum2[1<<20];

int main() {
    cin >> n >> k >> p;
    for (long i = 0; i < n; i++) cin >> a[i];

    long n2 = n/2;
    for (long i = 0; i < (1<<n2); i++) {
        long cnt = 0;
        long price = 0;
        for (int j = 0; j < n2; j++) {
            if ((i>>j)&1) {
                cnt++;
                price += a[j];
            }
        }
        sum1[i] = make_pair(cnt,price);
    }
    sort(sum1,sum1+(1<<n2));
    /* for (int i = 0; i < (1<<n2); i++) { */
    /*     cout << i << ": " << sum1[i].first << " " << sum1[i].second << endl;; */
    /* } */
    /* cout << endl; */
    long res = 0;
    for (int i = 0; i < (1<<(n-n2)); i++) {
        long cnt = 0;
        long price = 0;
        for (int j = 0; j < n-n2; j++) {
            if ((i>>j)&1) {
                cnt++;
                price += a[n2+j];
            }
        }
        /* cout << "cnt,price = " << cnt << " " << price << endl; */
        if (cnt > k) continue;
        auto p1 = make_pair(k-cnt,0L);
        auto p2 = make_pair(k-cnt,p-price);
        auto lb = lower_bound(sum1,sum1+(1<<n2),p1);
        auto ub = upper_bound(sum1,sum1+(1<<n2),p2);
        /* cout << "lb: " << distance(sum1,lb) << ", "; */
        /* cout << "ub: " << distance(sum1,ub) << endl; */
        res += distance(lb,ub);
    }
    cout << res << endl;
}
