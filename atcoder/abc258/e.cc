#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n, q, x;
    cin >> n >> q >> x;

    vector<long> w(n);
    rep(i,n) cin >> w[i];

    vector<long> sum(n + 1);
    rep(i,n) {
        sum[i+1] = sum[i] + w[i];
    }

    set<long> s;
    vector<long> v;
    vector<long> v_cnt;
    long i = 0;
    while (true) {
        long k = x;
        long cnt = 0;
        k -= (x/sum[n])*sum[n];
        cnt += n*(x/sum[n]);
        auto it = lower_bound(sum.begin(), sum.end(), sum[i] + k);
        if (it != sum.end()) {
            cnt += distance(next(sum.begin(), i), it);
            i = distance(sum.begin(), it);
        }
        else {
            cnt += n - i;
            k -= sum[n] - sum[i];
            auto it2 = lower_bound(sum.begin(), sum.end(), k);
            cnt += distance(sum.begin(), it2);
            i = distance(sum.begin(), it2);
        }
        v.push_back(i);
        v_cnt.push_back(cnt);
        if (s.contains(i)) {
            break;
        }
        s.insert(i);
    }

    long n_start = distance(v.begin(), find(v.begin(), v.end(), i)) + 1;
    long period = v.size() - n_start;

    rep(_,q) {
        long k;
        cin >> k;
        k--;

        if (k < v.size()) {
            cout << v_cnt[k] << endl;
        }
        else {
            long l = n_start + (k - n_start) % period;
            cout << v_cnt[l] << endl;
        }
    }
}
