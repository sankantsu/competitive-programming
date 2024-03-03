#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <cmath>
#include <map>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

struct P {
    long first;
    long second;
};

int main() {
    long n, q;
    cin >> n >> q;

    vector<long> a(n);
    rep(i,n) {
        cin >> a[i];
    }

    long bucket_size = sqrt(n);
    long n_bucket = (n / bucket_size) + 1;
    vector<vector<long>> bucket(n_bucket);
    rep(i,n) {
        long b_id = i / bucket_size;
        bucket[b_id].push_back(a[i]);
    }
    vector<map<long, size_t>> counts(n_bucket);
    rep(i,n) {
        long b_id = i / bucket_size;
        counts[b_id][a[i]]++;
    }

    rep(i,q) {
        long type;
        cin >> type;
        if (type == 1) {
            long p, x;
            cin >> p >> x;
            p--;
            long b_id = p / bucket_size;
            long off = p % bucket_size;
            long cur = bucket[b_id][off];
            counts[b_id][cur]--;
            if (counts[b_id][cur] == 0) {
                counts[b_id].erase(cur);
            }
            bucket[b_id][off] = x;
            counts[b_id][x]++;
        }
        if (type == 2) {
            long l, r;
            cin >> l >> r;
            l--;
            map<long, size_t> cnt;
            for (long b_id = 0; b_id < n_bucket; b_id++) {
                long start = bucket_size*b_id;
                long end = start + bucket[b_id].size();
                if (end <= l || r <= start) {
                    continue;
                }
                else if (l <= start && end <= r) {
                    int j = 0;
                    for (auto it = counts[b_id].rbegin(); it != counts[b_id].rend() && j < 2; it++, j++) {
                        cnt[it->first] += it->second;
                    }
                }
                else {
                    long min_j = max(l, start);
                    long max_j = min(r, end) - 1;
                    for (long j = min_j; j <= max_j; j++) {
                        long x = bucket[b_id][j - start];
                        cnt[x]++;
                    }
                }
            }
            if (cnt.size() < 2) {
                cout << 0 << endl;
            }
            else {
                auto it = cnt.rbegin();
                it++;
                cout << it->second << endl;
            }
        }
    }
}
