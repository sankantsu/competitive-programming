#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <random>
#include <map>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

bool is_prime(long n) {
    for (long i = 2; i*i <= n; i++) {
        if (n%i == 0) return false;
    }
    return true;
}

auto gen_primes() {
    mt19937 mt;
    vector<long> vs;
    long m = pow(10,9);
    while (vs.size() < 10) {
        long x = m + mt()%m;
        if (is_prime(x)) {
            vs.push_back(x);
        }
    }
    return vs;
}

long get_mod(const string& s, long m) {
    long res = 0;
    rep(i, s.size()) {
        long d = s[i] - '0';
        res *= 10;
        res = (res + d)%m;
    }
    return res;
}

int main() {
    auto ps = gen_primes();

    long n;
    cin >> n;

    vector<string> a(n);
    rep(i,n) {
        cin >> a[i];
    }

    // get mod
    vector<vector<long>> b;
    rep(i,n) {
        vector<long> v;
        rep(k,10) {
            long p = ps[k];
            long x = get_mod(a[i], p);
            v.push_back(x);
        }
        b.push_back(v);
    }

    map<vector<long>, size_t> mp;
    rep(i,n) {
        mp[b[i]]++;
    }

    size_t ans = 0;
    rep(i,n) {
        rep(j,n) {
            vector<long> v;
            rep(k,10) {
                long p = ps[k];
                long x = b[i][k];
                long y = b[j][k];
                long z = (x * y) % p;
                v.push_back(z);
            }
            ans += mp[v];
        }
    }
    cout << ans << endl;
}
