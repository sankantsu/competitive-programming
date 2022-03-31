#include <iostream>
#include <vector>

using namespace std;

long pow(long a, long b) {
    return (b > 0) ? a*pow(a,b-1) : 1;
}

auto prime_decomposition(long n) {
    long p = 2;
    const long m = n;
    vector<pair<long,int>> res;
    while (n > 1 && p*p <= m) {
        int cnt = 0;
        while (n%p == 0) {
            n /= p;
            cnt++;
        }
        if (cnt > 0) {
            res.push_back(make_pair(p,cnt));
        }
        p++;
    }
    if (n != 1) {
        res.push_back(make_pair(n,1));
    }
    return res;
}

template <typename Vector>
bool next(Vector &a, const Vector& b) {
    for (int i = 0; i < a.size(); i++) {
        if (a[i] < b[i]) {
            a[i]++;
            for (int j = 0; j < i; j++) {
                a[j] = 0;
            }
            return true;
        }
    }
    return false;
}

int main() {
    long k;
    cin >> k;

    const auto res = prime_decomposition(k);
    vector<int> prime_cnt(res.size());
    for (int i = 0; i < static_cast<int>(res.size()); i++) {
        prime_cnt[i] = res[i].second;
    }

    auto calc_factor = [&res](const auto& v) {
        long x = 1;
        for (int i = 0; i < static_cast<int>(v.size()); i++) {
            x *= pow(res[i].first,v[i]);
        }
        return x;
    };

    vector<int> v(res.size(),0);
    long cnt = 0;
    do {
        long a = calc_factor(v);
        /* cout << "a: " << a << endl; */
        if (a*a*a > k) {
            continue;
        }
        vector<int> u(v.size(),0);
        vector<int> rest(v.size());
        for (int i = 0; i < static_cast<int>(rest.size()); i++) {
            rest[i] = prime_cnt[i] - v[i];
        }
        do {
            long b = calc_factor(u);
            long c = (k/a)/b;
            /* cout << "b,c: " << b << " " << c << endl; */
            if (a > b || b > c) {
                continue;
            }
            cnt++;
        } while(next(u,rest));
    } while(next(v,prime_cnt));

    cout << cnt << endl;
}
