#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long n;

auto primes() {
    const long max_i = 1<<20;
    bool is_prime[max_i];
    vector<long> vs;
    rep(i,max_i) is_prime[i] = true;
    for (long i = 2; i < max_i; i++) {
        if (is_prime[i]) {
            vs.push_back(i);
            for (long j = 2*i; j < max_i; j += i) {
                is_prime[j] = false;
            }
        }
    }
    return vs;
}

int main() {
    cin >> n;
    auto vs = primes();
    long cnt = 0;
    rep(i,vs.size()) {
        long q = vs[i];
        rep(j,i) {
            long p = vs[j];
            if (p*q*q*q <= n) cnt++;
            else break;
        }
    }
    cout << cnt << endl;
}
