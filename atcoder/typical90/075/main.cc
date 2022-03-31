#include <iostream>
#include <vector>

using namespace std;

constexpr long pow(long a, long b) {
    return (b > 0) ? a*pow(a,b-1) : 1;
}

constexpr long max_n = pow(10,12);
constexpr long sqrt_max_n = pow(10,6);

bool is_prime[sqrt_max_n];
vector<long> primes;

// calc primes under n
void sieve() {
    for (long i = 0; i < sqrt_max_n; i++) {
        is_prime[i] = true;
    }
    is_prime[0] = false;
    is_prime[1] = false;
    for (long p = 2; p < sqrt_max_n; p++) {
        if (is_prime[p]) {
            primes.push_back(p);
            long q = 2*p;
            while (q < sqrt_max_n) {
                is_prime[q] = false;
                q += p;
            }
        }
    }
}

long count_prime_factors(long n) {
    long cnt = 0;
    for (auto p : primes) {
        if (n == 1) {
            break;
        }
        while (n % p == 0) {
            n /= p;
            cnt++;
        }
    }
    if (n != 1) cnt++;
    return cnt;
}

int main() {
    long n;
    cin >> n;

    sieve();
    long cnt = count_prime_factors(n);
    /* cout << cnt << endl; */

    long ans = 0;
    long cur = 1;
    while (cur < cnt) {
        cur <<= 1;
        ans += 1;
    }

    cout << ans << endl;
}
