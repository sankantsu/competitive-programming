// O(n log log n) AC
#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

vector<long> count_prime_factor;

void sieve(long n) {
    count_prime_factor.resize(n+1);
    for (long i = 2; i <= n; i++) {
        if (count_prime_factor[i] == 0) {
            count_prime_factor[i] = 1;
            long j = 2*i;
            while (j <= n) {
                count_prime_factor[j]++;
                j += i;
            }
        }
    }
}

bool check(long x, long k) {
    return count_prime_factor[x] >= k;
}

long n,k;

int main() {
    cin >> n >> k;
    sieve(n);
    long cnt = 0;
    for (long i = 2; i <= n; i++) {
        if (check(i,k)) {
            cnt++;
            /* cout << "i: " << i << endl; */
        }
    }
    cout << cnt << endl;
}
