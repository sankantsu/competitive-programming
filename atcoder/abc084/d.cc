#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

constexpr int max_n = 100000;

bool is_prime[max_n];
/* vector<int> primes; */
int cnt[max_n]; // i 以下で条件を満たす整数の個数

void sieve() {
    for (int i = 2; i < max_n; i++) {
        is_prime[i] = true;
    }
    for (int i = 2; i <= max_n; i++) {
        if (is_prime[i]) {
            /* primes.push_back(i); */
            int j = 2*i;
            while (j < max_n) {
                is_prime[j] = false;
                j += i;
            }
        }
    }
}

void make_table() {
    for (int i = 1; i < max_n; i++) {
        cnt[i] = cnt[i-1];
        if (i%2 == 0) continue;
        if (is_prime[i] && is_prime[(i+1)/2]) {
            cnt[i]++;
        }
    }
}

int main() {
    int q;
    cin >> q;
    sieve();
    make_table();
    /* for (int i = 0; i < 20; i++) { */
    /*     cout << cnt[i] << endl; */
    /* } */
    for (int i = 0; i < q; i++) {
        int l,r;
        cin >> l >> r;
        cout << (cnt[r]-cnt[l-1]) << endl;
    }
}
