#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

long n;
bool used[100001];

long pow(long a,long b) {
    if (b <= 0) return 1;
    return a*pow(a,b-1);
}

int main() {
    cin >> n;
    long cnt = 0;
    for (long i = 2; i*i <= n; i++) {
        if (used[i]) continue;
        long j = 2;
        while (true) {
            long k = pow(i,j);
            if (k > n) break;
            if (k <= 100000 && k*k <= n) {
                used[k] = true;
            }
            cnt++;
            j++;
        }
    }
    cout << (n - cnt) << endl;
}
