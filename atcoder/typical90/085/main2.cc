#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
    long k;
    cin >> k;

    vector<long> v;
    for (long i = 1; i*i <= k; i++) {
        if (k%i == 0) {
            v.push_back(i);
            if (k/i != i) {
                v.push_back(k/i);
            }
        }
    }
    sort(v.begin(),v.end());

    long cnt = 0;
    for (auto a : v) {
        for (auto b : v) {
            if (k/a < b) continue;
            if (k%(a*b) != 0) continue;
            long c = (k/a)/b;
            if (a > b || b > c) continue;
            cnt++;
        }
    }
    cout << cnt << endl;
}
