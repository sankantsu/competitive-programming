// D - Crossing
// https://atcoder.jp/contests/tenka1-2018-beginner/tasks/tenka1_2018_d

#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long n;

int main() {
    cin >> n;
    
    long s = 0;
    long k = 0;
    while (s < n) {
        s += k;
        k++;
    }
    if (s != n) {
        cout << "No" << endl;
        return 0;
    }

    cout << "Yes" << endl;
    cout << k << endl;

    vector<vector<long>> v(k);
    long x = 0;
    for (int i = 0; i < k; i++) {
        for (int j = i+1; j < k; j++) {
            x++;
            v[i].push_back(x);
            v[j].push_back(x);
        }
    }

    rep(i,k) {
        cout << k-1 << " ";
        for (auto x : v[i]) {
            cout << x << " ";
        }
        cout << endl;
    }
}
