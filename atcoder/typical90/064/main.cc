#include <iostream>
#include <cmath>

using namespace std;

constexpr int max_n = 100000;

long n,q;
long a[max_n];

long diff[max_n];

int main() {
    cin >> n >> q;
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }

    for (int i = 0; i < n-1; i++) {
        diff[i] = a[i+1] - a[i];
    }

    long inconvinience = 0;
    for (int i = 0; i < n-1; i++) {
        inconvinience += abs(diff[i]);
    }

    for (int i = 0; i < q; i++) {
        long l,r,v;
        cin >> l >> r >> v;
        l--; r--;
        if (l >= 1) {
            long prev = diff[l-1];
            long cur = prev + v;
            diff[l-1] = cur;
            inconvinience += abs(cur) - abs(prev);
        }
        if (r < n-1) {
            long prev = diff[r];
            long cur = prev - v;
            diff[r] = cur;
            inconvinience += abs(cur) - abs(prev);
        }
        cout << inconvinience << endl;
    }
}
