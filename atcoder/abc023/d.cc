#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n;
long long h[100001];
long long s[100001];

// can I achieve x point?
bool check(long long x) {
    /* cout << "check " << x << endl; */
    vector<long long> turn(n);
    for (int i = 0; i < n; i++) {
        if (x < h[i]) return false;
        turn[i] = (x-h[i])/s[i];
    }
    sort(turn.begin(),turn.end());
    for (int i = 0; i < n; i++) {
        /* cout << turn[i] << " "; */
        if (turn[i] < i) {
            /* cout << "NG" << endl; */
            return false;
        }
    }
    /* cout << "OK" << endl; */
    return true;
}

int main() {
    cin >> n;
    for (int i = 0; i < n; i++) {
        cin >> h[i] >> s[i];
    }

    long long lb = 0;
    long long ub = 1L<<62;
    // long long ub = 100;
    while (ub - lb > 1) {
        long long c = (lb+ub)/2;
        if (check(c)) ub = c;
        else lb = c;
    }
    cout << ub << endl;
    return 0;
}
