#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

int n;
int p[10];
int q[10];

vector<int> v;

int check() {
    int ans = 0;
    for (int i = 0; i < n; i++) {
        if (p[i] != v[i]) {
            break;
        }
        if (i == n-1) {
            ans |= 0x1;
        }
    }
    for (int i = 0; i < n; i++) {
        if (q[i] != v[i]) {
            break;
        }
        if (i == n-1) {
            ans |= 0x2;
        }
    }
    return ans;
}
int main() {
    cin >> n;
    for (int i = 0; i < n; i++) cin >> p[i];
    for (int i = 0; i < n; i++) cin >> q[i];

    v.resize(n);
    /* cout << "v: "; */
    for (int i = 0; i < n; i++) {
        v[i] = i+1;
        /* cout << v[i] << " "; */
    }
    /* cout << endl; */

    int a,b;
    int cnt = 1;
    do {
        int c = check();
        if (c&0x1) a = cnt;
        if (c&0x2) b = cnt;
        cnt++;
    } while(next_permutation(v.begin(),v.end()));
    cout << abs(b-a) << endl;
}
