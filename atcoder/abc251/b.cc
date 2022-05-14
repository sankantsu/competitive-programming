#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
long w;
long a[310];

int main() {
    cin >> n >> w;
    a[0] = 0;
    a[1] = 0;
    a[2] = 0;
    rep(i,n) cin >> a[i+3];

    vector<bool> flag(w+1,false);
    for (int i = 0; i < n+3; i++) {
        for (int j = i+1; j < n+3; j++) {
            for (int k = j+1; k < n+3; k++) {
                long s = a[i] + a[j] + a[k];
                /* cerr << "s: " << s << endl; */
                if (s <= w) flag[s] = true;
            }
        }
    }
    long cnt = 0;
    for (int i = 1; i <= w; i++) {
        if (flag[i]) {
            /* cerr << "i: " << i << endl; */
            cnt++;
        }
    }
    cout << cnt << endl;
}
