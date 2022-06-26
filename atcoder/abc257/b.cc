#include <iostream>
#include <vector>
#include <algorithm>
 
#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)
 
using namespace std;
 
int n,k,q;
int a[300];
int l[2000];
 
int main() {
    cin >> n >> k >> q;
    rep(i,k) cin >> a[i];
    rep(i,q) {
        cin >> l[i];
        l[i]--;
    }
 
    rep(i,q) {
        int p = l[i];
        int x = a[p];
        if (x == n) continue;
        if (p == k-1) {
            a[p] = x+1;
        }
        else {
            int y = a[p+1];
            if (y-x != 1) {
                a[p] = x+1;
            }
        }
    }
    rep(i,k) {
        cout << a[i] << " ";
    }
    cout << endl;
}
