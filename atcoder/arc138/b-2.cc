// 01 Generation
#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
int a[300000];

int main() {
    cin >> n;
    rep(i,n) {
        cin >> a[i];
    }
    bool ans = false;
    int p = 0;
    int q = n-1;
    int flip = 0;
    while(true) {
        if ((a[q]^flip) == 0) {
            if (p == q) {
                ans = true;
                break;
            }
            q--;
        }
        else {
            if ((a[p]^flip) == 1) {
                break;
            }
            p++;
            flip ^= 1;
        }
    }
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
