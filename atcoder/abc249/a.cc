#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int a,b,c,d,e,f,x;

int dist(int a, int b, int c, int x) {
    int d = 0;
    while (x > 0) {
        if (x >= a) {
            d += a*b;
            x -= a;
        }
        else {
            d += x*b;
            x -= x;
        }
        x -= c;
    }
    return d;
}

int main() {
    cin >> a >> b >> c >> d >> e >> f >> x;
    int l1 = dist(a,b,c,x);
    int l2 = dist(d,e,f,x);
    /* cout << l1 << " " << l2 << endl; */
    if (l1 > l2) {
        cout << "Takahashi" << endl;
    }
    else if (l2 > l1) {
        cout << "Aoki" << endl;
    }
    else {
        cout << "Draw" << endl;
    }
}
