#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long a,b,c,d,e,f,x;

long dist(long a, long b, long c, long x) {
    long d = 0;
    rep(i,x) {
        if (i%(a+c) < a) d += b;
    }
    return d;
}

int main() {
    cin >> a >> b >> c >> d >> e >> f >> x;
    long takahashi = dist(a,b,c,x);
    long aoki = dist(d,e,f,x);
    /* cout << takahashi << " " << aoki << endl; */
    if (takahashi > aoki) {
        cout << "Takahashi" << endl;
    }
    else if (aoki > takahashi) {
        cout << "Aoki" << endl;
    }
    else {
        cout << "Draw" << endl;
    }
}
