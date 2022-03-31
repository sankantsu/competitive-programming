#include <iostream>
#include <atcoder/modint>

using namespace std;
using mint = atcoder::modint1000000007;

mint pow(mint a, int b) {
    return (b > 0) ? a*pow(a,b-1) : 1;
}

// sum of [a,b]
mint interval_sum(mint a, mint b) {
    return (a + b)*(b - a + 1)/2;
}

int digits(long x) {
    int d = 0;
    while (x > 0) {
        d++;
        x /= 10;
    }
    return d;
}

int main () {
    long l,r;
    cin >> l >> r;

    int l_digits = digits(l);
    int r_digits = digits(r);
    // cout << l_digits << " " << r_digits << endl;

    mint sum = 0;
    if (l_digits == r_digits) {
        sum += l_digits * interval_sum(l,r);
    }
    else {
        sum += l_digits * interval_sum(l,pow(10,l_digits)-1);
        sum += r_digits * interval_sum(pow(10,r_digits-1),r);
        for (int i = l_digits + 1; i < r_digits; i++) {
            sum += i * interval_sum(pow(10,i-1),pow(10,i)-1);
        }
    }

    cout << sum.val() << endl;
}
