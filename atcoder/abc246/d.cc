#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

long n;

long fun(long a, long b) {
    return a*a*a + a*a*b + a*b*b + b*b*b;
}

bool check(long x) {
    for (long i = 0; i*i*i <= x; i++) {
        for (long j = max(i,(long)pow((double)x,(double)1/3)-i); i*i*i + j*j*j <= x; j++) {
            if (fun(i,j) == x) {
                return true;
            }
        }
    }
    return false;
}

int main() {
    cin >> n;
    while (!check(n)) {
        n++;
    }
    cout << n << endl;
}
