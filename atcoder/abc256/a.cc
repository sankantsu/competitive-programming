#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long pow(long a, long b) {
    if (b == 0) return 1;
    return a*pow(a,b-1);
}

int main() {
    long n;
    cin >> n;
    cout << pow(2,n) << endl;
}
