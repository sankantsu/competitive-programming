#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

long n;

int main() {
    cin >> n;
    long sum = 0;
    for (long i = 1; i < n; i++) {
        sum += i;
    }
    cout << sum << endl;
}
