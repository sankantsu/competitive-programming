#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n;

int count_factors(int x) {
    int cnt = 0;
    for (int i = 1; i*i <= x; i++) {
        if (x%i == 0) {
            cnt++;
            if (i*i != x) cnt++;
        }
    }
    return cnt;
}

int main() {
    cin >> n;
    int cnt = 0;
    for (int i = 1; i <= n; i++) {
        if (i%2 == 0) continue;
        if (count_factors(i) == 8) cnt++;
    }
    cout << cnt << endl;
}
