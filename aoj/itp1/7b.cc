#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n,x;

int main() {
    while (true) {
        cin >> n >> x;
        if (n == 0 && x == 0) break;
        int cnt = 0;
        for (int i = 1; i <= n; i++) {
            for (int j = i+1; j <= n; j++) {
                for (int k = j+1; k <= n; k++) {
                    if (i+j+k == x) cnt++;
                }
            }
        }
        cout << cnt << endl;
    }
}
