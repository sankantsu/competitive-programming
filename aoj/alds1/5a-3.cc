// recursion O(Q 2^N) AC

#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n, q;
int a[21];
int m[201];
bool ans[201];

bool rec(int i, int m) {
    if (i == n) {
        if (m == 0) {
            return true;
        }
        else {
            return false;
        }
    }
    bool res = rec(i+1,m) || rec(i+1,m-a[i]);
    return res;
}

int main() {
    cin >> n
    for (int i = 0; i < q; i++) cin >> m[i];

    for (int i = 0; i < q; i++) {
        int mi = m[i];
        ans[i] = rec(0,mi);
    }
    for (int i = 0; i < q; i++) {
        if (ans[i]) {
            cout << "yes" << endl;
        }
        else {
            cout << "no" << endl;
        }
    }
}
