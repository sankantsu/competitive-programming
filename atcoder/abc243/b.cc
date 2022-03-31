#include <iostream>
#include <vector>

using namespace std;

template <typename Vector>
void solve(int n, Vector a, Vector b) {
    int ans1 = 0;
    int ans2 = 0;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (a[i] == b[j]) {
                if (i == j) {
                    ans1++;
                }
                else {
                    ans2++;
                }
            }
        }
    }
    cout << ans1 << endl;
    cout << ans2 << endl;
}

int main() {
    int n;
    cin >> n;

    vector<int> a(n),b(n);
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }
    for (int i = 0; i < n; i++) {
        cin >> b[i];
    }

    solve(n,a,b);
}
