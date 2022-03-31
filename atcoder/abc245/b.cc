#include <iostream>
#include <vector>

using namespace std;

int n;

int main() {
    cin >> n;
    vector<int> a(n);
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }
    vector<bool> used(2001);
    for (int i = 0; i < n; i++) {
        used[a[i]] = true;
    }
    int ans = -1;
    for (int x = 0; x <= 2000; x++) {
        if (used[x] == false) {
            ans = x;
            break;
        }
    }
    cout << ans << endl;
}
