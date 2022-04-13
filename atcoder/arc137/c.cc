#include <iostream>
#include <vector>

using namespace std;

int main() {
    int n;
    cin >> n;
    vector<int> a(n);
    for (int i = 0; i < n; i++) cin >> a[i];
    if (a[n-1] - a[n-2] > 1) {
        cout << "Alice" << endl;
    }
    else {
        if ((a[n-1] - (n-1)) % 2 == 0) {
            cout << "Bob" << endl;
        }
        else {
            cout << "Alice" << endl;
        }
    }
}
