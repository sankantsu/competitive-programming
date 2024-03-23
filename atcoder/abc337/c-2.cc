
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>
#include <set>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n;
    cin >> n;

    vector<int> a(n);
    rep(i,n) {
        cin >> a[i];
        if (a[i] > 0) a[i]--;
    }

    int front;
    vector<int> b(n);

    rep(i,n) {
        if (a[i] == -1) {
            front = i;
        }
        else {
            b[a[i]] = i;
        }
    }

    rep(i,n) {
        cout << front + 1 << " ";
        front = b[front];
    }
    cout << endl;
}
