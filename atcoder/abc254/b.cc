#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

void print(vector<int>& v) {
    for (auto x : v) {
        cout << x << " ";
    }
    cout << endl;
}

int main() {
    int n;
    cin >> n;
    vector<int> v = {1};
    for (int i = 2; i <= n+1; i++) {
        print(v);
        if (i == n+1) break;
        vector<int> next(i);
        for (int j = 0; j < i; j++) {
            if (j == 0 || j == i-1) {
                next[j] = 1;
            }
            else {
                next[j] = v[j-1] + v[j];
            }
        }
        v = move(next);
    }
}
