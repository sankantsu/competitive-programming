#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    int n;
    cin >> n;
    cout << "L";
    rep(i,n) {
        cout << "o";
    }
    cout << "ng" << endl;
}
