#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    string s;
    cin >> s;

    string t = s;
    sort(t.begin(), t.end());
    if (s != t) {
        cout << "No" << endl;
    }
    else {
        cout << "Yes" << endl;
    }
}
