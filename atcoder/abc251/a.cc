#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

string s;

int main() {
    cin >> s;
    int n = s.size();
    rep(i,6/n) {
        cout << s;
    }
    cout << endl;
}
