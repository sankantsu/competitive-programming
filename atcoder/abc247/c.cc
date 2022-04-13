#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;
string s[20];

int main() {
    cin >> n;
    s[1] = "1";
    for (int i = 2; i <= n; i++) {
        string t = to_string(i);
        s[i] = s[i-1] + " " + t + " " + s[i-1];
    }
    cout << s[n] << endl;
}
