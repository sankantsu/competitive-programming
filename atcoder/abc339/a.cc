#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    string s;
    cin >> s;

    auto pos = s.rfind(".");
    cout << s.substr(pos+1) << endl;
}
