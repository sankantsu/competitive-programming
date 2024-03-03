#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    string s;
    cin >> s;

    bool check = true;
    rep(i,s.size()) {
        bool cond = (i == 0 && isupper(s[i])) || (i > 0 && islower(s[i]));
        check = check && cond;
    }
    if (check) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
