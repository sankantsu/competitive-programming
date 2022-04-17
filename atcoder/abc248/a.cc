#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

string s;
int flag[10];

int main() {
    cin >> s;
    rep(i,9) {
        flag[s[i]-'0'] = true;
    }
    rep(i,10) {
        if (!flag[i]) {
            cout << i << endl;
        }
    }
}
