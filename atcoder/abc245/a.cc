#include <iostream>

using namespace std;

int a,b,c,d;

int main() {
    cin >> a >> b >> c >> d;
    bool ans = false;
    if (a < c) {
        ans = true;
    }
    else if (a == c && b <= d) {
        ans = true;
    }
    if (ans) {
        cout << "Takahashi" << endl;
    }
    else {
        cout << "Aoki" << endl;
    }
}
