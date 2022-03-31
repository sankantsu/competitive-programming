
#include <iostream>

using namespace std;

int main() {
    int a,b,c,x;
    cin >> a >> b >> c >> x;

    double ans;
    if (x <= a) {
        ans = 1;
    }
    else if (b < x) {
        ans = 0;
    }
    else {
        ans = (double)c/(b - a);
    }

    cout << ans << endl;
}
