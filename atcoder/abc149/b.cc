#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

long a,b,k;

int main() {
    cin >> a >> b >> k;
    long res1, res2;
    if (k < a) {
        res1 = a-k;
        res2 = b;
    }
    else if (k < a+b) {
        res1 = 0;
        res2 = b-(k-a);
    }
    else {
        res1 = 0;
        res2 = 0;
    }
    cout << res1 << " " << res2 << endl;
}
