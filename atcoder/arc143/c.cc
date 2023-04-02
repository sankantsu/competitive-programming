#include <iostream>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

long n,x,y;
long a[200000];

int main() {
    cin >> n >> x >> y;
    rep(i,n) cin >> a[i];

    rep(i,n) {
        a[i] = a[i]%(x+y);
    }

    bool win_first = false;
    rep(i,n) {
        if (x <= a[i] && a[i] < y) { // only first player can take
            win_first = true;
            break;
        }
        else if (y <= a[i] && a[i] < x) { // only second player can take
            win_first = false;
            break;
        }
        else {
            if (a[i] < x && a[i] < y) continue; // both player cannot take
            else if (x <= a[i] && y <= a[i]) {  // both player can take
                win_first = true;
            }
            else {
                assert(false);
            }
        }
    }
    if (win_first) {
        cout << "First" << endl;
    }
    else {
        cout << "Second" << endl;
    }
}
