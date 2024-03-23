#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long t;
    cin >> t;

    rep(_,t) {
        long n;
        cin >> n;
        string x, y;
        cin >> x >> y;

        // trick to reduce cases
        x.push_back('C');
        y.push_back('C');

        vector<size_t> cs;
        rep(i,n+1) if (y[i] == 'C') cs.push_back(i);
        /* cerr << "cs.size: " << cs.size() << endl; */

        bool check = true;
        for (auto k : cs) {
            /* cerr << "k: " << k << endl; */
            if (x[k] != 'C') {
                check = false;
                break;
            }
            size_t nbx = 0, nby = 0;
            rep(i,k) if (x[i] == 'B') nbx++;
            rep(i,k) if (y[i] == 'B') nby++;
            for (long i = k - 1; i >= 0; i--) {
                if (x[i] == 'C') {
                    if (nbx < nby) {
                        x[i] = 'B';
                        nbx++;
                    }
                    else x[i] = 'A';
                }
            }
            if (nbx != nby) {
                check = false;
                break;
            }

            vector<size_t> pbx, pby;
            rep(i,k) if (x[i] == 'B') pbx.push_back(i);
            rep(i,k) if (y[i] == 'B') pby.push_back(i);
            /* cerr << "size: " << pbx.size() << endl; */
            assert(pbx.size() == pby.size());
            rep(i,pbx.size()) {
                /* cerr << "pbx, pby: " << pbx[i] << " " << pby[i] << endl; */
                if (pbx[i] < pby[i]) {  // cannot move 'B' to right
                    check = false;
                    break;
                }
            }
            if (!check) break;
        }
        if (check) {
            cout << "Yes" << endl;
        }
        else {
            cout << "No" << endl;
        }
    }
}
