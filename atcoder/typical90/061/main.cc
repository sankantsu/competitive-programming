#include <iostream>
#include <deque>

using namespace std;

int q;

int main() {
    cin >> q;

    deque<int> deq;
    for (int i = 0; i < q; i++) {
        int t,x;
        cin >> t >> x;
        if (t == 1) {
            deq.push_front(x);
        }
        else if (t == 2) {
            deq.push_back(x);
        }
        else if (t == 3) {
            cout << deq[x-1] << endl;
        }
        else {
            throw;
        }
    }
}
