#include <iostream>
#include <string>

using namespace std;

int main() {
    const int n = 1500;

    cout << n << endl;
    int ax = 750;
    int ay = 750;
    int bx = 1;
    int by = 1;
    cout << ax << " " << ay << endl;
    cout << bx << " " << by << endl;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i == 1 && j == 1) cout << '#';
            cout << '.';
        }
        cout << endl;
    }
}
