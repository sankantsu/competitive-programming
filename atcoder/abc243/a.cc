#include <iostream>

using namespace std;

int solve(int v, int a, int b, int c) {
    while (true) {
        if (v < a) { // father
            return 0;
        }
        v -= a;
        if (v < b) { // mother
            return 1;
        }
        v -= b;
        if (v < c) { // takahashi
            return 2;
        }
        v -= c;
    }
}

int main() {
    int v,a,b,c;
    cin >> v >> a >> b >> c;

    int ans = solve(v,a,b,c);
    if (ans == 0) {
        cout << "F" << endl;
    }
    else if (ans == 1) {
        cout << "M" << endl;
    }
    else if (ans == 2) {
        cout << "T" << endl;
    }
}
