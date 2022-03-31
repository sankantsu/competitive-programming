#include <iostream>

using namespace std;

constexpr int max_n = 20;

int n;

void print_parens(int k) {
    for (int i = 0; i < n; i++) {
        if ((k>>(n-1-i)) & 0x1)
            cout << ')';
        else
            cout << '(';
    }
    cout << endl;
}

bool is_correct(int k) {
    int cnt = 0;
    for (int i = 0; i < n; i++) {
        if ((k>>(n-1-i)) & 0x1)
            cnt--;
        else
            cnt++;
        if (cnt < 0) {
            return false;
        }
    }
    if (cnt == 0) {
        return true;
    }
    else {
        return false;
    }
}

int main() {
    cin >> n;
    for (int k = 0; k < 1<<n; k++) {
        if(is_correct(k)) {
            print_parens(k);
        }
    }
}
