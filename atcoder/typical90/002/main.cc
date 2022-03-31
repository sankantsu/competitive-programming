#include <iostream>

using namespace std;

constexpr int max_n = 20;

int n;
char buf[max_n];

void print_buf() {
    for (int i = 0; i < n; i++) {
        cout << buf[i];
    }
    cout << endl;
}

void print_rec(int i,int cnt) {
    if (i == n) {
        if (cnt == 0) {
            print_buf();
        }
        return;
    }
    buf[i] = '(';
    print_rec(i+1,cnt+1);
    if (cnt > 0) {
        buf[i] = ')';
        print_rec(i+1,cnt-1);
    }
}

int main() {
    cin >> n;
    print_rec(0,0);
}
