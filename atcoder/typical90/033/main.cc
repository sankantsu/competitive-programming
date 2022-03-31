#include <iostream>

using namespace std;

int h,w;

int main() {
    cin >> h >> w;
    if (h == 1 || w == 1) {
        cout << h*w << endl;
    }
    else {
        int x = (h+1)/2;
        int y = (w+1)/2;
        cout << (x*y) << endl;
    }
}
