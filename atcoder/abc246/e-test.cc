#include <iostream>
#include <random>

using namespace std;

int main(int argc, char **argv) {
    int n = stoi(argv[1]);

    random_device rnd;
    mt19937 mt(rnd());
    cout << n << endl;
    int ax = mt()%n;
    int ay = mt()%n;
    int bx = ax;
    int by = ay;
    while ((ax==bx) && (ay==by)) {
        bx = mt()%n;
        by = mt()%n;
    }
    cout << ax << " " << ay << endl;
    cout << bx << " " << by << endl;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i == ax && j == ay) {
                cout << ".";
            }
            else if (i == bx && j == by) {
                cout << ".";
            }
            else if (mt()%10 == 0) {
                cout << "#";
            }
            else {
                cout << ".";
            }
        }
        cout << endl;
    }
}
