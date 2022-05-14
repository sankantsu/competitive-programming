#include <iostream>
#include <vector>

using namespace std;

int main() {
    long w;
    cin >> w;

    vector<long> v;
    for (long i = 1; i < 100; i++) {
        v.push_back(i);
        v.push_back(i*100);
        v.push_back(i*10000);
    }
    v.push_back(1000000);

    cout << v.size() << endl;
    for (auto x : v) cout << x << " ";
    cout << endl;
}
