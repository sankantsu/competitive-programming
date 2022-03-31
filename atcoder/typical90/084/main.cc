#include <iostream>
#include <vector>
#include <string>

using namespace std;

int n;
string s;

int main() {
    cin >> n;
    cin >> s;

    vector<long> change;
    change.push_back(0);

    char c = s[0];
    for (int i = 1; i < n; i++) {
        if (s[i] != c) {
            change.push_back(i);
            c = s[i];
        }
    }
    /* cout << "change: " << endl; */
    /* for (int i = 0; i < change.size(); i++) { */
    /*     cout << change[i] << " "; */
    /* } */
    /* cout << endl; */

    long sum = 0;
    for (int i = 1; i < static_cast<int>(change.size()); i++) {
        sum += (change[i] - change[i-1]) * (n - change[i]);
    }

    cout << sum << endl;
}
