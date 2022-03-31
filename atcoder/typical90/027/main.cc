#include <iostream>
#include <set>
#include <string>

using namespace std;

int n;
set<string> s;

int main() {
    cin >> n;
    for (int i = 1; i <= n; i++) {
        string str;
        cin >> str;
        if (s.find(str) == s.end()) {
            s.insert(move(str));
            cout << i << endl;
        }
    }
}
