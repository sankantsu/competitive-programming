#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

using namespace std;

string s;

int count_acgt(int i, int j) {
    string T("ACGT");
    for (int k = i; k <= j; k++) {
        char c = s[k];
        if (T.find(c) == string::npos) {
            return 0;
        }
    }
    return j-i+1;
}

int main() {
    cin >> s;
    int res = -1;
    for (int i = 0; i < s.size(); i++) {
        for (int j = i; j < s.size(); j++) {
            res = max(res,count_acgt(i,j));
        }
    }
    cout << res << endl;
}
