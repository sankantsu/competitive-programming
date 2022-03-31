#include <iostream>
#include <vector>

using namespace std;

int main() {
    vector<char> s(3);
    vector<char> t(3);

    for (int i = 0; i < 3; i++) cin >> s[i];
    for (int i = 0; i < 3; i++) cin >> t[i];

    int cnt = 0;
    // 1番目から順に一致させるように交換
    for (int i = 0; i < 3; i++) {
        if (s[i] == t[i]) {
            continue;
        }
        for (int j = i+1; j < 3; j++) {
            if (t[i] == s[j]) {
                char tmp = s[j];
                s[j] = s[i];
                s[i] = tmp;
                cnt++;
                break;
            }
        }
    }
    if (cnt%2 == 0) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
