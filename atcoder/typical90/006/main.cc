#include <iostream>
#include <string>

using namespace std;

int n,k;
string s;
int c[100000][26];

int main() {
    cin >> n >> k;
    cin >> s;

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < 26; j++) {
            c[i][j] = 100000;
        }
    }

    c[n-1][s[n-1]-'a'] = n-1;
    for (int i = n-2; i >= 0; i--) {
        int ch = s[i]-'a';
        for (int j = 0; j < 26; j++) {
            if (j == ch) {
                c[i][j] = i;
            }
            else {
                c[i][j] = c[i+1][j];
            }
        }
    }

    string t;
    int cur = 0;
    for (int i = 0; i < k; i++) {
        for (int j = 0; j < 26; j++) {
            if (c[cur][j] <= n-k+i) {
                t.push_back(static_cast<char>('a'+j));
                cur = c[cur][j]+1;
                break;
            }
        }
    }
    
    cout << t << endl;
}
