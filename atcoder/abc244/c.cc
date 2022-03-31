#include <iostream>
#include <vector>
#include <cstdio>

using namespace std;

int main() {
    int n;
    cin >> n;

    vector<bool> v(2*n+1,false);
    int num = 0; // 実際に出す数 - 1
    for (int i = 0; i <= n; i++) {
        while (v[num]) {
            num++;
        }
        v[num] = true;
        cout << (num + 1) << endl;
        fflush(stdout);
        int num2; // 青木君が出す数
        cin >> num2;
        if (num2 == 0) {
            return 0;
        }
        v[num2-1] = true;
    }
}
