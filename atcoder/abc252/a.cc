#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int n;

int main() {
    cin >> n;
    char c = 'a' + (n-97);
    cout << c << endl;
}
