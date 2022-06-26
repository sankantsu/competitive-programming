#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    int n,x;
    cin >> n >> x;
    int k = (x-1)/n;
    char c = 'A'+k;
    cout << c << endl;
}
