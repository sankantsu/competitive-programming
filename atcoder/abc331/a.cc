#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int main() {
    int M, D;
    cin >> M >> D;
    
    int y, m, d;
    cin >> y >> m >> d;

    int ny, nm, nd;
    if (d < D) {
        ny = y;
        nm = m;
        nd = d + 1;
    }
    else if (m < M) {
        ny = y;
        nm = m + 1;
        nd = 1;
    }
    else {
        ny = y + 1;
        nm = 1;
        nd = 1;
    }
    cout << ny << " " << nm << " " << nd << endl;
}
