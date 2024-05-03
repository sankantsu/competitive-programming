#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

struct S {
    int i, j, k;
};

S nx(S x) {
    if (x.k < x.j) {
        return S{x.i, x.j, x.k+1};
    }
    else if (x.j < x.i) {
        return S{x.i, x.j+1, 1};
    }
    else {
        return S{x.i+1, 1, 1};
    }
}

string to_string(S x) {
    string s(x.i, '1');
    rep(j,x.j) s[j] = '2';
    rep(k,x.k) s[k] = '3';
    reverse(s.begin(), s.end());
    return s;
}

int main() {
    int n;
    cin >> n;

    const int max_n = 333;

    std::vector<S> vs;
    vs.push_back(S{1,1,1});

    rep(i,max_n) {
        vs.push_back(nx(vs.back()));
    }

    cout << to_string(vs[n-1]) << endl;
}
