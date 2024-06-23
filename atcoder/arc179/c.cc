#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <cassert>
#include <set>

#define rep(i,n) for (long i = 0; i < static_cast<long>((n)); i++)

using namespace std;

int query_order(int i, int j) {
    int q;
    cout << "? " << i << " " << j << endl;
    cin >> q;
    if (q == -1) {
        exit(1);
    }
    return q;
}

int query_add(int i, int j) {
    int q;
    cout << "+ " << i << " " << j << endl;
    cin >> q;
    if (q == -1) {
        exit(1);
    }
    return q;
}

void quick_sort(vector<int>& idx, int l, int r) {
    if (r - l <= 1) {
        return;
    }
    int pivot = idx[l];
    set<int> smaller;
    set<int> larger;
    smaller.insert(pivot);
    for (int j = l + 1; j < r; j++) {
        int q = query_order(pivot, idx[j]);
        if (q == 0) {
            smaller.insert(idx[j]);
        } else {
            larger.insert(idx[j]);
        }
    }
    int j = 0;
    for (auto id : smaller) {
        idx[l+j] = id;
        j++;
    }
    for (auto id : larger) {
        idx[l+j] = id;
        j++;
    }
    int m = smaller.size();
    quick_sort(idx, 0, m);
    quick_sort(idx, m, r);
}

int main() {
    int n;
    cin >> n;

    vector<int> idx(n);
    rep(i,n) cin >> idx[i];

    int k = 0;
    while (n > 1) {
        quick_sort(idx, 0, n);
        rep(i, n/2) {
            query_add(idx[i], idx[n - 1 - i]);
        }
        k += n;
        n /= 2;
        rep(i,n) {
            idx[i] = k + i;
        }
    }
    cout << "!" << endl;
}
