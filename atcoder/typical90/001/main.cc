#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

constexpr int inf = pow(10,9);

// check if given cost can be achieved
template <typename Vec>
bool cut_yokan(const int c, int k, int l, const Vec &a) {
    int last = 0; // last cut position
    int cnt = 0;  // cut count
    for (size_t i = 0; i < a.size(); i++) {
        if (a[i]-last >= c) {
            cnt++;
            last = a[i];
            if (cnt >= k) {
                break;
            }
        }
    }
    if (cnt >= k && l - last >= c) {
        return true;
    }
    else {
        return false;
    }
}

int main() {
    int n,l;
    cin >> n >> l;
    int k;
    cin >> k;
    vector<int> a(n);
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }

    // binary search
    int min = 0;
    int max = inf;
    int c;
    while (max - min > 1) {
        c = (min + max)/2;
        bool res = cut_yokan(c,k,l,a);
        if (res) {
            min = c;
        }
        else {
            max = c;
        }
    }
    cout << min << endl;
    
    return 0;
}
