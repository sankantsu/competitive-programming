#include <iostream>
#include <vector>
#include <set>
#include <algorithm>

using namespace std;

constexpr int max_n = 200000;

int n,m;
int a[max_n];
int b[max_n];
int c[max_n];
int d[max_n];

struct item {
    int type;
    int height;
    int width;
};

int main() {
    cin >> n >> m;
    for (int i = 0; i < n; i++) cin >> a[i];
    for (int i = 0; i < n; i++) cin >> b[i];
    for (int i = 0; i < m; i++) cin >> c[i];
    for (int i = 0; i < m; i++) cin >> d[i];

    auto comp = [](item lhs, item rhs) {
        if (lhs.height == rhs.height) {
            return lhs.type > rhs.type;
        }
        else {
            return lhs.height > rhs.height;
        }
    };

    vector<item> all;
    // chocolates
    for (int i = 0; i < n; i++) {
        item it;
        it.type = 0;
        it.height = a[i];
        it.width = b[i];
        all.push_back(it);
    }
    // boxes
    for (int i = 0; i < m; i++) {
        item it;
        it.type = 1;
        it.height = c[i];
        it.width = d[i];
        all.push_back(it);
    }

    sort(all.begin(),all.end(),comp);

    /* cout << "all:" << endl; */
    /* for (int i = 0; i < all.size(); i++) { */
    /*     item it = all[i]; */
    /*     cout << it.type << " " << it.height << " " << it.width << endl; */
    /* } */

    bool ans = true;
    multiset<int> ms;
    for (int i = 0; i < all.size(); i++) {
        item it = all[i];
        // box
        if (it.type == 1) {
            // cout << "box" << endl;
            ms.insert(it.width);
        }
        // chocolate
        else {
            // cout << "choco" << endl;
            auto iter = ms.lower_bound(it.width);
            if (iter == ms.end()) {
                ans = false;
                break;
            }
            ms.erase(iter);
        }
    }
    
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
