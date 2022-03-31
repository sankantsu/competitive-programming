#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>

using namespace std;

template <typename Vector>
bool solve(int n, Vector v, const string &s) {
    unordered_map<int,int> right_min; // 右を向いている中で一番左の人 (y -> x)
    unordered_map<int,int> left_max; // 左を向いている中で一番右の人
    for (int i = 0; i < n; i++) {
        int x = v[i].first;
        int y = v[i].second;
        if (s[i] == 'R') {
            if (right_min.find(y) == right_min.end() || x < right_min[y]) {
                right_min[y] = x;
            }
        }
        else { // s[i] == "L"
            if (left_max.find(y) == left_max.end()|| x > left_max[y]) {
                left_max[y] = x;
            }
        }
    }
    for (auto r : right_min) {
        int y = r.first;
        int min = r.second;
        if (left_max.find(y) != left_max.end() && min < left_max[y]) {
            return true;
        }
    }
    return false;
}

int main() {
    int n;
    cin >> n;

    vector<pair<int,int>> v;
    for (int i = 0; i < n; i++) {
        int x,y;
        cin >> x >> y;
        v.push_back(make_pair(x,y));
    }

    string s;
    cin >> s;

    bool ans = solve(n,v,s);
    
    if (ans) {
        cout << "Yes" << endl;
    }
    else {
        cout << "No" << endl;
    }
}
