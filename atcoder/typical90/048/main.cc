#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int n,k;

int main() {
    cin >> n >> k;
    vector<long> scores;
    for (int i = 0; i < n; i++) {
        long a,b;
        cin >> a >> b;
        scores.push_back(b);
        scores.push_back(a-b);
    }
    sort(scores.begin(),scores.end(),greater<long>{});
    long sum = 0;
    for (int i = 0; i < k; i++) {
        sum += scores[i];
    }
    cout << sum << endl;
}
