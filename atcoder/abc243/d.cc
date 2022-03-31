#include <iostream>
#include <deque>
#include <string>

using namespace std;

auto make_bits(long x) {
    deque<int> bits; // x のbit表現
    while (x > 0) {
        bits.push_front(x & 0x1);
        x >>= 1;
    }
    return bits;
}

template <typename Container>
long bits_to_long(const Container &bits) {
    long x = 0;
    for (auto b : bits) {
        x <<= 1;
        x += b;
    }
    return x;
}

long solve(long x, const string &s) {
    auto bits = make_bits(x);
    for (auto c : s) {
        if (c == 'U') {
            bits.pop_back();
        }
        if (c == 'L') {
            bits.push_back(0);
        }
        if (c == 'R') {
            bits.push_back(1);
        }
    }
    long ans = bits_to_long(bits);
    return ans;
}

int main() {
    int n;
    long x;
    cin >> n >> x;

    string s;
    cin >> s;

    long ans = solve(x,s);

    cout << ans << endl;
}
