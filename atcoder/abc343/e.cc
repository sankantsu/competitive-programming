#include <iostream>
#include <algorithm>
#include <map>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

using Problem = std::tuple<long, long, long>;

struct Solution {
    long a1;
    long b1;
    long c1;
    long a2;
    long b2;
    long c2;
    long a3;
    long b3;
    long c3;
};

map<Problem, Solution> solutions;

auto to_xyz(size_t pos) {
    long a = pos/(15*15) - 7;
    long b = (pos - (a + 7)*15*15)/15 - 7;
    long c = (pos - (a + 7)*15*15 - (b + 7)*15) - 7;
    return make_tuple(a, b, c);
}

auto len_three_stack(long a, long b, long c) {
    long lb = max(a, max(b, c));
    long ub = min(a, min(b, c)) + 7;
    return max(0L, ub - lb);
}

auto len_two_stack(long a, long b) {
    long lb = max(a, b);
    long ub = min(a, b) + 7;
    return max(0L, ub - lb);
}

auto solve(size_t p1, size_t p2) {
    auto [a1, b1, c1] = to_xyz(p1);
    auto [a2, b2, c2] = to_xyz(p2);

    long v3 = len_three_stack(0, a1, a2) * len_three_stack(0, b1, b2) * len_three_stack(0, c1, c2);
    long v2 = 0;
    v2 += len_two_stack(0, a1) * len_two_stack(0, b1) * len_two_stack(0, c1);
    v2 += len_two_stack(a1, a2) * len_two_stack(b1, b2) * len_two_stack(c1, c2);
    v2 += len_two_stack(0, a2) * len_two_stack(0, b2) * len_two_stack(0, c2);
    v2 -= 3*v3;
    long v1 = 3*7*7*7 - 2*v2 - 3*v3;
    return make_tuple(v1, v2, v3);
}

void solve_all() {
    constexpr size_t n_pos = 15*15*15;
    rep(p1, n_pos) {
        rep(p2, n_pos) {
            auto [v1, v2, v3] = solve(p1, p2);
            auto [a1, b1, c1] = to_xyz(p1);
            auto [a2, b2, c2] = to_xyz(p2);
            Solution s = {0, 0, 0, a1, b1, c1, a2, b2, c2};

            Problem ans {v1, v2, v3};
            if (solutions.find(ans) == solutions.end()) {
                solutions[ans] = s;
            }
        }
    }
}

int main() {
    solve_all();

    long v1, v2, v3;
    cin >> v1 >> v2 >> v3;

    Problem p {v1, v2, v3};
    if (solutions.find(p) != solutions.end()) {
        auto [a1, b1, c1, a2, b2, c2, a3, b3, c3] = solutions[p];
        cout << "Yes" << endl;
        cout << a1 << " " << b1 << " " << c1 << " " << a2 << " " << b2 << " " << c2 << " " << a3 << " " << b3 << " " << c3 << endl;
    }
    else {
        cout << "No" << endl;
    }
}
