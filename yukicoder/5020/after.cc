#include <iostream>
#include <vector>
#include <random>
#include <chrono>
#include <cmath>

constexpr int max_swap = 50;
static int n;
static std::mt19937 mt(0);
static std::uniform_real_distribution<> uniform(0.,1.);
static std::vector<long> a, b;

using Entry = std::pair<int, int>;
using Solution = std::vector<Entry>;

long calc_score(const Solution& ans, const std::vector<long>& a, const std::vector<long>& b) {
    std::vector<long> aa = a;
    std::vector<long> bb = b;
    for (auto [u,v] : ans) {
        long v1 = (aa[u] + aa[v])/2;
        long v2 = (bb[u] + bb[v])/2;
        aa[u] = v1;
        aa[v] = v1;
        bb[u] = v2;
        bb[v] = v2;
    }
    double v1 = std::abs(aa[0] - 5*1e17);
    double v2 = std::abs(bb[0] - 5*1e17);
    long score = 2000000L - 100000L * std::log10(std::max(v1, v2) + 1);
    return score;
}

void print_ans(const Solution& ans) {
    std::cout << ans.size() << std::endl;
    for (auto [u,v] : ans) {
        std::cout << u+1 << " " << v+1 << std::endl;
    }
}

void debug_print_ans(const Solution& ans) {
    for (auto [u,v] : ans) {
        std::cerr << "(" << u << "," << v << ") ";
    }
    std::cerr << "score = " << calc_score(ans, a, b) << std::endl;
}

auto initial_ans(int n) {
    constexpr int m = 50;
    std::vector<Entry> ans;
    for (int i = 1; i < n; i++) {
        ans.push_back(Entry{0,i});
    }
    return ans;
}

void annealing(Solution& ans) {
    std::cerr << "annealing()" << std::endl;
    constexpr int m = 50;
    long best_score = calc_score(ans, a, b);
    auto start_time = std::chrono::steady_clock::now();
    const double duration = 950;  // msec
    const double start_temp = 10000;
    const double end_temp = 1000;
    while (true) {
        auto time = std::chrono::steady_clock::now();
        auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(time - start_time).count();
        if (elapsed > duration) {
            break;
        }
        double temp = start_temp + (end_temp - start_temp) * (static_cast<double>(elapsed) / duration);
        // swap random two entries
        if (ans.size() == 0) continue;
        auto i = mt() % ans.size();
        auto j = mt() % ans.size();
        if (i == j) continue;
        std::swap(ans[i], ans[j]);
        long score = calc_score(ans, a, b);
        double ratio = std::exp((score - best_score) / temp);
        double dice = uniform(mt);
        if (dice < ratio) {
            best_score = score;
        }
        else {
            std::swap(ans[i], ans[j]);
        }
    }
}

int main() {
    std::cin >> n;

    a.resize(n);
    b.resize(n);
    for (int i = 0; i < n; i++) {
        std::cin >> a[i] >> b[i];
    }

    auto ans = initial_ans(n);
    std::cerr << "-----------------" << std::endl;
    std::cerr << "initial: ";
    debug_print_ans(ans);
    std::cerr << "-----------------" << std::endl;
    annealing(ans);
    std::cerr << "annealing: ";
    debug_print_ans(ans);
    std::cerr << "-----------------" << std::endl;
    std::cerr << "Final score: " << calc_score(ans, a, b) << std::endl;
    print_ans(ans);
}
