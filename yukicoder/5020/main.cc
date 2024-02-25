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

auto gen_random_ans(int n) {
    constexpr int m = 50;
    std::vector<Entry> ans;
    for (int i = 0; i < m; i++) {
        int u, v;
        while (true) {
            u = mt() % n;
            v = mt() % n;
            if (u != v) {
                break;
            }
        }
        ans.emplace_back(u,v);
    }
    return ans;
}

auto greedy_ans(int n) {
    constexpr int m = 50;
    std::vector<Entry> ans;
    long best_score = calc_score(ans, a, b);
    std::cerr << "initial score: " << best_score << std::endl;
    for (int i = 0; i < m; i++) {
        /* std::cerr << i << " th iter" << std::endl; */
        int u = 0;
        int best_v = -1;
        for (int v = 1; v < n; v++) {
            ans.emplace_back(u,v);
            long score = calc_score(ans, a, b);
            /* std::cerr << "v, score: " << v << ", " << score << std::endl; */
            if (score > best_score) {
                best_score = score;
                best_v = v;
            }
            ans.pop_back();
        }
        /* std::cerr << "best v, score: " << best_v << ", " << best_score << std::endl; */
        if (best_v == -1) {
            break;
        }
        else {
            ans.emplace_back(u,best_v);
        }
    }
    return ans;
}

auto add_three_swaps(Solution& ans) {
    constexpr int m = 50;
    long best_score = calc_score(ans, a, b);
    for (int u = 1; u < n-1; u++) {
        for (int v = u+1; v < n; v++) {
            for (int v2 = v+1; v2 < n; v2++) {
                if (ans.size() >= m - 2) break;
                ans.push_back(Entry{u,v});
                ans.push_back(Entry{u,v2});
                ans.push_back(Entry{0,u});
                long score = calc_score(ans, a, b);
                if (score > best_score) {
                    best_score = score;
                }
                else {
                    ans.pop_back();
                    ans.pop_back();
                    ans.pop_back();
                }
            }
        }
    }
    return ans;
}

auto greedy_iter_three_swaps(int n) {
    Solution ans;
    while (ans.size() <= max_swap - 3) {
        auto size = ans.size();
        add_three_swaps(ans);
        if (size == ans.size()) {  // no pair inserted
            break;
        }
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
    const double end_temp = 100;
    /* const long max_iter = 50000000; */
    /* for (long iter = 0; iter < max_iter; iter++) { */
    /*     double temp = start_temp + (end_temp - start_temp) * (static_cast<double>(iter) / max_iter); */
    while (true) {
        auto time = std::chrono::steady_clock::now();
        auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(time - start_time).count();
        if (elapsed > duration) {
            break;
        }
        double temp = start_temp + (end_temp - start_temp) * (static_cast<double>(elapsed) / duration);
        int r = mt() % 2;
        // add a random entry
        if (r == 0) {
            if (ans.size() == m) continue;
            int u = mt() % n;
            int v = mt() % n;
            if (u == v) continue;
            int pos = mt() % (ans.size() + 1);
            ans.insert(ans.begin() + pos, Entry{u,v});
            long score = calc_score(ans, a, b);
            double ratio = std::exp((score - best_score) / temp);
            double dice = uniform(mt);
            if (dice < ratio) {
                /* std::cerr << "insert (" << u << ", " << v << ")" << " at " << pos << std::endl; */
                /* std::cerr << "score: " << best_score << " -> " << score << std::endl; */
                best_score = score;
            }
            else {
                ans.erase(ans.begin() + pos);
            }
        }
        // remove a random entry
        if (r == 1) {
            if (ans.size() == 0) continue;
            int pos = mt() % ans.size();
            auto [u,v] = ans[pos];
            ans.erase(ans.begin() + pos);
            long score = calc_score(ans, a, b);
            double ratio = std::exp((score - best_score) / temp);
            double dice = uniform(mt);
            if (dice < ratio) {
                /* std::cerr << "Delete (" << u << ", " << v << ")" << " at " << pos << std::endl; */
                /* std::cerr << "score: " << best_score << " -> " << score << std::endl; */
                best_score = score;
            }
            else {
                ans.insert(ans.begin() + pos, Entry{u,v});
            }
        }
        // replace an entry with random value
        /* if (r == _) { */
        /*     if (ans.size() == 0) continue; */
        /*     int pos = mt() % ans.size(); */
        /*     int new_u = mt() % n; */
        /*     int new_v = mt() % n; */
        /*     auto [u,v] = ans[pos]; */
        /*     ans[pos] = Entry{new_u, new_v}; */
        /*     long score = calc_score(ans, a, b); */
        /*     double ratio = std::exp((score - best_score) / temp); */
        /*     double dice = uniform(mt); */
        /*     if (dice < ratio) { */
        /*         best_score = score; */
        /*     } */
        /*     else { */
        /*         ans[pos] = Entry{u,v}; */
        /*     } */
        /* } */
        // swap random two entries
        /* if (r == _) { */
        /*     if (ans.size() == 0) continue; */
        /*     auto i = mt() % ans.size(); */
        /*     auto j = mt() % ans.size(); */
        /*     if (i == j) continue; */
        /*     std::swap(ans[i], ans[j]); */
        /*     long score = calc_score(ans, a, b); */
        /*     double ratio = std::exp((score - best_score) / temp); */
        /*     double dice = uniform(mt); */
        /*     if (dice < ratio) { */
        /*         best_score = score; */
        /*     } */
        /*     else { */
        /*         std::swap(ans[i], ans[j]); */
        /*     } */
        /* } */
    }
}

int main() {
    std::cin >> n;

    a.resize(n);
    b.resize(n);
    for (int i = 0; i < n; i++) {
        std::cin >> a[i] >> b[i];
    }

    auto ans = greedy_iter_three_swaps(n);
    std::cerr << "-----------------" << std::endl;
    std::cerr << "greedy: ";
    debug_print_ans(ans);
    std::cerr << "-----------------" << std::endl;
    annealing(ans);
    std::cerr << "annealing: ";
    debug_print_ans(ans);
    std::cerr << "-----------------" << std::endl;
    std::cerr << "Final score: " << calc_score(ans, a, b) << std::endl;
    print_ans(ans);
}
