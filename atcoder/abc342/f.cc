#include <iostream>
#include <vector>
#include <algorithm>

#define rep(i,n) for (long long i = 0; i < static_cast<long long>((n)); i++)

using namespace std;

int main() {
    long n, l, d;
    cin >> n >> l >> d;

    // count dealder's state
    vector<double> dp(3*n);
    vector<double> sum(3*n);
    dp[0] = 1.;
    sum[0] = 1.;
    for (long i = 1; i < l; i++) {
        dp[i] += sum[i-1]/d;
        if (i-d-1 >= 0) {
            dp[i] -= sum[i-d-1]/d;
        }
        sum[i] = sum[i-1] + dp[i];
    }
    for (long i = l; i < l+d; i++) {
        dp[i] += sum[l-1]/d;
        if (i-d-1 >= 0) {
            dp[i] -= sum[i-d-1]/d;
        }
    }
    double dealer_total = 0;
    for (long i = l; i < 3*n; i++) {
        dealer_total += dp[i];
    }
    double dealer_burst = 0;
    for (long i = n+1; i < 3*n; i++) {
        dealer_burst += dp[i];
    }
    
    vector<double> stay_win_prob(n+1);
    vector<double> stay_win_prob_sum(n+1);
    stay_win_prob[n] = dealer_total - dp[n];
    stay_win_prob_sum[n] = stay_win_prob[n];
    long should_stay = n;
    for (long i = n - 1; i > n-d; i--) {
        if (i >= l) {
            stay_win_prob[i] = stay_win_prob[i+1] - dp[i];
        }
        else {
            stay_win_prob[i] = stay_win_prob[i+1];
        }
        stay_win_prob_sum[i] = stay_win_prob_sum[i+1] + stay_win_prob[i];
        double stay_win = stay_win_prob[i];
        double draw_win = stay_win_prob_sum[i]/d;
        if (stay_win >= draw_win) {
            should_stay = i;
        }
        else {
            break;
        }
    }

    // count player's state
    fill(dp.begin(), dp.end(), 0.);
    fill(sum.begin(), sum.end(), 0.);
    dp[0] = 1.;
    sum[0] = 1.;
    for (long i = 1; i < should_stay; i++) {
        dp[i] += sum[i-1]/d;
        if (i-d-1 >= 0) {
            dp[i] -= sum[i-d-1]/d;
        }
        sum[i] = sum[i-1] + dp[i];
    }
    for (long i = should_stay; i < should_stay+d; i++) {
        dp[i] += sum[should_stay-1]/d;
        if (i-d-1 >= 0) {
            dp[i] -= sum[i-d-1]/d;
        }
    }
    double player_total = 0;
    for (long i = should_stay; i < 3*n; i++) {
        player_total += dp[i];
    }

    double player_win_prob = 0;
    for (long i = should_stay; i <= n; i++) {
        player_win_prob += dp[i]*stay_win_prob[i];
    }

    cerr << "should_stay: " << should_stay << endl;
    cerr << "dealer_burst: " << dealer_burst << endl;
    cerr << "win_prob: " << player_win_prob << endl;
    cerr << "dealer_total: " << dealer_total << endl;
    cerr << "player_total: " << player_total << endl;
    double ans = static_cast<double>(player_win_prob)/(player_total * dealer_total);
    cout << ans << endl;
}
