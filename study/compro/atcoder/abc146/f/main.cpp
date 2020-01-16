#include <bits/stdc++.h>

#define rep(i, n) for (int i = 0; i < int(n); i++)
#define rrep(i, n) for (int i = int(n) - 1; i >= 0; i--)
#define reps(i, n) for (int i = 1; i <= int(n); i++)
#define rreps(i, n) for (int i = int(n); i >= 1; i--)
#define repc(i, n) for (int i = 0; i <= int(n); i++)
#define rrepc(i, n) for (int i = int(n); i >= 0; i--)
#define repi(i, a, b) for (int i = int(a); i < int(b); i++)
#define repic(i, a, b) for (int i = int(a); i <= int(b); i++)
#define each(x, y) for (auto &x : y)
#define all(a) (a).begin(), (a).end()
#define bit(b) (1ll << (b))

using namespace std;

using i32 = int;
using i64 = long long;
using u64 = unsigned long long;
using f80 = long double;
using vi32 = vector<i32>;
using vi64 = vector<i64>;
using vu64 = vector<u64>;
using vf80 = vector<f80>;
using vstr = vector<string>;

inline void yes() { cout << "Yes" << '\n'; exit(0); }
inline void no() { cout << "No" << '\n'; exit(0); }
inline i64 gcd(i64 a, i64 b) { if (min(a, b) == 0) return max(a, b); if (a % b == 0) return b; return gcd(b, a % b); }
inline i64 lcm(i64 a, i64 b) { return a / gcd(a, b) * b; }
inline u64 xorshift() { static u64 x = 88172645463325252ull; x = x ^ (x << 7); return x = x ^ (x >> 9); }
void solve(); int main() { ios::sync_with_stdio(0); cin.tie(0); cout << fixed << setprecision(16); solve(); return 0; }
template <typename T> class pqasc : public priority_queue<T, vector<T>, greater<T>> {};
template <typename T> class pqdesc : public priority_queue<T, vector<T>, less<T>> {};
template <typename T> inline void amax(T &x, T y) { if (x < y) x = y; }
template <typename T> inline void amin(T &x, T y) { if (x > y) x = y; }
template <typename T> inline T power(T x, i64 n, T e = 1) { T r = e; while (n > 0) { if (n & 1) r *= x; x *= x; n >>= 1; } return r; }
template <typename T> istream& operator>>(istream &is, vector<T> &v) { each(x, v) is >> x; return is; }
template <typename T> ostream& operator<<(ostream &os, vector<T> &v) { rep(i, v.size()) { if (i) os << ' '; os << v[i]; } return os; }
template <typename T, typename S> istream& operator>>(istream &is, pair<T, S> &p) { is >> p.first >> p.second; return is; }
template <typename T, typename S> ostream& operator<<(ostream &os, pair<T, S> &p) { os << p.first << ' ' << p.second; return os; }

template <typename T>
struct SegmentTree {
  using F = function<T(T, T)>;
  vector<T> v;
  F f;
  T e;
  int n;
  SegmentTree(int size, F f, T e) : f(f), e(e) {
    n = 1;
    while (n < size) n <<= 1;
    v.resize(n * 2, e);
  }
  void set(int k, T x) {
    v[k + n] = x;
  }
  void build() {
    for (int i = n - 1; i > 0; i--) {
      v[i] = f(v[i * 2 + 0], v[i * 2 + 1]);
    }
  }
  void update(int k, T x) {
    v[k += n] = x;
    while (k >>= 1) v[k] = f(v[k * 2 + 0], v[k * 2 + 1]);
  }
  T query(int a, int b) {
    T l = e, r = e;
    for (a += n, b += n; a < b; a >>= 1, b >>= 1) {
      if (a & 1) l = f(l, v[a++]);
      if (b & 1) r = f(v[--b], r);
    }
    return f(l, r);
  }
};

void solve() {
  int n, m;
  cin >> n >> m;
  string s;
  cin >> s;
  reverse(all(s));
  SegmentTree<int> st(n + 1, [](int x, int y) { return min(x, y); }, 1e9);
  st.update(0, 0);
  reps(i, n) {
    if (s[i] == '1') continue;
    int mi = st.query(max(i - m, 0), i);
    if (mi == 1e9) continue;
    st.update(i, mi + 1);
  }
  if (st.query(n, n + 1) == 1e9) {
    cout << -1 << endl;
    return;
  }
  vi32 ans;
  int pos = n;
  int now = st.query(n, n + 1);
  rrep(i, n) {
    int s = st.query(i, i + 1);
    if (s != now && s != 1e9) {
      ans.push_back(pos - i);
      now--;
      pos = i;
    }
  }
  cout << ans << endl;
}
