// The purpose of this test is to verify that the compiler can resolve
// template instantiation of a specialised function.
// (At the time of writing, the Mac clang compiler is not able to do so.)

template <class T>
class vcl_test_vec
{
  T* data_;
  int size_;
 public:
  vcl_test_vec(int s=1) : size_(s) { data_ = new T[s]; }
  ~vcl_test_vec() { delete[] data_; }
  int size() const { return size_; }
};

template <class T>
struct vcl_test_type_templ
{
  typedef vcl_test_vec<T> type;
};

template <class T>
class vcl_test_typename
{
 public:
  typedef typename vcl_test_type_templ<T>::type vector;
  vector v;

  vcl_test_typename() { if (vcl_test_typename_func(v) < 1) v = 1; }
};

inline int vcl_test_typename_func(float /*v*/) { return 1; }
inline int vcl_test_typename_func(double /*v*/) { return 1; }
template <class T> inline int vcl_test_typename_func(vcl_test_vec<T> const& v) { return v.size(); }

int test_typename_main(int /*argc*/,char* /*argv*/[])
{
  return 0;
}
