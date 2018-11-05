// This is vcl/tests/test_vector.cxx
#include <iostream>
#include <vector>
#include <algorithm>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

std::ostream &delim(std::ostream &os)
{
  //return os << endl;
  return os << ", ";
}

int frurk(std::vector<int> const &a,
          std::vector<int> const &b)
{
  if (a == b)
    return 0;
  if (a != b)
    return 1;
  return 2;
}

int test_vector_main(int /*argc*/,char* /*argv*/[])
{
  bool fail = false;
  {
    typedef std::vector<int> container;
    container m;

    m.push_back(1);
    m.push_back(2);

    for (int & p : m)
      std::cout << p << std::endl;
  }
  {
    std::vector<double> v;
    for (unsigned i=0; i<10; ++i)
    {
      std::cout << "size : " << v.size() << delim
               << "capacity : " << v.capacity() << delim;
      if (i>0)
        std::cout << "begin : " << (void*) &* v.begin()
                 << delim << "end - 1: " << (void*) &* (v.end() - 1) << std::endl;
      else
        std::cout << std::endl;

      v.push_back(13.141592653589793 * i);
    }
  }
  {
      std::vector<bool> bv(2);
      bv[0] = true;
      bv[1] = false;
      std::nth_element(bv.begin(), bv.begin()+1, bv.end());
  }
  { // check contiguity
#define macro(T) do { \
    std::vector<T > v; \
    for (int i=0; i<5; ++i) v.push_back(T(i)); \
    bool ok = true; \
    for (unsigned int i=1; i<v.size(); ++i) { T *p = &v[i-1]; T *q = &v[i]; if (p + 1 != q) ok = false; } \
    if (ok) std::cout << "PASS: vector<" << #T << "> has contiguous storage\n"; \
    else  { std::cout << "FAIL: vector<" << #T << "> has non-contiguous storage\n"; fail = true; } \
} while (false)
    macro(char);
    macro(int);
    macro(double);
#undef macro
  }

  return fail ? 1 : 0;
}
