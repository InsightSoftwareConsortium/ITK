// This is vcl/tests/test_vector.cxx
#include <vcl_iostream.h>
#include <vcl_vector.h>
#include <vcl_algorithm.h>

vcl_ostream &delim(vcl_ostream &os)
{
  //return os << endl;
  return os << ", ";
}

int frurk(vcl_vector<int> const &a,
          vcl_vector<int> const &b)
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
    typedef vcl_vector<int> container;
    container m;

    m.push_back(1);
    m.push_back(2);

    for (container::iterator p = m.begin(); p != m.end(); ++p)
      vcl_cout << (*p) << vcl_endl;
  }
  {
    vcl_vector<double> v;
    for (unsigned i=0; i<10; ++i)
    {
      vcl_cout << "size : " << v.size() << delim
               << "capacity : " << v.capacity() << delim;
      if (i>0)
        vcl_cout << "begin : " << (void*) &* v.begin()
                 << delim << "end - 1: " << (void*) &* (v.end() - 1) << vcl_endl;
      else
        vcl_cout << vcl_endl;

      v.push_back(13.141592653589793 * i);
    }
  }
  {
      vcl_vector<bool> bv(2);
      bv[0] = true;
      bv[1] = false;
      vcl_nth_element(bv.begin(), bv.begin()+1, bv.end());
  }
  { // check contiguity
#define macro(T) do { \
    vcl_vector<T > v; \
    for (int i=0; i<5; ++i) v.push_back(T(i)); \
    bool ok = true; \
    for (unsigned int i=1; i<v.size(); ++i) { T *p = &v[i-1]; T *q = &v[i]; if (p + 1 != q) ok = false; } \
    if (ok) vcl_cout << "PASS: vector<" << #T << "> has contiguous storage\n"; \
    else  { vcl_cout << "FAIL: vector<" << #T << "> has non-contiguous storage\n"; fail = true; } \
} while (false)
    macro(char);
    macro(int);
    macro(double);
#undef macro
  }

  return fail ? 1 : 0;
}
