// Compiler support <> in friend template ?
#ifdef TEST_GDCM_CXX_HAS_NULL_TEMPLATE_ARGS
template <class T> class A;
template <class T> int f(A<T>&);
template <class T> class A
{
public:
  // "friend int f<>(A<T>&)" would conform
  friend int f(A<T>&);
private:
  int x;
};

template <class T> int f(A<T>& a) { return a.x = 0; }
template int f(A<int>&);

int main()
{
  A<int> a;
  return f(a);
}
#endif

// Test for particular structure
#ifdef TEST_GDCM_STAT_HAS_ST_MTIM
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
int main()
{
  struct stat stat1;
  (void)stat1.st_mtim.tv_sec;
  (void)stat1.st_mtim.tv_nsec;
  return 0;
}
#endif

// 64 bits compilers
#ifdef TEST_GDCM_CXX_SAME_LONG_AND___INT64
void function(long**) {}
int main()
{
  __int64** p = 0;
  function(p);
  return 0;
}
#endif

// Not all compilers support long long
#ifdef TEST_GDCM_CXX_SAME_LONG_LONG_AND___INT64
void function(long long**) {}
int main()
{
  __int64** p = 0;
  function(p);
  return 0;
}
#endif

// Cast problem on some compilers
#ifdef TEST_GDCM_CAN_CONVERT_UI64_TO_DOUBLE
void function(double& l, unsigned __int64 const& r)
{
  l = static_cast<double>(r);
}

int main()
{
  double tTo = 0.0;
  unsigned __int64 tFrom = 0;
  function(tTo, tFrom);
  return 0;
}
#endif

// Some compilers defines char as unsigned type. Indeed char, signed char and unisgned char are three completely different types
#ifdef TEST_GDCM_CHAR_IS_SIGNED
/* Return 1 for char signed and 0 for char unsigned.  */
int main()
{
  unsigned char uc = 255;
  return (*reinterpret_cast<char*>(&uc) < 0)?1:0;
}
#endif

#ifdef TEST_GDCM_CXX_HAS_FUNCTION
// Minimal test for existence of __FUNCTION__ pseudo-macro
#include <string.h>

int TestFUNCTION()
{
#ifdef __BORLANDC__
  #ifndef __FUNCTION__
    #define __FUNCTION__ __FUNC__
  #endif
#endif
  const char *f = __FUNCTION__;
  int r = strcmp( "TestFUNCTION", f);
  return r;
}

int main()
{
  return TestFUNCTION();
}

#endif
