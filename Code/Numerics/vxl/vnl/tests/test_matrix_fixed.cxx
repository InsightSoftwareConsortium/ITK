#include <vcl_new.h>
#include <vcl_cstdlib.h>
#include <vcl_iostream.h>

#include <vnl/vnl_test.h>

#include <vnl/vnl_double_3x3.h>
#include <vnl/vnl_double_3.h>
#include <vnl/vnl_linear_operators_3.h>

bool verbose_malloc = false;
int malloc_count = 0;

void testvnl_matrix_fixed()
{
  verbose_malloc = true;
  double datablock[9] = {
    11, 12, 13,
    21, 22, 23,
    31, 32, 33,
  };
  
  vcl_cout << "Calling ctor -- should be no mallocs\n";
  //Refvnl_double_3x3 X(datablock);
  malloc_count = 0;
  vnl_double_3x3 X(datablock);
  vcl_cout << "X = [" << X << "]\n";

  vnl_double_3 v(10,11,12);
  vcl_cout << "v = [ " << v << "]\n";
  
  vcl_cout << "X v = " << X * (v + v) << vcl_endl;
  // This shouldn't compile...
  // vnl_matrix<double>* base = new vnl_double_3x3(datablock);

  // FIXME: Win32 will have different operator new in vnl dll from
  // the one generated here, so this test fails - RWMC.
#ifndef WIN32
  vnl_test_assert("mallocs", malloc_count <= 1);
#endif
  
  vcl_cout << "Now watch them mallocs\n";
  vnl_matrix<double>& CX = X;
  vnl_vector<double>& cv = v;
  vcl_cout << "X v = " << CX * (cv + cv) << vcl_endl;
  
  verbose_malloc = false;  
}


void* operator new(size_t s)
  // [18.4.1] lib.new.delete
#if defined(VCL_SUNPRO_CC_50) || defined(GNU_LIBSTDCXX_V3) || defined(VCL_KAI)
  throw (std::bad_alloc)
#endif
{
  void *r = malloc(s);
  
  ++malloc_count;
  
  if (verbose_malloc) 
    vcl_cout << "malloc: " << r << " for " << s << vcl_endl;
 
  return r;
}
 
void operator delete(void* s)
#if defined(GNU_LIBSTDCXX_V3) || defined(VCL_SUNPRO_CC_50)
  throw ()
#endif
{
  if (verbose_malloc)
    vcl_cout << "delete: " << s << vcl_endl;
  free(s);
}

TESTMAIN(testvnl_matrix_fixed);
