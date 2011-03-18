#include <testlib/testlib_test.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_vector_ref.h>
#include <vnl/vnl_matrix_ref.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>

#include <vcl_cassert.h>
#include <vcl_iostream.h>


// --- dynamic ------------------------------

#define NewMat(mat, r,c,data) \
   assert( sizeof(data) >= r*c*sizeof(double) ); \
   vnl_matrix<double> mat( data, r, c )
#define NewVec(vec, n,data) \
   assert( sizeof(data) >= n*sizeof(double) ); \
   vnl_vector<double> vec( data, n )

static
void
test_arithmetic_dynamic()
{
#include "test_arithmetic_body.h"
}

#undef NewMat
#undef NewVec


// --- ref ----------------------------------

#define NewMat(mat, r,c,data) \
   assert( sizeof(data) >= r*c*sizeof(double) ); \
   vnl_matrix_ref<double> mat( r, c, data )
#define NewVec(vec, n,data) \
   assert( sizeof(data) >= n*sizeof(double) ); \
   vnl_vector_ref<double> vec( n, data )

static
void
test_arithmetic_ref()
{
#include "test_arithmetic_body.h"
}


// --- fixed --------------------------------

#undef NewMat
#undef NewVec

#define NewMat(mat, r,c,data) \
   assert( sizeof(data) >= r*c*sizeof(double) ); \
   vnl_matrix_fixed<double,r,c> mat( data )
#define NewVec(vec, n,data) \
   assert( sizeof(data) >= n*sizeof(double) ); \
   vnl_vector_fixed<double,n> vec( data )

void
test_arithmetic_fixed()
{
#include "test_arithmetic_body.h"
}

#undef NewMat
#undef NewVec

void test_arithmetic()
{
  vcl_cout << "---- dynamic ----\n";
  test_arithmetic_dynamic();
  vcl_cout << "---- reference ----\n";
  test_arithmetic_ref();
  vcl_cout << "---- fixed ----\n";
  test_arithmetic_fixed();
}

TESTMAIN( test_arithmetic );
