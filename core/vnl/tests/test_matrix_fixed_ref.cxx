// This is core/vnl/tests/test_matrix_fixed_ref.cxx
#include <algorithm>
#include <cstdlib>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_matrix_fixed_ref.h>
#include <vnl/vnl_vector_fixed.h>

#include <vcl_compiler.h>
#include <testlib/testlib_test.h>

void test_matrix_fixed_ref()
{
  enum{rows = 3};
  enum{cols = 4};
  typedef vnl_matrix_fixed<double,rows,cols> mf;
  typedef vnl_matrix_fixed_ref<double,rows,cols> mfr;
  typedef vnl_matrix_fixed_ref_const<double,rows,cols> mfrc;

  unsigned int i,j;
  mf mat; // copy in
  for (i=0;i<rows;++i)
    for (j=0;j<cols;++j)
      mat(i,j) = 10 * i + j;

  // matrix fixed_ref tests


  // fixed_ref_const
  const mf & cmf = mat;
  mfrc cref(cmf);
  // check address
  for (i=0;i<rows;++i)
  {
    for (j=0;j<cols;++j)
    {
      TEST("const_address",&cref(i,j),&mat(i,j));
    }
  }

  // wrap around const mf
  //    get_row
  for (i=0;i<rows;++i)
  {
    vnl_vector_fixed<double,cols> row_copy = cmf.get_row(i);
    vnl_vector_fixed<double,cols> row_copy2 = mat.get_row(i);
    TEST("get_row", row_copy,row_copy2);
  }
  //    get_col
  for (j=0;j<cols;++j)
  {
    vnl_vector_fixed<double,rows> col_copy = cmf.get_column(j);
    vnl_vector_fixed<double,rows> col_copy2 = mat.get_column(j);
    TEST("get_column", col_copy,col_copy2);
  }
  //    get_diagonal
  vnl_vector_fixed<double,3> v(0,11,22);
  TEST("get_diagonal()", cmf.get_diagonal(), v);

  // fixed_ref (non-const)
  // wrap around mat
  mfr ref(mat);
  // check address
  for (i=0;i<rows;++i)
  {
    for (j=0;j<cols;++j)
    {
      TEST("nonconst_address",&ref(i,j),&mat(i,j));
    }
  }
  //    set_row
  for (i=0;i<rows;++i)
  {
    vnl_vector_fixed<double,cols> new_row;
    std::generate(new_row.begin(),new_row.end(),std::rand);

    ref.set_row(i,new_row);
    vnl_vector_fixed<double,cols> row_copy = mat.get_row(i);
    TEST("set_row", new_row, row_copy);
  }
  //    set_col
  for (j=0;j<cols;++j)
  {
    vnl_vector_fixed<double,rows> new_col;
    std::generate(new_col.begin(),new_col.end(),std::rand);

    ref.set_column(j,new_col);
    vnl_vector_fixed<double,rows> col_copy = mat.get_column(j);
    TEST("set_col", new_col, col_copy);
  }
  //   set diagonal
  ref.set_diagonal(vnl_vector_fixed<double,3>(16,7,9));
  TEST("set_diagonal(7,9,16))",ref(0,0)==16 && ref(1,1)==7 && ref(2,2)==9, true);

  //    assign from mat
  mf other;
  std::generate(other.begin(),other.end(),std::rand);
  {
    //    assign from const mfr
    std::generate(other.begin(),other.end(),std::rand);
    mfrc cref(other);
    ref = cref;
    TEST("assign_const_ref", ref, other);
    // test different addresses
    TEST("assign_const_ref", (ref.begin() != other.begin()), true);
  }

  // arithmetic
  {
    // plus
    mf a,b;
    std::generate(a.begin(),a.end(),std::rand);
    std::generate(b.begin(),b.end(),std::rand);
    mfrc arefc(a), brefc(b);
    mf mc = arefc + brefc;

    mfr aref(a), bref(b);
    mf m = aref + bref;

    mf m2 = arefc + bref;
    mf m3 = arefc + brefc;
    TEST("plus", mc, m);
    TEST("plus", mc, m2);
    TEST("plus", mc, m3);
  }
  {
    // times
    mf a,b;
    std::generate(a.begin(),a.end(),std::rand);
    std::generate(b.begin(),b.end(),std::rand);
    mfrc arefc(a), brefc(b);
    mf mc = arefc + brefc;

    mfr aref(a), bref(b);
    mf m = aref + bref;

    mf m2 = arefc + bref;
    mf m3 = arefc + brefc;
    TEST("plus", mc, m);
    TEST("plus", mc, m2);
    TEST("plus", mc, m3);
  }
}

TESTMAIN(test_matrix_fixed_ref);
