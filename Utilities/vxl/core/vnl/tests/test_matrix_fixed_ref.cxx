// This is core/vnl/tests/test_matrix_fixed_ref.cxx
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_matrix_fixed_ref.h>
#include <vnl/vnl_vector_fixed.h>

#include <vcl_algorithm.h> // for vcl_generate()
#include <vcl_cstdlib.h> // for vcl_rand()
#include <testlib/testlib_test.h>

void test_matrix_fixed_ref()
{
  enum{rows = 3};
  enum{cols = 4};
  typedef vnl_matrix_fixed<double,rows,cols> mf;
  typedef vnl_matrix_fixed_ref<double,rows,cols> mfr;
  typedef vnl_matrix_fixed_ref_const<double,rows,cols> mfrc;

  int i,j;
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
    vcl_generate(new_row.begin(),new_row.end(),vcl_rand);

    ref.set_row(i,new_row);
    vnl_vector_fixed<double,cols> row_copy = mat.get_row(i);
    TEST("set_row", new_row, row_copy);
  }
  //    set_col
  for (j=0;j<cols;++j)
  {
    vnl_vector_fixed<double,rows> new_col;
    vcl_generate(new_col.begin(),new_col.end(),vcl_rand);

    ref.set_column(j,new_col);
    vnl_vector_fixed<double,rows> col_copy = mat.get_column(j);
    TEST("set_col", new_col, col_copy);
  }

  //    assign from mat
  mf other;
  vcl_generate(other.begin(),other.end(),vcl_rand);
#if 0 // cannot assign to a vnl_matrix_fixed_ref_const
  ref = other;
  TEST("assign_mf", ref, other);
  // test different adresses
  TEST("assign_mf", (ref.begin() != other.begin()), true);
#endif // 0

  {
  //    assign from const mfr
  vcl_generate(other.begin(),other.end(),vcl_rand);
  mfrc cref(other);
  ref = cref;
  TEST("assign_const_ref", ref, other);
  // test different adresses
  TEST("assign_const_ref", (ref.begin() != other.begin()), true);
  }

  {
#if 0 // cannot assign to a vnl_matrix_fixed_ref_const
  //    assign from mfr
  vcl_generate(other.begin(),other.end(),vcl_rand);
  mfr ref2(other);
  ref = ref2;
  TEST("assign_ref", ref, other);
  // test different adresses
  TEST("assign_ref", (ref.begin() != other.begin()), true);
#endif // 0
  }
  // arithmetic
  {
    // plus
    mf a,b;
    vcl_generate(a.begin(),a.end(),vcl_rand);
    vcl_generate(b.begin(),b.end(),vcl_rand);
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
    vcl_generate(a.begin(),a.end(),vcl_rand);
    vcl_generate(b.begin(),b.end(),vcl_rand);
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
