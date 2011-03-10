#include <testlib/testlib_test.h>
#include <vnl/vnl_crs_index.h>
#include <vcl_iostream.h>


void display_mask(const vcl_vector<vcl_vector<bool> >& mask)
{
  for (unsigned int i=0; i<mask.size(); ++i) {
    for (unsigned int j=0; j<mask[i].size(); ++j) {
      vcl_cout << (mask[i][j]?'1':'0') << ' ';
    }
    vcl_cout << '\n';
  }
  vcl_cout << vcl_endl;
}

static void test_crs_index()
{
  vcl_vector<bool> null_col(8,false);
  vcl_vector<vcl_vector<bool> > mask(10,null_col);

  mask[0][1] = true;
  mask[0][2] = true;
  mask[0][4] = true;
  mask[2][0] = true;
  mask[2][1] = true;
  mask[2][7] = true;
  mask[3][4] = true;
  mask[4][3] = true;
  mask[4][6] = true;
  mask[6][0] = true;
  mask[6][2] = true;
  mask[6][3] = true;
  mask[6][4] = true;
  mask[6][6] = true;
  mask[6][7] = true;
  mask[7][7] = true;
  mask[9][6] = true;

  display_mask(mask);

  vnl_crs_index crs(mask);

  TEST("num rows",crs.num_rows(),(int)mask.size());
  TEST("num cols",crs.num_cols(),(int)mask[0].size());
  TEST("num non-zero",crs.num_non_zero(),17);

  // look up each index and check it's validity
  bool valid = true;
  int curr = 0;
  for (int i=0; i<crs.num_rows(); ++i) {
    for (int j=0; j<crs.num_cols(); ++j) {
      int idx = crs(i,j);
      if (((idx<0) == mask[i][j]) && idx!=curr++)
        valid = false;
      vcl_cout << crs(i,j) << ' ';
    }
    vcl_cout << vcl_endl;
  }
  TEST("operator ()",valid,true);

  // test column extraction
  vnl_crs_index::sparse_vector col = crs.sparse_col(1);
  valid = true;
  for (unsigned int i=0; i<col.size(); ++i) {
    if (col[i].first != crs(col[i].second,1))
      valid = false;
  }
  TEST("sparse_col",col.size() == 2 && valid, true);
  col = crs.sparse_col(5);
  TEST("empty sparse_col",col.size(),0);

  // test row extraction
  vnl_crs_index::sparse_vector row = crs.sparse_row(6);
  valid = true;
  for (unsigned int i=0; i<row.size(); ++i) {
    if (row[i].first != crs(6,row[i].second))
      valid = false;
  }
  TEST("sparse_row", row.size() == 6 && valid, true);
  row = crs.sparse_row(5);
  TEST("empty sparse_row", row.size(), 0);
}

TESTMAIN(test_crs_index);
