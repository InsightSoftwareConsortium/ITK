// This is core/vnl/tests/test_sym_matrix.cxx
#include <vcl_iostream.h>
#include <vnl/vnl_sym_matrix.h>
#include <testlib/testlib_test.h>

static
void test_int()
{
  vcl_cout << "*****************************\n"
           << "Testing Symmetric Matrix<int>\n"
           << "*****************************\n";
  vnl_sym_matrix<int> sm1(2);
  TEST("\n\nvnl_sym_matrix<int> m1(2)", (sm1.rows()==2 && sm1.columns()==2), true);
  vnl_sym_matrix<int> sm2(2,2);
  TEST("\n\nvnl_sym_matrix<int> sm2(2,2)",
       (sm2(0,0)==2 && sm2(0,1)==2 && sm2(1,0)==2 && sm2(1,1)==2), true);
  const vnl_matrix<int> ma1(2, 2, 3);
  const vnl_sym_matrix<int> sm3(ma1);
  TEST("\n\n(const vnl_sym_matrix) sm3",
       (sm3(0,0)==3 && sm3(0,1)==3 && sm3(1,0)==3 && sm3(1,1)==3), true);
  vcl_cout << "sm3\n" << sm3 <<vcl_endl << vcl_endl;

  int td[] = {1, 2, 3};
  vnl_sym_matrix<int> sm4(td, 2);
  vcl_cout << "sm4\n" << sm4 << vcl_endl << vcl_endl;
  vnl_matrix<int>  ma2 = sm4.as_matrix();
  TEST("(const vnl_matrix) ma2",
       (ma2(0,0)==1 && ma2(0,1)==2 && ma2(1,0)==2 && ma2(1,1)==3), true);
  vcl_cout << "ma2\n" << ma2 << vcl_endl << vcl_endl;
  TEST("operator== ", ma2==sm4 && !(ma2==sm3), true);
  vcl_cout << "sm3\n" << sm3 << vcl_endl << vcl_endl;

  int td5[] = {0, 0, 0};
  vnl_sym_matrix<int> sm5(td5, 2);
  swap(sm5, sm4);
  TEST("swap",
       (sm4(0,0)==0 && sm4(0,1)==0 && sm4(1,0)==0 && sm4(1,1)==0) &&
       (sm5(0,0)==1 && sm5(0,1)==2 && sm5(1,0)==2 && sm5(1,1)==3), true);

  vnl_sym_matrix<int> sm6(3, 0);
  sm6.update(sm5,1);
  TEST("update",
       (sm6(0,0)==0 && sm6(0,1)==0 && sm6(0,2)==0 &&
        sm6(1,0)==0 && sm6(1,1)==1 && sm6(1,2)==2 &&
        sm6(2,0)==0 && sm6(2,1)==2 && sm6(2,2)==3), true);
  vcl_cout << "sm6\n" << sm6 << vcl_endl << vcl_endl;

  sm5 = sm6;
  TEST("operator =",
       (sm5(0,0)==0 && sm5(0,1)==0 && sm5(0,2)==0 &&
        sm5(1,0)==0 && sm5(1,1)==1 && sm5(1,2)==2 &&
        sm5(2,0)==0 && sm5(2,1)==2 && sm5(2,2)==3), true);
  vcl_cout << "sm5\n" << sm5 << vcl_endl << vcl_endl;

  TEST("operator ==",
       (sm5==sm6 && !(sm5==sm4) && !(sm4==sm3)), true);
  vcl_cout << "sm4 sm3\n" << sm4 << vcl_endl << sm3 << vcl_endl << vcl_endl;

  vnl_vector<int> v1(2,5);
  sm5.set_half_row(v1, 1);
  TEST("set_half_row",
       (sm5(0,0)==0 && sm5(0,1)==5 && sm5(0,2)==0 &&
        sm5(1,0)==5 && sm5(1,1)==5 && sm5(1,2)==2 &&
        sm5(2,0)==0 && sm5(2,1)==2 && sm5(2,2)==3), true);
  vcl_cout << "sm5\n" << sm5 << vcl_endl << vcl_endl;
}


static
void test_sym_matrix()
{
  test_int();
}

TESTMAIN(test_sym_matrix);
