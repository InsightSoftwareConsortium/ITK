// This is core/vnl/tests/test_matrix.cxx
#include <vcl_iostream.h>
#include <vnl/vnl_matrix.h>
#include <testlib/testlib_test.h>
#include <vcl_cmath.h> // sqrt()

static
void test_int()
{
  vcl_cout << "***********************\n"
           << "Testing vnl_matrix<int>\n"
           << "***********************\n";
  vnl_matrix<int> m0(2,2);
  TEST("vnl_matrix<int> m0(2,2)", (m0.rows()==2 && m0.columns()==2), true);
  vnl_matrix<int> m1(3,4);
  TEST("vnl_matrix<int> m1(3,4)", (m1.rows()==3 && m1.columns()==4), true);
  vnl_matrix<int> m2(2,2,2);
  TEST("vnl_matrix<int> m2(2,2,2)",
       (m2.get(0,0)==2 && m2.get(0,1)==2 && m2.get(1,0)==2 && m2.get(1,1)==2), true);
  TEST("m2 = vnl_matrix<int>(2,2, 2)",
       (m2 = vnl_matrix<int>(2,2, 2),
        (m2.get(0,0)==2 && m2.get(0,1)==2 && m2.get(1,0)==2 && m2.get(1,1)==2)), true);
  const vnl_matrix<int> ma = m2;
  TEST("(const vnl_matrix)(i,j)",
       (ma(0,0)==2 && ma(0,1)==2 && ma(1,0)==2 && ma(1,1)==2), true);
  vnl_matrix<int> mb = m2;
  TEST("(vnl_matrix)(i,j)",
       (mb(0,0) = 0,
        mb(0,0)==0 && mb(0,1)==2 && mb(1,0)==2 && mb(1,1)==2), true);
  int mcvalues[4] = {1, 2, 3};
  vnl_matrix<int> mc(2,2, 4, mcvalues);
  TEST("vnl_matrix<int> mc(2,2, 4,int[])",
       (mc(0,0)==1 && mc(0,1)==2 && mc(1,0)==3 && mc(1,1)==0), true);
  TEST("m0=2",
       (m0=2,
        (m0.get(0,0)==2 && m0.get(0,1)==2 && m0.get(1,0)==2 && m0.get(1,1)==2)), true);
  TEST("m0 == m2", (m0 == m2), true);
  TEST("(m0 == m2)", (m0 == m2), true);
  TEST("m2.put(1,1,3)", (m2.put(1,1,3),m2.get(1,1)), 3);
  TEST("m2.get(1,1)", m2.get(1,1), 3);
  TEST("m0 == m2", (m0 == m2), false);
  TEST("m0 != m2", (m0 != m2), true);
  TEST("(m0 == m2)", (m0 == m2), false);
  TEST("m1.fill(3)",
       (m1.fill(3),
        (m1.get(0,0)==3 && m1.get(1,1)==3 && m1.get(2,2)==3 && m1.get(2,3)==3)), true);
  TEST("m2.fill(2)",
       (m2.fill(2),
        (m2.get(0,0)==2 && m2.get(0,1)==2 && m2.get(1,0)==2 && m2.get(1,1)==2)), true);
  int m3values [] = {1,2,3};
  vnl_matrix<int> m3(1,3,3, m3values);
  TEST("m3(1,3,3,{1,2,3})",
       (m3.get(0,0)==1 && m3.get(0,1)==2 && m3.get(0,2)==3), true);
  vnl_matrix<int> m4(m3);
  TEST("vnl_matrix<int> m4(m3)", (m3==m4), true);
  TEST("m0=m2", (m0=m2, (m0==m2)), true);

  // test additions and subtractions
  TEST("m0=m2+3",
       ((m0=m2+3),
        (m0.get(0,0)==5 && m0.get(0,1)==5 && m0.get(1,0)==5 && m0.get(1,1)==5)), true);
  TEST("m0=3+m2",
       ((m0=3+m2),
        (m0.get(0,0)==5 && m0.get(0,1)==5 && m0.get(1,0)==5 && m0.get(1,1)==5)), true);
  TEST("m0+=(-3)",
       (m0+=(-3),
        (m0.get(0,0)==2 && m0.get(0,1)==2 && m0.get(1,0)==2 && m0.get(1,1)==2)), true);
  TEST("m0-=(-3)",
       (m0-=(-3),
        (m0.get(0,0)==5 && m0.get(0,1)==5 && m0.get(1,0)==5 && m0.get(1,1)==5)), true);
  TEST("m0=m2-3",
       ((m0=m2-3),
        (m0.get(0,0)==-1 && m0.get(0,1)==-1 && m0.get(1,0)==-1 && m0.get(1,1)==-1)), true);
  TEST("m0=3-m2",
       ((m0=3-m2),
        (m0.get(0,0)==1 && m0.get(0,1)==1 && m0.get(1,0)==1 && m0.get(1,1)==1)), true);
  TEST("m0= -m2",
       (m0= -m2,
        (m0.get(0,0)==-2 && m0.get(0,1)==-2 && m0.get(1,0)==-2 && m0.get(1,1)==-2)), true);

  vnl_matrix<int> m5(2,2);
  m0 = m2;
  TEST("m5=m0+m2",
       ((m5=m0+m2),
        (m5.get(0,0)==4 && m5.get(0,1)==4 && m5.get(1,0)==4 && m5.get(1,1)==4)), true);
  TEST("m5=m0-m2",
       ((m5=m0-m2),
        (m5.get(0,0)==0 && m5.get(0,1)==0 && m5.get(1,0)==0 && m5.get(1,1)==0)), true);
  TEST("m0+=m2",
       ((m0+=m2),
        (m0.get(0,0)==4 && m0.get(0,1)==4 && m0.get(1,0)==4 && m0.get(1,1)==4)), true);
  TEST("m0-=m2",
       ((m0-=m2),
        (m0.get(0,0)==2 && m0.get(0,1)==2 && m0.get(1,0)==2 && m0.get(1,1)==2)), true);

  /// test multiplications and divisions
  TEST("m4=m3*5",
       ((m4=m3*5),
        (m4.get(0,0)==5 && m4.get(0,1)==10 && m4.get(0,2)==15)), true);
  TEST("m4=5*m3",
       ((m4=5*m3),
        (m4.get(0,0)==5 && m4.get(0,1)==10 && m4.get(0,2)==15)), true);
  TEST("m3*=5",((m3*=5), (m3== m4)), true);
  TEST("m4=m3/5",
       ((m4=m3/5),
        (m4.get(0,0)==1 && m4.get(0,1)==2 && m4.get(0,2)==3)), true);
  TEST("m3/=5", ((m3/=5), (m3==m4)), true);

  int m6values [] = {1,2,3,4};
  vnl_matrix<int> m6(2,2,4,m6values);
  TEST("vnl_matrix<int> m6(2,2,4,{1,2,3,4})", m6.get(1,1), 4);
  int m7values [] = {5,6,7,8};
  vnl_matrix<int> m7(2,2,4,m7values);
  TEST("vnl_matrix<int> m7(2,2,4,{5,6,7,8})", m7.get(1,1), 8);
  TEST("m5=m6*m7",
       ((m5=m6*m7),
        (m5.get(0,0)==19 && m5.get(0,1)==22 && m5.get(1,0)==43 && m5.get(1,1)==50)), true);
  TEST("m6*=m7",
       ((m6*=m7),
        (m6.get(0,0)==19 && m6.get(0,1)==22 && m6.get(1,0)==43 && m6.get(1,1)==50)), true);
  int c0values [] = {1,0};
  vnl_matrix<int> c0(2,1,2,c0values);
  vnl_matrix<int> c1;
  TEST("c1=m6*c0",
       ((c1=m6*c0),
        c1.rows()==c0.rows() && c1.columns()==c0.columns() &&
        c1.get(0,0)==19 && c1.get(1,0)==43), true);
  int r0values [] = {1,0};
  vnl_matrix<int> r0(1,2,2,r0values);
  vnl_matrix<int> r1;
  TEST("r1=r0*m6",
       ((r1=r0*m6),
        r1.rows()==r0.rows() && r1.columns()==r0.columns() &&
        r1.get(0,0)==19 && r1.get(0,1)==22), true);
  TEST("r0*=m6",
       ((r0*=m6), r0==r1), true);
  TEST("m6*=c0",
       ((m6*=c0), c1==m6), true);


  // additional tests
  int mvalues [] = {0,-2,2,0};
  vnl_matrix<int> m(2,2,4,mvalues);
  m0 = m; m1 = m;
  TEST("m(i,j)",
       (m(0,0)==0 && m(0,1)==-2 && m(1,0)==2 && m(1,1)==0), true);
  TEST("m.transpose()",
       ((m1 = m.transpose()),
        (m1(0,0)==0 && m1(0,1)==2 && m1(1,0)==-2 && m1(1,1)==0)), true);
#if 0
  TEST("m.abs()",
       ((m1 = m.abs()),
        (m1(0,0)==0 && m1(0,1)==2 && m1(1,0)==2 && m1(1,1)==0)), true);
  TEST("m.sign()",
       ((m1 = m.sign()),
        (m1(0,0)==0 && m1(0,1)==-1 && m1(1,0)==1 && m1(1,1)==0)), true);
#endif // 0
  TEST("element_product(m,m)",
       ((m1 = element_product(m,m)),
        (m1(0,0)==0 && m1(0,1)==4 && m1(1,0)==4 && m1(1,1)==0)), true);
  TEST("element_quotient(m,[2])",
       ((m2 = 2),
        (m1 = element_quotient(m,m2)),
        (m1(0,0)==0 && m1(0,1)==-1 && m1(1,0)==1 && m1(1,1)==0)), true);
#if 0
  TEST("m.update(m.abs())",
       ((m1 = m.abs()),
        (m2.update(m1)),
        (m2==m1)), true);
#endif // 0
  TEST("m.extract(1,1,1,1)",
       ((m1 = m.extract(1,1,1,1)),
        (m1.rows()==1 && m1.columns()==1 && m1(0,0)==m(1,1))), true);
  TEST("m.update([4],1,1)",
       ((m1=4),
        (m.update(m1,1,1)),
        (m(0,0)==0 && m(0,1)==-2 && m(1,0)==2 && m(1,1)==4)), true);

  int vvalues[] = {1,0,0,0};
  vnl_matrix<int> v (4,1,4,vvalues);
#if 0
  TEST("v(i)",
       (v(0,0)==v.x() && v.x()==1 &&
        v(1,0)==v.y() && v.y()==0 &&
        v(2,0)==v.z() && v.z()==0 &&
        v(3,0)==v.t() && v.t()==0), true);
#endif // 0
  int v1values [] = {1,0,0};
  int v2values [] = {0,1,0};
  int v3values [] = {0,0,1};
  vnl_matrix<int> v1(3,1,3,v1values);
  vnl_matrix<int> v2(3,1,3,v2values);
  vnl_matrix<int> v3(3,1,3,v3values);
  TEST("dot_product(v1,v2)",
       (dot_product(v1,v2)==0 && dot_product(v1,v3)==0 && dot_product(v2,v3)==0), true);
  v = v3;
  TEST("4d-v=3d-v", (v.rows()==3 && v.columns()==1 && v==v3), true);

  // Zero-size
  {
    vnl_matrix<int> m1(0,3);
    vnl_matrix<int> m2(3,4);
    vnl_matrix<int> m3(4,0);
    vnl_matrix<int> m = m1 * (m2 * m3);
    TEST("zero-size mult rows", m.rows(), 0);
    TEST("zero-size mult cols", m.columns(), 0);

    m = (m1 * m2) * m3;
    TEST("zero-size mult rows", m.rows(), 0);
    TEST("zero-size mult cols", m.columns(), 0);

    m2.clear();
    TEST("zero-size after clear()", m2.rows(), 0);
    TEST("zero-size after clear()", m2.columns(), 0);
  }
}


void test_float()
{
  vcl_cout << "*************************\n"
           << "Testing vnl_matrix<float>\n"
           << "*************************\n";
  vnl_matrix<float> d0(2,2);
  TEST("vnl_matrix<float> d0(2,2)", (d0.rows()==2 && d0.columns()==2), true);
  vnl_matrix<float> d1(3,4);
  TEST("vnl_matrix<float> d1(3,4)", (d1.rows()==3 && d1.columns()==4), true);
  vnl_matrix<float> d2(2,2,2.0);
  TEST("vnl_matrix<float> d2(2,2,2.0)",
       (d2.get(0,0)==2.0 && d2.get(0,1)==2.0 && d2.get(1,0)==2.0 && d2.get(1,1)==2.0), true);
  TEST("d0=2.0", (d0=2.0,
                 (d0.get(0,0)==2.0 && d0.get(0,1)==2.0 && d0.get(1,0)==2.0 && d0.get(1,1)==2.0)), true);
  TEST("d0 == d2", (d0 == d2), true);
  TEST("(d0 == d2)", (d0==d2), true);
  TEST("d2.put(1,1,3.0)", (d2.put(1,1,(float)3.0),d2.get(1,1)), (float)3.0);
  TEST("d2.get(1,1)", d2.get(1,1), (float)3.0);
  TEST("d0 == d2", (d0 == d2), false);
  TEST("d0 != d2", (d0 != d2), true);
  TEST("(d0 == d2)", (d0==d2), false);
  TEST("d1.fill(3.0)",
       (d1.fill(3.0),
        (d1.get(0,0)==3.0 && d1.get(1,1)==3.0 && d1.get(2,2)==3.0 && d1.get(2,3)==3.0)), true);
  TEST("d2.fill(2.0)",
       (d2.fill(2.0),
        (d2.get(0,0)==2.0 && d2.get(0,1)==2.0 && d2.get(1,0)==2.0 && d2.get(1,1)==2.0)), true);
  float d3values [] = {1.0,2.0,3.0};
  vnl_matrix<float> d3(1,3,3,d3values);
  TEST("d3(1,3,3,{1.0,2.0,3.0})",
       (d3.get(0,0)==1.0 && d3.get(0,1)==2.0 && d3.get(0,2)==3.0), true);
  vnl_matrix<float> d4(d3);
  TEST("vnl_matrix<float> d4(d3)", d3, d4);
  TEST("d0=d2", (d0=d2,  (d0==d2)), true);
  TEST("d0=d2+3.0",
       ((d0=d2+(float)3.0),
        (d0.get(0,0)==5.0 && d0.get(0,1)==5.0 && d0.get(1,0)==5.0 && d0.get(1,1)==5.0)), true);
  TEST("d0+=(-3.0)",
       (d0+=(-3.0),
        (d0.get(0,0)==2.0 && d0.get(0,1)==2.0 && d0.get(1,0)==2.0 && d0.get(1,1)==2.0)), true);
  vnl_matrix<float> d5(2,2);
  TEST("d5=d0+d2",
       ((d5=d0+d2),
        (d5.get(0,0)==4.0 && d5.get(0,1)==4.0 && d5.get(1,0)==4.0 && d5.get(1,1)==4.0)), true);
  TEST("d0+=d2",
       ((d0+=d2),
        (d0.get(0,0)==4.0 && d0.get(0,1)==4.0 && d0.get(1,0)==4.0 && d0.get(1,1)==4.0)), true);
  TEST("d4=d3*5.0",((d4=d3*5.0),(d4.get(0,0)==5.0 && d4.get(0,1)==10.0 && d4.get(0,2)==15.0)), true);
  TEST("d3*=5.0",((d3*=5.0),  (d3== d4)), true);
  float d6values [] = {1.0,2.0,
                       3.0,4.0};
  vnl_matrix<float> d6(2,2,4,d6values);
  TEST("vnl_matrix<float> d6(2,2,4,{1.0,2.0,3.0,4.0})", d6.get(1,1), 4.0);
  float d7values [] = {5.0,6.0,
                       7.0,8.0};
  vnl_matrix<float> d7(2,2,4,d7values);
  TEST("vnl_matrix<float> d7(2,2,4,{5.0,6.0,7.0,8.0})", d7.get(1,1), 8.0);
  TEST("d5=d6*d7", ((d5=d6*d7),
                    (d5.get(0,0)==19.0 && d5.get(0,1)==22.0 && d5.get(1,0)==43.0 && d5.get(1,1)==50.0)), true);
  TEST("d6*=d7", ((d6*=d7),
                  (d6.get(0,0)==19.0 && d6.get(0,1)==22.0 && d6.get(1,0)==43.0 && d6.get(1,1)==50.0)), true);

  // additional tests
  vnl_matrix<float> m0, m1, m2;
  float mvalues [] = {0,-2,2,0};
  vnl_matrix<float> m(2,2,4,mvalues);
  m0 = m; m1 = m; m2 = m;
  TEST("m(i,j)",
       (m(0,0)==0 && m(0,1)==-2 && m(1,0)==2 && m(1,1)==0), true);
  TEST("m.transpose()",
       ((m1 = m.transpose()),
        (m1(0,0)==0 && m1(0,1)==2 && m1(1,0)==-2 && m1(1,1)==0)), true);
#if 0
  TEST("m.abs()",
       ((m1 = m.abs()),
        (m1(0,0)==0 && m1(0,1)==2 && m1(1,0)==2 && m1(1,1)==0)), true);
  TEST("m.sign()",
       ((m1 = m.sign()),
        (m1(0,0)==0 && m1(0,1)==-1 && m1(1,0)==1 && m1(1,1)==0)), true);
#endif
  TEST("element_product(m,m)",
       ((m1 = element_product(m,m)),
        (m1(0,0)==0 && m1(0,1)==4 && m1(1,0)==4 && m1(1,1)==0)), true);
  TEST("element_quotient(m,[2])",
       ((m2 = 2),
        (m1 = element_quotient(m,m2)),
        (m1(0,0)==0 && m1(0,1)==-1 && m1(1,0)==1 && m1(1,1)==0)), true);
#if 0
  TEST("m.update(m.abs())",
       ((m1 = m.abs()),
        (m2.update(m1)),
        (m2==m1)), true);
#endif
  TEST("m.extract(1,1,1,1)",
       ((m1 = m.extract(1,1,1,1)),
        (m1.rows()==1 && m1.columns()==1 && m1(0,0)==m(1,1))), true);
  TEST("m.update([4],1,1)",
       ((m1=4),
        (m.update(m1,1,1)),
        (m(0,0)==0 && m(0,1)==-2 && m(1,0)==2 && m(1,1)==4)), true);

  float vvalues[] = {1,0,0,0};
  vnl_matrix<float> v (4,1,4,vvalues);
#if 0
  TEST("v(i)",
       (v(0,0)==v.x() && v.x()==1 &&
        v(1,0)==v.y() && v.y()==0 &&
        v(2,0)==v.z() && v.z()==0 &&
        v(3,0)==v.t() && v.t()==0), true);
#endif
  float v1values [] = {1,0,0};
  float v2values [] = {0,1,0};
  float v3values [] = {0,0,1};
  vnl_matrix<float> v1(3,1,3,v1values);
  vnl_matrix<float> v2(3,1,3,v2values);
  vnl_matrix<float> v3(3,1,3,v3values);
  TEST("dot_product(v1,v2)",
       (dot_product(v1,v2)==0 && dot_product(v1,v3)==0 && dot_product(v2,v3)==0), true);
  v = v3;
  TEST("4d-v=3d-v", (v.rows()==3 && v.columns()==1 && v==v3), true);

  v.clear();
  TEST("zero-size after clear()", v.rows(), 0);
  TEST("zero-size after clear()", v.columns(), 0);
}

void test_double()
{
  vcl_cout << "**************************\n"
           << "Testing vnl_matrix<double>\n"
           << "**************************\n";
  vnl_matrix<double> d0(2,2);
  TEST("vnl_matrix<double> d0(2,2)", (d0.rows()==2 && d0.columns()==2), true);
  vnl_matrix<double> d1(3,4);
  TEST("vnl_matrix<double> d1(3,4)", (d1.rows()==3 && d1.columns()==4), true);
  vnl_matrix<double> d2(2,2,2.0);
  TEST("vnl_matrix<double> d2(2,2,2.0)",
       (d2.get(0,0)==2.0 && d2.get(0,1)==2.0 && d2.get(1,0)==2.0 && d2.get(1,1)==2.0), true);
  TEST("d0=2.0", (d0=2.0,
                  (d0.get(0,0)==2.0 && d0.get(0,1)==2.0 && d0.get(1,0)==2.0 && d0.get(1,1)==2.0)), true);
  TEST("d0 == d2", (d0 == d2), true);
  TEST("(d0 == d2)", (d0==d2), true);
  TEST("d2.put(1,1,3.0)", (d2.put(1,1,3.0),d2.get(1,1)), 3.0);
  TEST("d2.get(1,1)", d2.get(1,1), 3.0);
  TEST("d0 == d2", (d0 == d2), false);
  TEST("d0 != d2", (d0 != d2), true);
  TEST("(d0 == d2)", (d0==d2), false);
  TEST("d1.fill(3.0)",
       (d1.fill(3.0),
        (d1.get(0,0)==3.0 && d1.get(1,1)==3.0 && d1.get(2,2)==3.0 && d1.get(2,3)==3.0)), true);
  TEST("d2.fill(3.0)",
       (d2.fill(2.0),
        (d2.get(0,0)==2.0 && d2.get(0,1)==2.0 && d2.get(1,0)==2.0 && d2.get(1,1)==2.0)), true);
  double d3values [] = {1.0,2.0,3.0};
  vnl_matrix<double> d3(1,3,3,d3values);
  TEST("d3(1,3,3,{1.0,2.0,3.0})",
       (d3.get(0,0)==1.0 && d3.get(0,1)==2.0 && d3.get(0,2)==3.0), true);
  vnl_matrix<double> d4(d3);
  TEST("vnl_matrix<double> d4(d3)", (d3 == d4), true);
  TEST("d0=d2", (d0=d2,  (d0==d2)), true);
  TEST("d0=d2+3.0",
       ((d0=d2+3.0),
        (d0.get(0,0)==5.0 && d0.get(0,1)==5.0 && d0.get(1,0)==5.0 && d0.get(1,1)==5.0)), true);
  TEST("d0+=(-3.0)",
       (d0+=(-3.0),
        (d0.get(0,0)==2.0 && d0.get(0,1)==2.0 && d0.get(1,0)==2.0 && d0.get(1,1)==2.0)), true);
  vnl_matrix<double> d5(2,2);
  TEST("d5=d0+d2",
       ((d5=d0+d2),
        (d5.get(0,0)==4.0 && d5.get(0,1)==4.0 && d5.get(1,0)==4.0 && d5.get(1,1)==4.0)), true);
  TEST("d0+=d2",
       ((d0+=d2),
        (d0.get(0,0)==4.0 && d0.get(0,1)==4.0 && d0.get(1,0)==4.0 && d0.get(1,1)==4.0)), true);
  TEST("d4=d3*5.0",((d4=d3*5.0),(d4.get(0,0)==5.0 && d4.get(0,1)==10.0 && d4.get(0,2)==15.0)), true);
  TEST("d3*=5.0",((d3*=5.0),  (d3== d4)), true);
  double d6values [] = {1.0,2.0,
                        3.0,4.0};
  vnl_matrix<double> d6(2,2,4,d6values);
  TEST("vnl_matrix<double> d6(2,2,4,{1.0,2.0,3.0,4.0})", d6.get(1,1), 4.0);
  double d7values [] = {5.0,6.0,
                        7.0,8.0};
  vnl_matrix<double> d7(2,2,4,d7values);
  TEST("vnl_matrix<double> d7(2,2,4,{5.0,6.0,7.0,8.0})", d7.get(1,1), 8.0);
  TEST("d5=d6*d7", ((d5=d6*d7),
                    (d5.get(0,0)==19.0 && d5.get(0,1)==22.0 && d5.get(1,0)==43.0 && d5.get(1,1)==50.0)), true);
  TEST("d6*=d7", ((d6*=d7),
                  (d6.get(0,0)==19.0 && d6.get(0,1)==22.0 && d6.get(1,0)==43.0 && d6.get(1,1)==50.0)), true);

  d0.clear();
  TEST("zero-size after clear()", d0.rows(), 0);
  TEST("zero-size after clear()", d0.columns(), 0);

  // apply sqrt to every element
  double d8values [] = {0.0, 1.0, 9.0, 16.0};
  vnl_matrix<double> d8(2,2,4,d8values);
  d8 = d8.apply(vcl_sqrt);
  TEST("apply(sqrt)", d8[0][0]==0 && d8[0][1]==1 && d8[1][0]==3 && d8[1][1]==4, true);

  // normalizations
  d8.normalize_rows();
  TEST("normalize_rows()", d8[0][0]==0 && d8[0][1]==1, true);
  TEST_NEAR("normalize_rows()", d8[1][0], 0.6, 1e-12);
  TEST_NEAR("normalize_rows()", d8[1][1], 0.8, 1e-12);
  d8.normalize_columns();
  TEST("normalize_columns()", d8[0][0]==0 && d8[1][0]==1, true);
}

#ifdef LEAK
static
void test_leak()   // use top4.1 to watch memory usage.
{
  for (;;) {       // remember to kill process.
    test_int();
    test_float();
    test_double();
  }
}
#endif

static
void test_matrix()
{
  test_int();
  test_float();
  test_double();
#ifdef LEAK
  test_leak();
#endif
}

TESTMAIN(test_matrix);
