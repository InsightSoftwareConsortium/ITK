#include <iostream>
#include <vector>
#include <sstream>

#include <testlib/testlib_test.h>
#include <vnl/vnl_polynomial.h>
#include <vnl/vnl_decnum.h>
#include <vnl/vnl_rational.h>
#include <vcl_compiler.h>

void test_polynomial_double()
{
  vnl_polynomial<double> f1(3),f2(4);

  for (int i=0;i<=f1.degree();++i) f1[i]=4.0-i; // f1 = X^3 +2 X^2 +3 X + 4
  std::cout << "f1 =" << f1 << std::endl;
  { std::stringstream testStream; testStream << f1;
    std::string expected = " X^3 +2 X^2 +3 X +4";
    TEST("f1 prints as X^3 +2 X^2 +3 X +4", testStream.str(), expected);
  }

  for (int i=0;i<=f2.degree();++i) f2[i]=9.0-2*i; // f2 = X^4 +3 X^3 +5 X^2 +7 X +9
  std::cout << "f2 =" << f2 << std::endl;

  vnl_polynomial<double> f3 = -f1;
  std::cout << "-f1 =" << f3 << std::endl;
  TEST("-f1(x)", -f1.evaluate(2.5), f3.evaluate(2.5));
  // Evaluating in 2.5 is exact, since 2.5 is exactly representable (binary 10.1)
  TEST("-f1", f3.degree()==3 && f3[3]==-1 && f3[2]==-2 && f3[1]==-3 && f3[0]==-4, true);

  f3 = f1+f2;
  std::cout << "f1+f2 =" << f3 << std::endl;
  TEST("f1(x)+f2(x)",f1.evaluate(2.5)+f2.evaluate(2.5), f3.evaluate(2.5));
  TEST("f1+f2", f3.degree()==4 && f3[4]==1 && f3[3]==4 && f3[2]==7 && f3[1]==10 && f3[0]==13, true);

  f3 = f2+f1;
  std::cout << "f2+f1 =" << f3 << std::endl;
  TEST("f2(x)+f1(x)",f2.evaluate(2.5)+f1.evaluate(2.5), f3.evaluate(2.5));
  TEST("f2+f1", f3.degree()==4 && f3[4]==1 && f3[3]==4 && f3[2]==7 && f3[1]==10 && f3[0]==13, true);

  f3 = f1-f2;
  std::cout << "f1-f2 =" << f3 << std::endl;
  TEST("f1(x)-f2(x)",f1.evaluate(2.5)-f2.evaluate(2.5), f3.evaluate(2.5));
  TEST("f1-f2", f3.degree()==4 && f3[4]==-1 && f3[3]==-2 && f3[2]==-3 && f3[1]==-4 && f3[0]==-5, true);

  f3 = f2-f1;
  std::cout << "f2-f1 =" << f3 << std::endl;
  TEST("f2(x)-f1(x)",f2.evaluate(2.5)-f1.evaluate(2.5), f3.evaluate(2.5));
  TEST("f2-f1", f3.degree()==4 && f3[4]==1 && f3[3]==2 && f3[2]==3 && f3[1]==4 && f3[0]==5, true);

  f3 = f2/f1;
  std::cout << "f2/f1 =" << f3 << std::endl; // should be X+1
  TEST("f2(x)/f1(x)", f3.evaluate(2.5), 3.5);
  TEST("f2/f1", f3.degree()==1 && f3[0]==1 && f3[1]==1, true);

  f3 = f2%f1;
  std::cout << "f2%f1 =" << f3 << std::endl; // should be 5
  TEST("f2(x)%f1(x)", f3.evaluate(2.5), 5);
  TEST("f2%f1", f3.degree()==0 && f3[0]==5, true);

  f3 = f1/f2;
  std::cout << "f1/f2 =" << f3 << std::endl;
  TEST("f1/f2", f3.degree(), -1);

  f3 = f1%f2;
  std::cout << "f1%f2 =" << f3 << std::endl;
  TEST("f1%f2", f3, f1);

  vnl_polynomial<double> f1d = f1.derivative();
  std::cout << "f1d =" << f1d << std::endl;
  vnl_polynomial<double> f2d = f2.derivative();
  std::cout << "f2d =" << f2d << std::endl;

  f3 = f1*f2;
  std::cout << "f1*f2 =" << f3 << std::endl;
  TEST("f1*f2 has correct degree", f3.degree(), f1.degree()+f2.degree());
  TEST("f1(x)*f2(x)",f1.evaluate(2.5)*f2.evaluate(2.5), f3.evaluate(2.5));

  std::cout << "f3d =" << f3.derivative() << std::endl;
  TEST("Derivative", f3.derivative(), f1d*f2+f2d*f1);

  vnl_polynomial<double> f3p = (f1d*f2+f2d*f1).primitive();
  std::cout << "f3p =" << f3p << std::endl;

  TEST("Primitive", f3p, f3-f3.evaluate(0.0));
  TEST_NEAR("Integral", f1.evaluate_integral(2.0), 70.0/3, 1e-9);

  TEST("Polynomial of degree 0", vnl_polynomial<double>(1.0).evaluate(0.5),1.0);
  TEST("Polynomial of degree -1", vnl_polynomial<double>().evaluate(0.5),0.0);
  std::vector<double> v; v.push_back(1.0); v.push_back(2.0); v.push_back(3.0);
  vnl_polynomial<double> f4(v);
  TEST("Vector initialisation", f4.evaluate(2.0),17.0);
  TEST("Degree", f4.degree(),2);
  f4 -= vnl_polynomial<double>(6.0).primitive().primitive(); // which is 3 X^2
  TEST("Degree after manipulation", f4.degree(),1); // since highest order term dropped out

  std::cout << "(f1*f2)/f2 =" << (f1*f2)/f2 << std::endl;
  TEST("(f1*f2)/f2 == f1", (f1*f2)/f2, f1);
  std::cout << "(f1*f2)/f1 =" << (f1*f2)/f1 << std::endl;
  TEST("(f1*f2)/f1 == f2", (f1*f2)/f1, f2);
  std::cout << "(f1*f2)%f2 =" << (f1*f2)%f2 << std::endl;
  TEST("(f1*f2)%f2 == 0", (f1*f2)%f2, vnl_polynomial<double>());
  std::cout << "(f1*f2)%f1 =" << (f1*f2)%f1 << std::endl;
  TEST("(f1*f2)%f1 == 0", (f1*f2)%f1, vnl_polynomial<double>());

  f4[0]=f4[1]=1; // f4 = X + 1
  { std::stringstream testStream; testStream << f4;
    std::string expected = " X +1";
    TEST("f4 prints as X +1", testStream.str(), expected);
  }
  f4 *= 2.0; // f4 = 2 X + 2
  { std::stringstream testStream; testStream << f4;
    std::string expected = " 2 X +2";
    TEST("f4 prints as 2 X +2", testStream.str(), expected);
  }
  f4 = -f4; // f4 = -2 X - 2
  { std::stringstream testStream; testStream << f4;
    std::string expected = " -2 X -2";
    TEST("f4 prints as -2 X -2", testStream.str(), expected);
  }
  f4[1] = 1; // f4 = X - 2
  { std::stringstream testStream; testStream << f4;
    std::string expected = " X -2";
    TEST("f4 prints as X -2", testStream.str(), expected);
  }
  f4[0] = 0; // f4 = X
  { std::stringstream testStream; testStream << f4;
    std::string expected = " X";
    TEST("f4 prints as X", testStream.str(), expected);
  }
  f4[1] = 0; // f4 = 0
  { std::stringstream testStream; testStream << f4;
    std::string expected = " 0";
    TEST("f4 prints as 0", testStream.str(), expected);
  }
}

void test_polynomial_long()
{
  vnl_polynomial<long> f1(3),f2(4);

  for (int i=0;i<=f1.degree();++i) f1[i]=4-i; // f1 = X^3 +2 X^2 +3 X + 4
  std::cout << "f1 =" << f1 << std::endl;
  { std::stringstream testStream; testStream << f1;
    std::string expected = " X^3 +2 X^2 +3 X +4";
    TEST("f1 prints as X^3 +2 X^2 +3 X +4", testStream.str(), expected);
  }

  for (int i=0;i<=f2.degree();++i) f2[i]=9-2*i; // f2 = X^4 +3 X^3 +5 X^2 +7 X +9
  std::cout << "f2 =" << f2 << std::endl;

  vnl_polynomial<long> f3 = -f1;
  std::cout << "-f1 =" << f3 << std::endl;
  TEST("-f1(x)", -f1.evaluate(302L), f3.evaluate(302L));
  TEST("-f1", f3.degree()==3 && f3[3]==-1 && f3[2]==-2 && f3[1]==-3 && f3[0]==-4, true);

  f3 = f1+f2;
  std::cout << "f1+f2 =" << f3 << std::endl;
  TEST("f1(x)+f2(x)",f1.evaluate(302L)+f2.evaluate(302L), f3.evaluate(302L));
  TEST("f1+f2", f3.degree()==4 && f3[4]==1 && f3[3]==4 && f3[2]==7 && f3[1]==10 && f3[0]==13, true);

  f3 = f2+f1;
  std::cout << "f2+f1 =" << f3 << std::endl;
  TEST("f2(x)+f1(x)",f2.evaluate(302L)+f1.evaluate(302L), f3.evaluate(302L));
  TEST("f2+f1", f3.degree()==4 && f3[4]==1 && f3[3]==4 && f3[2]==7 && f3[1]==10 && f3[0]==13, true);

  f3 = f1-f2;
  std::cout << "f1-f2 =" << f3 << std::endl;
  TEST("f1(x)-f2(x)",f1.evaluate(302L)-f2.evaluate(302L), f3.evaluate(302L));
  TEST("f1-f2", f3.degree()==4 && f3[4]==-1 && f3[3]==-2 && f3[2]==-3 && f3[1]==-4 && f3[0]==-5, true);

  f3 = f2-f1;
  std::cout << "f2-f1 =" << f3 << std::endl;
  TEST("f2(x)-f1(x)",f2.evaluate(302L)-f1.evaluate(302L), f3.evaluate(302L));
  TEST("f2-f1", f3.degree()==4 && f3[4]==1 && f3[3]==2 && f3[2]==3 && f3[1]==4 && f3[0]==5, true);

  vnl_polynomial<long> f1d = f1.derivative();
  std::cout << "f1d =" << f1d << std::endl;
  vnl_polynomial<long> f2d = f2.derivative();
  std::cout << "f2d =" << f2d << std::endl;

  f3 = f1*f2;
  std::cout << "f1*f2 =" << f3 << std::endl;
  TEST("f1*f2 has correct degree", f3.degree(), f1.degree()+f2.degree());
  TEST("f1(x)*f2(x)",f1.evaluate(29L)*f2.evaluate(29L), f3.evaluate(29L));

  std::cout << "f3d =" << f3.derivative() << std::endl;
  TEST("Derivative", f3.derivative(), f1d*f2+f2d*f1);

  vnl_polynomial<long> f3p = (f1d*f2+f2d*f1).primitive();
  std::cout << "f3p =" << f3p << std::endl;

  TEST("Polynomial of degree 0", vnl_polynomial<long>(12L).evaluate(302L),12L);
  TEST("Polynomial of degree -1", vnl_polynomial<long>().evaluate(302L),0L);
  std::vector<long> v; v.push_back(1L); v.push_back(2L); v.push_back(3L);
  vnl_polynomial<long> f4(v);
  TEST("Vector initialisation", f4.evaluate(2L),17L);
  TEST("Degree", f4.degree(),2);
  f4 -= vnl_polynomial<long>(6L).primitive().primitive(); // which is 3 X^2
  std::cout << f4;
  TEST("Degree after manipulation", f4.degree(),1); // since highest order term dropped out

  f4[0]=f4[1]=1L; // f4 = X + 1
  { std::stringstream testStream; testStream << f4;
    std::string expected = " X +1";
    TEST("f4 prints as X +1", testStream.str(), expected);
  }
  f4 *= 2L; // f4 = 2 X + 2
  { std::stringstream testStream; testStream << f4;
    std::string expected = " 2 X +2";
    TEST("f4 prints as 2 X +2", testStream.str(), expected);
  }
  f4 = -f4; // f4 = -2 X - 2
  { std::stringstream testStream; testStream << f4;
    std::string expected = " -2 X -2";
    TEST("f4 prints as -2 X -2", testStream.str(), expected);
  }
  f4[1] = 1L; // f4 = X - 2
  { std::stringstream testStream; testStream << f4;
    std::string expected = " X -2";
    TEST("f4 prints as X -2", testStream.str(), expected);
  }
  f4[0] = 0L; // f4 = X
  { std::stringstream testStream; testStream << f4;
    std::string expected = " X";
    TEST("f4 prints as X", testStream.str(), expected);
  }
  f4[1] = 0L; // f4 = 0
  { std::stringstream testStream; testStream << f4;
    std::string expected = " 0";
    TEST("f4 prints as 0", testStream.str(), expected);
  }
}

void test_polynomial_rational()
{
  vnl_polynomial<vnl_rational> f1(3),f2(4);

  for (int i=0;i<=f1.degree();++i) f1[i]=vnl_rational(1,4-i); // f1 = X^3 +1/2 X^2 +1/3 X + 1/4
  std::cout << "f1 =" << f1 << std::endl;
  { std::stringstream testStream; testStream << f1;
    std::string expected = " X^3 +1/2 X^2 +1/3 X +1/4";
    TEST("f1 prints as X^3 +1/2 X^2 +1/3 X +1/4", testStream.str(), expected);
  }

  for (int i=0;i<=f2.degree();++i) f2[i]=9-2*i; // f2 = X^4 +3 X^3 +5 X^2 +7 X +9
  std::cout << "f2 =" << f2 << std::endl;

  vnl_polynomial<vnl_rational> f3 = -f1;
  std::cout << "-f1 =" << f3 << std::endl;
  TEST("-f1(x)", -f1.evaluate(vnl_rational(7,9)), f3.evaluate(vnl_rational(7,9)));
  TEST("-f1", f3.degree()==3 && f3[3]==-1 && f3[2]==vnl_rational(-1,2) && f3[1]==vnl_rational(-1,3) && f3[0]==vnl_rational(-1,4), true);

  f3 = f1+f2;
  std::cout << "f1+f2 =" << f3 << std::endl;
  TEST("f1(x)+f2(x)",f1.evaluate(vnl_rational(7,9))+f2.evaluate(vnl_rational(7,9)), f3.evaluate(vnl_rational(7,9)));
  TEST("f1+f2", f3.degree()==4 && f3[4]==1 && f3[3]==4 && f3[2]==vnl_rational(11,2) && f3[1]==vnl_rational(22,3) && f3[0]==vnl_rational(37,4), true);

  f3 = f2+f1;
  std::cout << "f2+f1 =" << f3 << std::endl;
  TEST("f2(x)+f1(x)",f2.evaluate(vnl_rational(7,9))+f1.evaluate(vnl_rational(7,9)), f3.evaluate(vnl_rational(7,9)));
  TEST("f2+f1", f3.degree()==4 && f3[4]==1 && f3[3]==4 && f3[2]==vnl_rational(11,2) && f3[1]==vnl_rational(22,3) && f3[0]==vnl_rational(37,4), true);

  f3 = f1-f2;
  std::cout << "f1-f2 =" << f3 << std::endl;
  TEST("f1(x)-f2(x)",f1.evaluate(vnl_rational(7,9))-f2.evaluate(vnl_rational(7,9)), f3.evaluate(vnl_rational(7,9)));
  TEST("f1-f2", f3.degree()==4 && f3[4]==-1 && f3[3]==-2 && f3[2]==vnl_rational(-9,2) && f3[1]==vnl_rational(-20,3) && f3[0]==vnl_rational(-35,4), true);

  f3 = f2-f1;
  std::cout << "f2-f1 =" << f3 << std::endl;
  TEST("f2(x)-f1(x)",f2.evaluate(vnl_rational(7,9))-f1.evaluate(vnl_rational(7,9)), f3.evaluate(vnl_rational(7,9)));
  TEST("f2-f1", f3.degree()==4 && f3[4]==1 && f3[3]==2 && f3[2]==vnl_rational(9,2) && f3[1]==vnl_rational(20,3) && f3[0]==vnl_rational(35,4), true);

  f3 = f2/f1;
  std::cout << "f2/f1 =" << f3 << std::endl; // should be X+5/2
  TEST("f2(x)/f1(x)", f3.evaluate(vnl_rational(7,9)), vnl_rational(59,18));
  TEST("f2/f1", f3.degree()==1 && f3[1]==1 && f3[0]==vnl_rational(5,2), true);

  f3 = f2%f1;
  std::cout << "f2%f1 =" << f3 << std::endl; // should be 41/12 X^2 +71/12 X +67/8
  TEST("f2(x)%f1(x)", f3.evaluate(vnl_rational(7,9)), vnl_rational(29245,1944));
  TEST("f2%f1", f3.degree()==2 && f3[2]==vnl_rational(41,12) && f3[1]==vnl_rational(71,12) && f3[0]==vnl_rational(67,8), true);

  f3 = f1/f2;
  std::cout << "f1/f2 =" << f3 << std::endl;
  TEST("f1/f2", f3.degree(), -1);

  f3 = f1%f2;
  std::cout << "f1%f2 =" << f3 << std::endl;
  TEST("f1%f2", f3, f1);

  vnl_polynomial<vnl_rational> f1d = f1.derivative();
  std::cout << "f1d =" << f1d << std::endl;
  vnl_polynomial<vnl_rational> f2d = f2.derivative();
  std::cout << "f2d =" << f2d << std::endl;

  f3 = f1*f2;
  std::cout << "f1*f2 =" << f3 << std::endl;
  TEST("f1*f2 has correct degree", f3.degree(), f1.degree()+f2.degree());
  TEST("f1(x)*f2(x)",f1.evaluate(vnl_rational(7,9))*f2.evaluate(vnl_rational(7,9)), f3.evaluate(vnl_rational(7,9)));

  std::cout << "f3d =" << f3.derivative() << std::endl;
  TEST("Derivative", f3.derivative(), f1d*f2+f2d*f1);

  vnl_polynomial<vnl_rational> f3p = (f1d*f2+f2d*f1).primitive();
  std::cout << "f3p =" << f3p << std::endl;

  TEST("Primitive", f3p, f3-f3.evaluate(0));
  TEST("Integral", f1.evaluate_integral(2), vnl_rational(13,2));

  TEST("Polynomial of degree 0", vnl_polynomial<vnl_rational>(vnl_rational(1)).evaluate(vnl_rational(7,9)),1);
  TEST("Polynomial of degree -1", vnl_polynomial<vnl_rational>().evaluate(vnl_rational(7,9)),0);
  std::vector<vnl_rational> v; v.push_back(1); v.push_back(2); v.push_back(3);
  vnl_polynomial<vnl_rational> f4(v);
  TEST("Vector initialisation", f4.evaluate(vnl_rational(7,9)),vnl_rational(354,81));
  TEST("Degree", f4.degree(),2);
  f4 -= vnl_polynomial<vnl_rational>(vnl_rational(6)).primitive().primitive(); // which is 3 X^2
  TEST("Degree after manipulation", f4.degree(),1); // since highest order term dropped out

  std::cout << "(f1*f2)/f2 =" << (f1*f2)/f2 << std::endl;
  TEST("(f1*f2)/f2 == f1", (f1*f2)/f2, f1);
  std::cout << "(f1*f2)/f1 =" << (f1*f2)/f1 << std::endl;
  TEST("(f1*f2)/f1 == f2", (f1*f2)/f1, f2);
  std::cout << "(f1*f2)%f2 =" << (f1*f2)%f2 << std::endl;
  TEST("(f1*f2)%f2 == 0", (f1*f2)%f2, vnl_polynomial<vnl_rational>());
  std::cout << "(f1*f2)%f1 =" << (f1*f2)%f1 << std::endl;
  TEST("(f1*f2)%f1 == 0", (f1*f2)%f1, vnl_polynomial<vnl_rational>());

  f4[0]=f4[1]=1; // f4 = X + 1
  { std::stringstream testStream; testStream << f4;
    std::string expected = " X +1/1";
    TEST("f4 prints as X +1/1", testStream.str(), expected);
  }
  f4 *= vnl_rational(2); // f4 = 2 X + 2
  { std::stringstream testStream; testStream << f4;
    std::string expected = " 2/1 X +2/1";
    TEST("f4 prints as 2/1 X +2/1", testStream.str(), expected);
  }
  f4 = -f4; // f4 = -2 X - 2
  { std::stringstream testStream; testStream << f4;
    std::string expected = " -2/1 X -2/1";
    TEST("f4 prints as -2/1 X -2/1", testStream.str(), expected);
  }
  f4[1] = 1; // f4 = X - 2
  { std::stringstream testStream; testStream << f4;
    std::string expected = " X -2/1";
    TEST("f4 prints as X -2/1", testStream.str(), expected);
  }
  f4[0] = 0; // f4 = X
  { std::stringstream testStream; testStream << f4;
    std::string expected = " X";
    TEST("f4 prints as X", testStream.str(), expected);
  }
  f4[1] = 0; // f4 = 0
  { std::stringstream testStream; testStream << f4;
    std::string expected = " 0";
    TEST("f4 prints as 0", testStream.str(), expected);
  }
}

void test_polynomial_decnum()
{
  vnl_polynomial<vnl_decnum> f1(3),f2(4);

  for (int i=0;i<=f1.degree();++i) f1[i]=4-i; // f1 = X^3 +2 X^2 +3 X + 4
  std::cout << "f1 =" << f1 << std::endl;
  { std::stringstream testStream; testStream << f1;
    std::string expected = " X^3 +2 X^2 +3 X +4";
    TEST("f1 prints as X^3 +2 X^2 +3 X +4", testStream.str(), expected);
  }

  for (int i=0;i<=f2.degree();++i) f2[i]=9-2*i; // f2 = X^4 +3 X^3 +5 X^2 +7 X +9
  std::cout << "f2 =" << f2 << std::endl;

  vnl_polynomial<vnl_decnum> f3 = -f1;
  std::cout << "-f1 =" << f3 << std::endl;
  TEST("-f1(x)", -f1.evaluate("35e19"), f3.evaluate("35e19"));
  TEST("-f1", f3.degree()==3 && f3[3]==-1 && f3[2]==-2 && f3[1]==-3 && f3[0]==-4, true);

  f3 = f1+f2;
  std::cout << "f1+f2 =" << f3 << std::endl;
  TEST("f1(x)+f2(x)",f1.evaluate("35e19")+f2.evaluate("35e19"), f3.evaluate("35e19"));
  TEST("f1+f2", f3.degree()==4 && f3[4]==1 && f3[3]==4 && f3[2]==7 && f3[1]==10 && f3[0]==13, true);

  f3 = f2+f1;
  std::cout << "f2+f1 =" << f3 << std::endl;
  TEST("f2(x)+f1(x)",f2.evaluate("35e19")+f1.evaluate("35e19"), f3.evaluate("35e19"));
  TEST("f2+f1", f3.degree()==4 && f3[4]==1 && f3[3]==4 && f3[2]==7 && f3[1]==10 && f3[0]==13, true);

  f3 = f1-f2;
  std::cout << "f1-f2 =" << f3 << std::endl;
  TEST("f1(x)-f2(x)",f1.evaluate("35e19")-f2.evaluate("35e19"), f3.evaluate("35e19"));
  TEST("f1-f2", f3.degree()==4 && f3[4]==-1 && f3[3]==-2 && f3[2]==-3 && f3[1]==-4 && f3[0]==-5, true);

  f3 = f2-f1;
  std::cout << "f2-f1 =" << f3 << std::endl;
  TEST("f2(x)-f1(x)",f2.evaluate("35e19")-f1.evaluate("35e19"), f3.evaluate("35e19"));
  TEST("f2-f1", f3.degree()==4 && f3[4]==1 && f3[3]==2 && f3[2]==3 && f3[1]==4 && f3[0]==5, true);

  vnl_polynomial<vnl_decnum> f1d = f1.derivative();
  std::cout << "f1d =" << f1d << std::endl;
  vnl_polynomial<vnl_decnum> f2d = f2.derivative();
  std::cout << "f2d =" << f2d << std::endl;

  f3 = f1*f2;
  std::cout << "f1*f2 =" << f3 << std::endl;
  TEST("f1*f2 has correct degree", f3.degree(), f1.degree()+f2.degree());
  TEST("f1(x)*f2(x)",f1.evaluate("35e19")*f2.evaluate("35e19"), f3.evaluate("35e19"));

  std::cout << "f3d =" << f3.derivative() << std::endl;
  TEST("Derivative", f3.derivative(), f1d*f2+f2d*f1);

  vnl_polynomial<vnl_decnum> f3p = (f1d*f2+f2d*f1).primitive();
  std::cout << "f3p =" << f3p << std::endl;

  TEST("Polynomial of degree 0", vnl_polynomial<vnl_decnum>("12").evaluate("35e19"),vnl_decnum("12"));
  TEST("Polynomial of degree -1", vnl_polynomial<vnl_decnum>().evaluate("35e19"),0L);
  std::vector<vnl_decnum> v; v.push_back(1L); v.push_back(2L); v.push_back(3L);
  vnl_polynomial<vnl_decnum> f4(v);
  TEST("Vector initialisation", f4.evaluate(2L),17L);
  TEST("Degree", f4.degree(),2);
  f4 -= vnl_polynomial<vnl_decnum>("6").primitive().primitive(); // which is 3 X^2
  std::cout << f4;
  TEST("Degree after manipulation", f4.degree(),1); // since highest order term dropped out

  f4[0]=f4[1]=1L; // f4 = X + 1
  { std::stringstream testStream; testStream << f4;
    std::string expected = " X +1";
    TEST("f4 prints as X +1", testStream.str(), expected);
  }
  f4 *= vnl_decnum("2"); // f4 = 2 X + 2
  { std::stringstream testStream; testStream << f4;
    std::string expected = " 2 X +2";
    TEST("f4 prints as 2 X +2", testStream.str(), expected);
  }
  f4 = -f4; // f4 = -2 X - 2
  { std::stringstream testStream; testStream << f4;
    std::string expected = " -2 X -2";
    TEST("f4 prints as -2 X -2", testStream.str(), expected);
  }
  f4[1] = 1L; // f4 = X - 2
  { std::stringstream testStream; testStream << f4;
    std::string expected = " X -2";
    TEST("f4 prints as X -2", testStream.str(), expected);
  }
  f4[0] = 0L; // f4 = X
  { std::stringstream testStream; testStream << f4;
    std::string expected = " X";
    TEST("f4 prints as X", testStream.str(), expected);
  }
  f4[1] = 0L; // f4 = 0
  { std::stringstream testStream; testStream << f4;
    std::string expected = " 0";
    TEST("f4 prints as 0", testStream.str(), expected);
  }
}

void test_polynomial()
{
  std::cout << "========== testing vnl_polynomial<double> ==========" << std::endl;
  test_polynomial_double();
  std::cout << "========== testing vnl_polynomial<long> ==========" << std::endl;
  test_polynomial_long();
  std::cout << "========== testing vnl_polynomial<vnl_rational> ==========" << std::endl;
  test_polynomial_rational();
  std::cout << "========== testing vnl_polynomial<vnl_decnum> ==========" << std::endl;
  test_polynomial_decnum();
}

TESTMAIN(test_polynomial);
