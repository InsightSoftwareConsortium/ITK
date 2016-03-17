#include <vcl_iostream.h>
#include <vcl_iomanip.h>
#include <vcl_sstream.h>
#include <vcl_limits.h> // for infinity()
#include <vnl/vnl_math.h>
#include <vnl/vnl_na.h>
#include <testlib/testlib_test.h>


#define print_hex(p) \
  vcl_hex<<vcl_setfill('0')<<vcl_setw(2)<<(short)reinterpret_cast<unsigned char*>(&p)[sizeof(p)-1]; \
  for (unsigned int i=2; i<=sizeof(p); ++i) \
    vcl_cout<<vcl_setfill('0')<<vcl_setw(2)<<(short)(reinterpret_cast<unsigned char*>(&p))[sizeof(p)-i]; \
  vcl_cout<<vcl_dec


template <class T> void test_na_type(T na_v, T qnan_v)
{
  TEST("isnan(NaN)", vnl_math::isnan(qnan_v), true);
  TEST("isnan(NA)", vnl_math::isnan(vnl_na(T())), true);
  TEST("isnan(NA2)", vnl_math::isnan(na_v), true);
  TEST("isnan(1/NA2)", vnl_math::isnan(1.0f/na_v), true);
  TEST("isna(NA)", vnl_na_isna(vnl_na(T())), true);
  TEST("isna(NA2)", vnl_na_isna(na_v), true);
  TEST("isna(1/NA2)", vnl_na_isna(1.0f/na_v), true);
  TEST("!isfinite(NA)", !vnl_math::isfinite(na_v), true);
  TEST("!isinf(NA)", !vnl_math::isinf(na_v), true);

  TEST("!isna(0)", !vnl_na_isna(0.0), true);
  TEST("!isna(-0)", !vnl_na_isna(-0.0), true);
  TEST("!isna(-1.0)", !vnl_na_isna(-1.0), true);
  TEST("!isna(inf)", !vnl_na_isna(vcl_numeric_limits<T>::infinity()), true);
  TEST("!isna(NaN)", !vnl_na_isna(qnan_v), true);
  TEST("!isna(-NaN)", !vnl_na_isna(-qnan_v), true);
  TEST("isna(nan_to_na(NaN))", vnl_na_isna(vnl_na_nan_to_na(qnan_v)), true);
  TEST("!isna(nan_to_na(4.0))", !vnl_na_isna(vnl_na_nan_to_na(T(4.0))), true);
  TEST("nan_to_na(4.0)", vnl_na_nan_to_na(T(4.0)), T(4.0));
  TEST("nan_to_na(-inf)", vnl_na_nan_to_na(-vcl_numeric_limits<T>::infinity()), -vcl_numeric_limits<T>::infinity());
  TEST("isna(nan_to_na(NaN))", vnl_na_isna(vnl_na_nan_to_na(qnan_v)), true);
  TEST("!isna(nan_to_na(4.0))", !vnl_na_isna(vnl_na_nan_to_na(T(4.0))), true);

  {
    T x=0.0;
    vcl_istringstream ss("NA");
    vnl_na_extract(ss, x);
    TEST("x=\"NA\"", vnl_na_isna(x), true);
    vcl_cout << "x = " << x << " = " << print_hex(x) << vcl_endl;
  }

  {
    T x=0.0;
    vcl_istringstream ss("NA  ");
    vnl_na_extract(ss, x);
    TEST("x=\"NA  \"", vnl_na_isna(x), true);
    vcl_cout << "x = " << x << " = " << print_hex(x) << vcl_endl;
  }

  {
    T x=0.0;
    vcl_istringstream ss("1.0   ");
    vnl_na_extract(ss, x);
    TEST("x=\"1.0\"", x, 1.0);
    vcl_cout << "x = " << x << " = " << print_hex(x) << vcl_endl;
  }

  {
    T x=0.0, y=0.0;
    vcl_istringstream ss("NA1.0");
    vnl_na_extract(ss, x);
    vnl_na_extract(ss, y);
    TEST("x,y=\"NA1.0\"", vnl_na_isna(x) && y==1.0, true);
    vcl_cout << "x = " << x << " = " << print_hex(x) << vcl_endl;
  }

  {
    T x=0.0, y=0.0;
    vcl_istringstream ss("1.0NA");
    vnl_na_extract(ss, x);
    vnl_na_extract(ss, y);
    TEST("x,y=\"1.0NA\"", vnl_na_isna(y) && x==1.0, true);
    vcl_cout << "y = " << x << " = " << print_hex(x) << vcl_endl;
  }

  {
    T x=0.0, y=0.0;
    vcl_istringstream ss("NANA");
    vnl_na_extract(ss, x);
    vnl_na_extract(ss, y);
    TEST("x,y=\"NANA\"", vnl_na_isna(x) && vnl_na_isna(y), true);
    vcl_cout << "x = " << x << " = " << print_hex(x) << vcl_endl;
  }

  {
    T x=0.0, y=0.0;
    vcl_istringstream ss("NA 1.0");
    vnl_na_extract(ss, x);
    vnl_na_extract(ss, y);
    TEST("x,y=\"NA 1.0\"", vnl_na_isna(x) && y==1.0, true);
    vcl_cout << "x = " << x << " = " << print_hex(x) << vcl_endl;
  }

  {
    T x=0.0, y=0.0;
    vcl_istringstream ss("1.0 NA");
    vnl_na_extract(ss, x);
    vnl_na_extract(ss, y);
    TEST("x,y=\"1.0 NA\"", vnl_na_isna(y) && x==1.0, true);
    vcl_cout << "y = " << y << " = " << print_hex(y) << vcl_endl;
  }

  {
    T x=0.0, y=0.0;
    vcl_istringstream ss("NA NA");
    vnl_na_extract(ss, x);
    vnl_na_extract(ss, y);
    TEST("x,y=\"NA NA\"", vnl_na_isna(x) && vnl_na_isna(y), true);
    vcl_cout << "x = " << x << " = " << print_hex(x) << vcl_endl;
  }

  {
    T x=0.0, y=0.0;
    vcl_istringstream ss("-1.0-1.0");
    vnl_na_extract(ss, x);
    vnl_na_extract(ss, y);
    TEST("x,y=\"-1.0-1.0\"", x==-1.0 && y==-1.0, true);
    vcl_cout << "x = " << x << " = " << print_hex(x) << vcl_endl;
  }

  {
    vcl_ostringstream ss;
    vnl_na_insert(ss, -1.5);
    vnl_na_insert(ss, vnl_na(T()));
    TEST("output \"-1.5NA\"", ss.str(), "-1.5NA");
    vcl_cout << "ss = " << ss.str() << vcl_endl;
  }

  {
    vcl_stringstream ss;
    ss << vnl_na_stream(-1.0) << ' ' << vnl_na_stream(vnl_na(T()));
    T x=0.0, y=0.0;
    ss >> vnl_na_stream(x) >> vnl_na_stream(y);
    TEST("x,y=\"-1.0 NA\"", vnl_na_isna(y) && x==-1.0, true);
    vcl_cout << "y = " << y << " = " << print_hex(y) << '\n'
             << "ss = " << ss.str() << vcl_endl;
  }
}

void test_na()
{
  // Create NaN, NA
  double qnan_d = vcl_numeric_limits<double>::quiet_NaN();
  double na_d = vnl_na(double());

  float qnan_f = vcl_numeric_limits<float>::quiet_NaN();
  float na_f = vnl_na(float());

#define print_hex(p) \
  vcl_hex<<vcl_setfill('0')<<vcl_setw(2)<<(short)reinterpret_cast<unsigned char*>(&p)[sizeof(p)-1]; \
  for (unsigned int i=2; i<=sizeof(p); ++i) \
    vcl_cout<<vcl_setfill('0')<<vcl_setw(2)<<(short)(reinterpret_cast<unsigned char*>(&p))[sizeof(p)-i]; \
  vcl_cout<<vcl_dec

  vcl_cout << "qnan_d = " << qnan_d << " = " << print_hex(qnan_d) << '\n'
           << "na_d   = " << na_d   << " = " << print_hex(na_d)   << '\n'
           << "qnan_f = " << qnan_f << " = " << print_hex(qnan_f) << '\n'
           << "na_f   = " << na_f   << " = " << print_hex(na_f)   << '\n'
           << vcl_endl;

  vcl_cout << "\nSingle precision\n";
  test_na_type(na_f, qnan_f);
  vcl_cout << "\nDouble precision\n";
  test_na_type(na_d, qnan_d);


#undef print_hex

  //  It would be great if this test passed - but it never will.
  //  Just be explicit about it:
  TEST("static_cast<NA float> is not NA double", vnl_na_isna(static_cast<double>(na_f)), false);
}

TESTMAIN(test_na);
