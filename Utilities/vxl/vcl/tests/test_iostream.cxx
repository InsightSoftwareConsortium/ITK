// include all the streams headers and <string>, to
// ensure they are compatible.
#include <vcl_string.h>
#include <vcl_ios.h> // for vcl_ios_fixed etc.
#include <vcl_iomanip.h> // for vcl_fixed etc.
#include <vcl_iostream.h>
#include <vcl_fstream.h>
#include <vcl_sstream.h>

// This appears to do nothing, and it doesn't compile on MSVC with some weird error
//      'flux' : illegal member initialization: 'fstream' is not a base or member
#ifndef VCL_VC
struct flux : public vcl_fstream
{
  // check that bitwise OR of {openmode}s works.
  flux(vcl_ios_openmode mode = vcl_ios_in | vcl_ios_binary)
    : vcl_fstream("/tmp/flux", mode) { }
};
#endif

int test_iostream_main(int /*argc*/,char* /*argv*/[])
{
  vcl_cout << vcl_string("hello, vcl") << vcl_endl
           << vcl_oct <<  01000 << vcl_endl
           << vcl_hex << 0x1000 << vcl_endl
           << vcl_dec <<   1000 << vcl_endl
           << vcl_endl;

  // I/O formatting
  vcl_cin.flags(vcl_ios_skipws | vcl_ios_boolalpha);
  vcl_cout.unsetf(vcl_ios_dec);
  vcl_ios_fmtflags flgs =
    vcl_cout.setf(vcl_ios_uppercase |
                  vcl_ios_showbase |
                  vcl_ios_showpos |
                  vcl_ios_showpoint);
  vcl_cout.setf(vcl_ios_oct, vcl_ios_basefield);
  vcl_cout.setf(vcl_ios_scientific, vcl_ios_floatfield);
  vcl_cout.setf(vcl_ios_left, vcl_ios_adjustfield);
  vcl_cout << "Scientific, precision=2, width=20, pad_right : [";
  vcl_cout.precision(2); vcl_cout.width(20); vcl_cout.fill('x');
  // Note that precision() only applies to the next numeric or string entry!
  vcl_cout << 27182.81828 << "] oct " << 10 << vcl_endl;
  vcl_cout.flags(vcl_ios_showbase | vcl_ios_showpoint);
  vcl_cout.setf(vcl_ios_hex, vcl_ios_basefield);
  vcl_cout.setf(vcl_ios_fixed, vcl_ios_floatfield);
  vcl_cout.setf(vcl_ios_right, vcl_ios_adjustfield);
  vcl_cout << "Fixed,      precision=2, width=20, pad_left  : [";
  vcl_cout.width(20);
  vcl_cout << 27182.81828 << "] hex " << 10 << vcl_endl;
  vcl_cout.flags(flgs); // restore
  vcl_cout.setf(vcl_ios_showpos);
  vcl_cout.setf((vcl_ios_fmtflags)0, vcl_ios_floatfield);
  vcl_cout.setf(vcl_ios_internal, vcl_ios_adjustfield);
  vcl_cout << "Default,    precision=2, width=20, pad_intern: [";
  vcl_cout.precision(2); vcl_cout.width(20);
  vcl_cout << 27182.81828 << "] dec " << 10 << vcl_endl << vcl_endl;

  // Now the same output, using manipulators from <iomanip> :
  if (false) vcl_cin >> vcl_ws >> vcl_boolalpha;
  vcl_cout << vcl_resetiosflags(vcl_ios_dec)
           << vcl_uppercase << vcl_showbase << vcl_showpos << vcl_showpoint
           << vcl_oct << vcl_scientific << vcl_left
           << "Scientific, precision=2, width=20, pad_right : ["
           << vcl_setprecision(2) << vcl_setw(20) << vcl_setfill('x')
           << 27182.81828 << "] oct " << 10 << vcl_endl
           << vcl_nouppercase << vcl_noshowpos
           << vcl_hex << vcl_fixed << vcl_right
           << "Fixed,      precision=2, width=20, pad_left  : ["
           << vcl_setw(20)
           << 27182.81828 << "] hex " << 10 << vcl_endl
           << vcl_noshowbase << vcl_showpos << vcl_noshowpoint
           << vcl_resetiosflags(vcl_ios_fixed | vcl_ios_scientific)
           << vcl_resetiosflags(vcl_ios_right | vcl_ios_left)
           << vcl_dec << vcl_internal
           << "Default,    precision=2, width=20, pad_intern: ["
           << vcl_setprecision(2) << vcl_setw(20)
           << 27182.81828 << "] dec " << 10 << vcl_endl << vcl_endl;

  vcl_streampos a = vcl_cin.tellg();
  vcl_streampos b = vcl_cout.tellp();
  a = b; b = a; // quell warning about unused vars. compilers are sooo gullible.

  vcl_streambuf *ptr = 0;
  if (ptr) // quell warning.
    ++ ptr;

  vcl_streamsize size = 3141;
  ++ size; // quell warning.

  if (false) {
    int x;
    vcl_cin >> x; // read from stdin [27.3.1.2]
    vcl_cout << "cout goes to stdout [27.3.1.3]" << vcl_endl;
    vcl_cerr << "cerr goes to stderr [27.3.1.4]" << vcl_endl;
    vcl_clog << "clog goes to stderr [27.3.1.5]" << vcl_endl;
  }

  if (false) {
    vcl_ofstream f("dont_worry_this_file_is_not_created",
                   vcl_ios_in |
                   vcl_ios_out |
                   vcl_ios_ate |
                   vcl_ios_app |
                   vcl_ios_trunc |
                   vcl_ios_binary);

    f.write("hello, file", 11);
    f.seekp(10);
    f.seekp(-2, vcl_ios_cur);
    f.seekp(1, vcl_ios_beg);
    f.seekp(-1, vcl_ios_end);
    f.close();
  }

  if (false) {
    signed char sc;
    vcl_cin >> sc;

    bool bb;
    vcl_cin >> bb;
  }

  vcl_stringstream s(vcl_ios_in | vcl_ios_out | vcl_ios_binary);

  return !s;
}
