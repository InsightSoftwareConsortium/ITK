// include all the streams headers and <string>, to
// ensure they are compatible.
#include <string>
#include <ios>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <sstream>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

// This appears to do nothing, and it doesn't compile on MSVC with some weird error
//      'flux' : illegal member initialization: 'fstream' is not a base or member
#ifndef _MSC_VER
struct flux : public std::fstream
{
  // check that bitwise OR of {openmode}s works.
  flux(std::ios::openmode mode = std::ios::in | std::ios::binary)
    : std::fstream("/tmp/flux", mode) { }
};
#endif

int test_iostream_main(int /*argc*/,char* /*argv*/[])
{
  std::cout << std::string("hello, vcl") << std::endl
           << std::oct <<  01000 << std::endl
           << std::hex << 0x1000 << std::endl
           << std::dec <<   1000 << std::endl
           << std::endl;

  // I/O formatting
  std::cin.flags(std::ios::skipws | std::ios::boolalpha);
  std::cout.unsetf(std::ios::dec);
  std::ios::fmtflags flgs =
    std::cout.setf(std::ios::uppercase |
                  std::ios::showbase |
                  std::ios::showpos |
                  std::ios::showpoint);
  std::cout.setf(std::ios::oct, std::ios::basefield);
  std::cout.setf(std::ios::scientific, std::ios::floatfield);
  std::cout.setf(std::ios::left, std::ios::adjustfield);
  std::cout << "Scientific, precision=2, width=20, pad_right : [";
  std::cout.precision(2); std::cout.width(20); std::cout.fill('x');
  // Note that precision() only applies to the next numeric or string entry!
  std::cout << 27182.81828 << "] oct " << 10 << std::endl;
  std::cout.flags(std::ios::showbase | std::ios::showpoint);
  std::cout.setf(std::ios::hex, std::ios::basefield);
  std::cout.setf(std::ios::fixed, std::ios::floatfield);
  std::cout.setf(std::ios::right, std::ios::adjustfield);
  std::cout << "Fixed,      precision=2, width=20, pad_left  : [";
  std::cout.width(20);
  std::cout << 27182.81828 << "] hex " << 10 << std::endl;
  std::cout.flags(flgs); // restore
  std::cout.setf(std::ios::showpos);
  std::cout.setf((std::ios::fmtflags)0, std::ios::floatfield);
  std::cout.setf(std::ios::internal, std::ios::adjustfield);
  std::cout << "Default,    precision=2, width=20, pad_intern: [";
  std::cout.precision(2); std::cout.width(20);
  std::cout << 27182.81828 << "] dec " << 10 << std::endl << std::endl;

  // Now the same output, using manipulators from <iomanip> :
  if (false) std::cin >> std::ws >> std::boolalpha;
  std::cout << std::resetiosflags(std::ios::dec)
           << std::uppercase << std::showbase << std::showpos << std::showpoint
           << std::oct << std::scientific << std::left
           << "Scientific, precision=2, width=20, pad_right : ["
           << std::setprecision(2) << std::setw(20) << std::setfill('x')
           << 27182.81828 << "] oct " << 10 << std::endl
           << std::nouppercase << std::noshowpos
           << std::hex << std::fixed << std::right
           << "Fixed,      precision=2, width=20, pad_left  : ["
           << std::setw(20)
           << 27182.81828 << "] hex " << 10 << std::endl
           << std::noshowbase << std::showpos << std::noshowpoint
           << std::resetiosflags(std::ios::fixed | std::ios::scientific)
           << std::resetiosflags(std::ios::right | std::ios::left)
           << std::dec << std::internal
           << "Default,    precision=2, width=20, pad_intern: ["
           << std::setprecision(2) << std::setw(20)
           << 27182.81828 << "] dec " << 10 << std::endl << std::endl;

  std::streampos a = std::cin.tellg();
  std::streampos b = std::cout.tellp();
  a = b; b = a; // quell warning about unused vars. compilers are sooo gullible.

  std::streambuf *ptr = nullptr;
  if (ptr) // quell warning.
    ++ ptr;

  std::streamsize size = 3141;
  ++ size; // quell warning.

  if (false) {
    int x;
    std::cin >> x; // read from stdin [27.3.1.2]
    std::cout << "cout goes to stdout [27.3.1.3]" << std::endl;
    std::cerr << "cerr goes to stderr [27.3.1.4]" << std::endl;
    std::clog << "clog goes to stderr [27.3.1.5]" << std::endl;
  }

  if (false) {
    std::ofstream f("dont_worry_this_file_is_not_created",
                   std::ios::in |
                   std::ios::out |
                   std::ios::ate |
                   std::ios::app |
                   std::ios::trunc |
                   std::ios::binary);

    f.write("hello, file", 11);
    f.seekp(10);
    f.seekp(-2, std::ios::cur);
    f.seekp(1, std::ios::beg);
    f.seekp(-1, std::ios::end);
    f.close();
  }

  if (false) {
    signed char sc;
    std::cin >> sc;

    bool bb;
    std::cin >> bb;
  }

  std::stringstream s(std::ios::in | std::ios::out | std::ios::binary);

  return !s;
}
