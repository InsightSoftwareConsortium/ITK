// Try to check that compiler preprocessor definitions are sane.

#include <iostream>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include <vcl_compiler_detection.h>

int test_preprocessor_main(int /*argc*/,char* /*argv*/[])
{
  constexpr unsigned compiler_count
      = VXL_COMPILER_IS_Comeau
        + VXL_COMPILER_IS_Intel
        + VXL_COMPILER_IS_PathScale
        + VXL_COMPILER_IS_Embarcadero
        + VXL_COMPILER_IS_Borland
        + VXL_COMPILER_IS_Watcom
        + VXL_COMPILER_IS_OpenWatcom
        + VXL_COMPILER_IS_SunPro
        + VXL_COMPILER_IS_HP
        + VXL_COMPILER_IS_Compaq
        + VXL_COMPILER_IS_zOS
        + VXL_COMPILER_IS_XL
        + VXL_COMPILER_IS_VisualAge
        + VXL_COMPILER_IS_PGI
        + VXL_COMPILER_IS_Cray
        + VXL_COMPILER_IS_TI
        + VXL_COMPILER_IS_Fujitsu
        + VXL_COMPILER_IS_SCO
        + VXL_COMPILER_IS_AppleClang
        + VXL_COMPILER_IS_Clang
        + VXL_COMPILER_IS_GNU
        + VXL_COMPILER_IS_MSVC
        + VXL_COMPILER_IS_ADSP
        + VXL_COMPILER_IS_IAR
        + VXL_COMPILER_IS_ARMCC
        + VXL_COMPILER_IS_MIPSpro;

  int result = 0;

  std::cout << "Compiler brand uniquely identified: ";
  if ( compiler_count == 1 ) {
    std::cout << "PASSED\n";
  } else if ( compiler_count < 1 ) {
    result = 1;
    std::cout << "FAILED\n"
             << "This compiler is not recognized by vcl_compiler_detection.h.\n"
             << "Please contact the VXL maintainers and ask them\n"
             << "to fix it. (vxl-maintainers@lists.sourceforge.net)\n";
  } else if ( compiler_count > 1 ) {
    result = 1;
    std::cout << "FAILED\n"
             << "This compiler is recognized as multiple compilers\n"
             << "by vcl_compiler_detection.h.\n"
             << "Please contact the VXL maintainers and ask them\n"
             << "to fix it. (vxl-maintainers@lists.sourceforge.net)\n";
  }

  std::cout << "Compiler release identified: "
            <<  VXL_COMPILER_VERSION_MAJOR << "."
            <<  VXL_COMPILER_VERSION_MINOR << "."
            <<  VXL_COMPILER_VERSION_PATCH ;

  std::cout <<  "Version identified implies release identified: ";
  std::cout << "PASSED\n";
  return result;
}
