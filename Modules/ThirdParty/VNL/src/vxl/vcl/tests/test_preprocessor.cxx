// Try to check that compiler preprocessor definitions are sane.

#include <vcl_compiler.h>
#include <vcl_iostream.h>

int test_preprocessor_main(int /*argc*/,char* /*argv*/[])
{
  unsigned compiler_count = 0;
  unsigned major_count = 0;
  unsigned minor_count = 0;

#ifdef VCL_GCC
  ++compiler_count;
#endif

#ifdef VCL_GCC_4
  ++major_count;
#endif
#ifdef VCL_GCC_40
  ++minor_count;
#endif
#ifdef VCL_GCC_41
  ++minor_count;
#endif
#ifdef VCL_GCC_42
  ++minor_count;
#endif
#ifdef VCL_GCC_43
  ++minor_count;
#endif
#ifdef VCL_GCC_44
  ++minor_count;
#endif

#ifdef VCL_GCC_5
  ++major_count;
#endif
#ifdef VCL_GCC_50
  ++minor_count;
#endif
#ifdef VCL_GCC_51
  ++minor_count;
#endif
#ifdef VCL_GCC_52
  ++minor_count;
#endif
#ifdef VCL_GCC_53
  ++minor_count;
#endif

#ifdef VCL_GCC_6
  ++major_count;
#endif
#ifdef VCL_GCC_60
  ++minor_count;
#endif
#ifdef VCL_GCC_61
  ++minor_count;
#endif
#ifdef VCL_GCC_62
  ++minor_count;
#endif
#ifdef VCL_GCC_63
  ++minor_count;
#endif

#ifdef VCL_VC
  ++compiler_count;
#endif

#ifdef VCL_ICC
  ++compiler_count;
#endif

  int result = 0;

  vcl_cout << "Compiler brand uniquely identified: ";
  if ( compiler_count == 1 ) {
    vcl_cout << "PASSED\n";
  } else if ( compiler_count < 1 ) {
    result = 1;
    vcl_cout << "FAILED\n"
             << "This compiler is not recognized by vcl_compiler.h.\n"
             << "Please contact the VXL maintainers and ask them\n"
             << "to fix it. (vxl-maintainers@lists.sourceforge.net)\n";
  } else if ( compiler_count > 1 ) {
    result = 1;
    vcl_cout << "FAILED\n"
             << "This compiler is recognized as multiple compilers\n"
             << "by vcl_compiler.h.\n"
             << "Please contact the VXL maintainers and ask them\n"
             << "to fix it. (vxl-maintainers@lists.sourceforge.net)\n";
  }

  vcl_cout << "Compiler release identified: ";
  if ( major_count == 1 ) {
    vcl_cout << "PASSED\n";
  } else if ( major_count > 1 ) {
    result = 1;
    vcl_cout << "FAILED\n"
             << "This release is recognized as multiple releases\n"
             << "by vcl_compiler.h.\n"
             << "Please contact the VXL maintainers and ask them\n"
             << "to fix it. (vxl-maintainers@lists.sourceforge.net)\n";
  } else {
    vcl_cout << "(not identified) PASSED\n";
  }

  vcl_cout << "Compiler version identified: ";
  if ( minor_count == 1 ) {
    vcl_cout << "PASSED\n";
  } else if ( minor_count > 1 ) {
    result = 1;
    vcl_cout << "FAILED\n"
             << "This version is recognized as multiple versions\n"
             << "by vcl_compiler.h.\n"
             << "Please contact the VXL maintainers and ask them\n"
             << "to fix it. (vxl-maintainers@lists.sourceforge.net)\n";
  } else {
    vcl_cout << "(not identified) PASSED\n";
  }

  vcl_cout <<  "Version identified implies release identified: ";
  if ( minor_count>0 && major_count==0 ) {
    result = 1;
    vcl_cout << "FAILED\n"
             << "This compiler defines a flag for the compiler version\n"
             << "(minor version) without defining a flag for the\n"
             << "corresponding release (major version)\n";
  } else {
    vcl_cout << "PASSED\n";
  }

  return result;
}
