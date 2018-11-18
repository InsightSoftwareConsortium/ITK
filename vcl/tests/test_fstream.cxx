#include <iostream>
#include <fstream>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

int test_fstream_main(int /*argc*/,char* /*argv*/[])
{
  if (false) {
    std::fstream f("dont_worry_this_file_is_not_created", std::ios::out | std::ios::binary);

    f.write("hello, file", 11);

    f.seekp(0);
    f.seekg(0);

    f.close();
  }
  return 0;
}
