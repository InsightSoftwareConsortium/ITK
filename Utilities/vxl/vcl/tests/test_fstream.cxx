#include <vcl_fstream.h>

int test_fstream_main(int /*argc*/,char* /*argv*/[])
{
  if (false) {
    vcl_fstream f("dont_worry_this_file_is_not_created", vcl_ios_out | vcl_ios_binary);

    f.write("hello, file", 11);

    f.seekp(0);
    f.seekg(0);

    f.close();
  }
  return 0;
}
