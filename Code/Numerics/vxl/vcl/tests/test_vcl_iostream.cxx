// include all the streams headers and <string>, to 
// ensure they are compatible.
#include <vcl_string.h>
#include <vcl_iostream.h>
#include <vcl_fstream.h>
#include <vcl_strstream.h>

int main() 
{
  vcl_cout << vcl_string("hello, vcl") << vcl_endl
	   << vcl_hex << 0x1000 << vcl_endl
	   << vcl_dec <<   1000 << vcl_endl
	   << vcl_endl;
  
  vcl_streampos a = vcl_cin.tellg();
  vcl_streampos b = vcl_cout.tellp();
  a = b; b = a; // quell warning about unused vars. compilers are sooo gullible.

  vcl_streambuf *ptr = 0;
  if (ptr) // quell warning.
    ++ ptr;
  
  vcl_streamsize size = 3141;
  ++ size; // quell warning.

  if (false) {
    vcl_ofstream f("dont_worry_this_file_is_not_created", 
		   vcl_ios_in |
		   vcl_ios_out |
		   vcl_ios_ate |
		   vcl_ios_app |
		   vcl_ios_trunc |
		   vcl_ios_binary);
    
    f.write((char*)"hello, file", 11);
    f.close();
  }

  if (false) {
    signed char sc;
    vcl_cin >> sc;
    
    bool bb;
    vcl_cin >> bb;
  }

  return 0;
}
