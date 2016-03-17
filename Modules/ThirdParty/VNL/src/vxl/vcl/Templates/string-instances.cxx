#include <vcl_string.hxx>
#include <vcl_iostream.h>

VCL_BASIC_STRING_INSTANTIATE(char, vcl_char_traits<char> );

// this should work for all compilers. by putting it in the
// library we (a) get the implicit template instances it
// needs and (b) make sure that it does work.
static void vcl_string_instance_tickler(vcl_ostream &os, vcl_string::iterator i, char *a, char const *b)
{
  char ch;
  vcl_char_traits<char>::eq(ch, ch);
  vcl_string s(b, b);
  os << s;
  s.replace(i, i, a, a);
  s.replace(i, i, b, b);
  //s.find(i);
  s.find(a);
  s.find(b);

  vcl_string(s.begin()+3, s.end()) == b;
  s + s;
  vcl_string_instance_tickler(os, i, a, b); // avoid "unused function" warning
}

#endif
