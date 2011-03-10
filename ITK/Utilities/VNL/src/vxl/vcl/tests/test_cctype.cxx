#include <vcl_cctype.h>

// Test the functionality, and also cause a link to make sure the
// function exists.

int test_cctype_main(int /*argc*/,char* /*argv*/[])
{
  return ! ( vcl_isspace(' ') && vcl_isspace('\n') && !vcl_isspace('a') &&
             vcl_isalnum('1') && vcl_isalnum('z') && !vcl_isalnum('@') &&
             vcl_isdigit('4') && !vcl_isdigit('k') && !vcl_isdigit('%') &&
             vcl_isprint(' ') && vcl_isprint('(') && !vcl_isprint('\n') &&
             vcl_isupper('A') && !vcl_isupper('a') && !vcl_isupper('1') &&
             vcl_islower('b') && !vcl_islower('G') && !vcl_islower('8') &&
             vcl_isalpha('A') && vcl_isalpha('a') && !vcl_isalpha('1') &&
             vcl_isgraph('%') && vcl_isgraph('j') && !vcl_isgraph(' ') &&
             vcl_ispunct('&') && !vcl_ispunct('a') && !vcl_ispunct(' ') &&
             vcl_isxdigit('8') && vcl_isxdigit('F') && vcl_isxdigit('f') && !vcl_isxdigit('g') &&
             vcl_iscntrl('\n') && vcl_iscntrl('\177') && !vcl_iscntrl('i') &&
             vcl_tolower('A')=='a' && vcl_tolower('a')=='a' && vcl_tolower('@')=='@' &&
             vcl_toupper('K')=='K' && vcl_toupper('j')=='J' && vcl_toupper('$')=='$' );
}
