#include <iostream>
#include <cctype>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

// Test the functionality, and also cause a link to make sure the
// function exists.

int test_cctype_main(int /*argc*/,char* /*argv*/[])
{
  return ! ( std::isspace(' ') && std::isspace('\n') && !std::isspace('a') &&
             std::isalnum('1') && std::isalnum('z') && !std::isalnum('@') &&
             std::isdigit('4') && !std::isdigit('k') && !std::isdigit('%') &&
             std::isprint(' ') && std::isprint('(') && !std::isprint('\n') &&
             std::isupper('A') && !std::isupper('a') && !std::isupper('1') &&
             std::islower('b') && !std::islower('G') && !std::islower('8') &&
             std::isalpha('A') && std::isalpha('a') && !std::isalpha('1') &&
             std::isgraph('%') && std::isgraph('j') && !std::isgraph(' ') &&
             std::ispunct('&') && !std::ispunct('a') && !std::ispunct(' ') &&
             std::isxdigit('8') && std::isxdigit('F') && std::isxdigit('f') && !std::isxdigit('g') &&
             std::iscntrl('\n') && std::iscntrl('\177') && !std::iscntrl('i') &&
             std::tolower('A')=='a' && std::tolower('a')=='a' && std::tolower('@')=='@' &&
             std::toupper('K')=='K' && std::toupper('j')=='J' && std::toupper('$')=='$' );
}
