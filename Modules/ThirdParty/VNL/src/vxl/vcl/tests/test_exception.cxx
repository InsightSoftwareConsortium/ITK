// This is vcl/tests/test_exception.cxx
#include <iostream>
#include <exception>

int
test_exception_main(int /*argc*/, char * /*argv*/[])
{
  const char * const ex = "\"const char* exception\"";
  int result = 0;
  try
  {
    std::cout << "throw " << ex << std::endl;
    throw ex;
  }
  catch (const char * e)
  {
    std::cout << "caught " << e << ".  Good." << std::endl;
    result = 0;
  }
  catch (...)
  {
    std::cout << "caught nothing.  Bad." << std::endl;
    result = 1;
  }
  return result;
}
