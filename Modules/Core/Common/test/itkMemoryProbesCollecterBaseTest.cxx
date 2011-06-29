#include "itkMemoryProbesCollectorBase.h"
#include "itkMemoryProbe.h"

#if defined(TEST_WITH_SLEEP)
#if defined(_WIN32) && !defined(__CYGWIN__)
#include <windows.h>
#else
#include <unistd.h>
inline
void Sleep(unsigned int milleseconds)
{
  sleep(milleseconds / 1000);
}
#endif
#else
#define Sleep(x) // Empty
#endif //defined(TEST_WITH_SLEEP)

#include <stdlib.h>

int itkMemoryProbesCollecterBaseTest(int, char *[])
{
  const size_t megabyte = 1024L * 1024L;

  itk::MemoryProbesCollectorBase mcollecter;
  itk::MemoryProbe probe;
  mcollecter.Start("Update");
  Sleep(5000);
  mcollecter.Stop("Update");
  mcollecter.Report();
  mcollecter.Clear();
  mcollecter.Start("Update");
  probe.Start();
  char *buf = new char[megabyte];
  for(unsigned int i = 0; i < megabyte; i++)
    {
    buf[i] = static_cast<char>(i & 0xff);
    }
  Sleep(5000);
  mcollecter.Stop("Update");
  probe.Stop();
  size_t total = probe.GetTotal();
  std::cout << " Total Value " << probe.GetTotal() << std::endl;
  if(total == 0)
    {
    std::cout << "Total memory usage should be greater than zero" << std::endl;
    delete [] buf;
    return EXIT_FAILURE;
    }
  mcollecter.Report();
  delete [] buf;
  return EXIT_SUCCESS;
}
