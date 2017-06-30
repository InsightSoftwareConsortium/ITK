/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkMemoryProbesCollectorBase.h"
#include "itkTestingMacros.h"
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
    std::cout << "WARNING: Total memory usage should be greater than zero"
              << "Memory Probes do not work on this platform"
              << std::endl;
    delete[] buf;
    return EXIT_SUCCESS;
    }
  mcollecter.Report();
  delete[] buf;


  TRY_EXPECT_EXCEPTION(mcollecter.GetProbe("IDoNotExist"));
  TRY_EXPECT_NO_EXCEPTION(mcollecter.GetProbe("Update"));

  return EXIT_SUCCESS;
}
