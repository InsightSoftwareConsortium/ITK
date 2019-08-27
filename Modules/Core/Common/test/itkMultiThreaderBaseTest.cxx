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

#include "itkMultiThreaderBase.h"
#include "itkPlatformMultiThreader.h"
#include "itkPoolMultiThreader.h"
#ifdef ITK_USE_TBB
#  include "itkTBBMultiThreader.h"
#endif
#include "itkTestingMacros.h"

bool
VerifyRange(int value, int min, int max, const char * msg)
{
  if (value < min)
  {
    std::cerr << msg << std::endl;
    return false;
  }

  if (value > max)
  {
    std::cerr << msg << std::endl;
    return false;
  }
  return true;
}


bool
SetAndVerifyGlobalMaximumNumberOfThreads(int value)
{
  itk::MultiThreaderBase::SetGlobalMaximumNumberOfThreads(value);
  return VerifyRange(itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads(),
                     1,
                     itk::ITK_MAX_THREADS,
                     "Range error in MaximumNumberOfThreads");
}

bool
SetAndVerifyGlobalDefaultNumberOfThreads(int value)
{
  itk::MultiThreaderBase::SetGlobalDefaultNumberOfThreads(value);
  return VerifyRange(itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads(),
                     1,
                     itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads(),
                     "Range error in DefaultNumberOfThreads");
}

bool
SetAndVerifyMaximumNumberOfThreads(int value, itk::MultiThreaderBase * threader)
{
  threader->SetMaximumNumberOfThreads(value);
  return VerifyRange(threader->GetMaximumNumberOfThreads(),
                     1,
                     itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads(),
                     "Range error in MaximumNumberOfThreads");
}

bool
SetAndVerify(int number)
{
  bool result = true;
  result &= SetAndVerifyGlobalMaximumNumberOfThreads(number);
  result &= SetAndVerifyGlobalDefaultNumberOfThreads(number);
  itk::MultiThreaderBase::Pointer threader = itk::MultiThreaderBase::New();
  // PoolMultiThreader can only increase number of threads
  // so make sure to increase this before testing thread count
  itk::MultiThreaderBase::SetGlobalMaximumNumberOfThreads(itk::ITK_MAX_THREADS);
  result &= SetAndVerifyMaximumNumberOfThreads(number, threader);
  // number of Work Units is not max-limited by TBBMultiThreader
  return result;
}

#define TEST_SINGLE_CLASS(ClassName)                                                                                   \
  {                                                                                                                    \
    itk::ClassName::Pointer threader = itk::ClassName::New();                                                          \
    if (threader.IsNull())                                                                                             \
    {                                                                                                                  \
      result = false;                                                                                                  \
    }                                                                                                                  \
                                                                                                                       \
    ITK_EXERCISE_BASIC_OBJECT_METHODS(threader, ClassName, MultiThreaderBase);                                         \
  }

int
itkMultiThreaderBaseTest(int argc, char * argv[])
{
  // Choose a number of threads.
  int numberOfThreads = 10;
  if (argc > 1)
  {
    const int nt = std::stoi(argv[1]);
    if (nt > 1)
    {
      numberOfThreads = nt;
    }
  }

  bool result = true;
  TEST_SINGLE_CLASS(PlatformMultiThreader);
  TEST_SINGLE_CLASS(PoolMultiThreader);
#ifdef ITK_USE_TBB
  TEST_SINGLE_CLASS(TBBMultiThreader);
#endif

  itk::MultiThreaderBase::SetGlobalDefaultNumberOfThreads(numberOfThreads);

  result &= SetAndVerify(-1);
  result &= SetAndVerify(0);
  result &= SetAndVerify(1);
  result &= SetAndVerify(2);
  result &= SetAndVerify(itk::ITK_MAX_THREADS);
  result &= SetAndVerify(itk::ITK_MAX_THREADS - 1);
  result &= SetAndVerify(itk::ITK_MAX_THREADS + 1);
  result &= SetAndVerify(itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads());
  result &= SetAndVerify(itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads() - 1);
  result &= SetAndVerify(itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads() + 1);

  if (!result)
  {
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
