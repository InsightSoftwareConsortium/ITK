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
#include "itkAbsImageFilter.h"
#include <type_traits>
#include <set>

using ThreaderType = itk::MultiThreaderBase::ThreaderType;

bool
checkThreaderByName(ThreaderType expectedThreaderType)
{
  using ImageType = itk::Image<unsigned, 3>;
  // any filter type which does not manually specify threader type will do
  using FilterType = itk::AbsImageFilter<ImageType, ImageType>;
  FilterType::Pointer filter = FilterType::New();

  std::string realThreaderName = filter->GetMultiThreader()->GetNameOfClass();
  std::string expectedThreaderName =
    itk::MultiThreaderBase::ThreaderTypeToString(expectedThreaderType) + "MultiThreader";
  if (realThreaderName != expectedThreaderName)
  {
    std::cout << "ERROR: filter threader's name is " << realThreaderName << ", while expected threader name "
              << expectedThreaderName << std::endl;
    return false;
  }
  return true;
}

int
itkMultiThreaderTypeFromEnvironmentTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cout << "ERROR: known threader type required" << std::endl;
    return EXIT_FAILURE;
  }

  bool success = true;

  ThreaderType expectedThreaderType = itk::MultiThreaderBase::ThreaderTypeFromString(argv[1]);
  ThreaderType realThreaderType = itk::MultiThreaderBase::GetGlobalDefaultThreader();

  if (realThreaderType != expectedThreaderType)
  {
    std::cout << "ERROR: expected threader type " << expectedThreaderType << ", but got " << realThreaderType
              << " from MultiThreaderBase::GetGlobalDefaultThreader()" << std::endl;
    success = false;
  }

  success &= checkThreaderByName(expectedThreaderType);

  // check that developer's choice for default is respected
  std::set<ThreaderType> threadersToTest = { ThreaderType::Platform, ThreaderType::Pool };
#ifdef ITK_USE_TBB
  threadersToTest.insert(ThreaderType::TBB);
#endif // ITK_USE_TBB
  for (auto thType : threadersToTest)
  {
    itk::MultiThreaderBase::SetGlobalDefaultThreader(thType);
    success &= checkThreaderByName(thType);
  }

  // When implementing a new multi-threader:
  // 1. insert it into threadersToTest set
  // 2. add tests to Modules/Core/Common/test/CMakeLists.txt similarily to tests for other multi-threaders
  // 3. rewrite the condition below to use whatever is really the last threader type
  itkAssertOrThrowMacro(ThreaderType::TBB == ThreaderType::Last,
                        "All multi-threader implementation have to be tested!");

  if (success)
  {
    std::cout << "Test PASSED!" << std::endl;
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }
}
