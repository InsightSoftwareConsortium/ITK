/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkMultiThreaderBase.h"
#include <cstdlib>
#include "itkCommand.h"
#include "itkAbsImageFilter.h"

class ShowProgress : public itk::Command
{
public:
  itkNewMacro(ShowProgress);

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * caller, const itk::EventObject & event) override
  {
    if (!itk::ProgressEvent().CheckEvent(&event))
    {
      return;
    }
    const auto * processObject = dynamic_cast<const itk::ProcessObject *>(caller);
    if (!processObject)
    {
      return;
    }
    std::cout << " " << processObject->GetProgress();
  }
};

int
itkMultiThreaderParallelizeArrayTest(int argc, char * argv[])
{
  itk::MultiThreaderBase::Pointer mt = itk::MultiThreaderBase::New();
  if (mt.IsNull())
  {
    std::cerr << "MultiThreaderBase could not be instantiated!" << std::endl;
    return EXIT_FAILURE;
  }
  if (argc >= 2)
  {
    unsigned int workUnitCount = static_cast<unsigned int>(std::stoi(argv[1]));
    mt->SetNumberOfWorkUnits(workUnitCount);
  }

  constexpr unsigned int    size = 1029;
  std::vector<unsigned int> vec(size);

  using SomeProcessObject = itk::AbsImageFilter<itk::Image<char>, itk::Image<char>>;
  auto progressPO = SomeProcessObject::New();
  auto showProgress = ShowProgress::New();
  progressPO->AddObserver(itk::ProgressEvent(), showProgress);
  mt->ParallelizeArray(
    1, size, [&vec](int i) { vec[i] = i; }, progressPO);

  int result = EXIT_SUCCESS;
  if (vec[0] != 0)
  {
    std::cerr << "vec[0] was modified!" << std::endl;
    result = EXIT_FAILURE;
  }

  for (unsigned int i = 1; i < size; ++i)
  {
    if (vec[i] != i)
    {
      std::cerr << "vec[" << i << "] is not " << i << ", but " << vec[i] << std::endl;
      result = EXIT_FAILURE;
    }
  }

  if (result != EXIT_FAILURE)
  {
    std::cout << "\nTest PASSED" << std::endl;
  }
  return result;
}
