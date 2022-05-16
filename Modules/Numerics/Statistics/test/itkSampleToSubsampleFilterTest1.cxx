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

#include <iostream>
#include "itkListSample.h"
#include "itkSampleToSubsampleFilter.h"

namespace itk
{
namespace Statistics
{
namespace itkSampleToSubsampleFilter1Namespace
{

template <typename TSample>
class SubsamplerTester : public SampleToSubsampleFilter<TSample>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SubsamplerTester);

  /** Standard class type aliases. */
  using Self = SubsamplerTester;
  using Superclass = SampleToSubsampleFilter<TSample>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard macros */
  itkTypeMacro(SubsamplerTester, SampleToSubsampleFilter);
  itkNewMacro(Self);

protected:
  SubsamplerTester() = default;
  ~SubsamplerTester() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    this->Superclass::PrintSelf(os, indent);
    os << "Superclass = " << this->Superclass::GetNameOfClass() << std::endl;
  }

  void
  GenerateData() override
  {}

private:
};

} // namespace itkSampleToSubsampleFilter1Namespace
} // namespace Statistics
} // namespace itk


int
itkSampleToSubsampleFilterTest1(int, char *[])
{

  constexpr unsigned int MeasurementVectorSize = 17;

  using MeasurementVectorType = itk::FixedArray<float, MeasurementVectorSize>;

  using SampleType = itk::Statistics::ListSample<MeasurementVectorType>;

  using FilterType = itk::Statistics::itkSampleToSubsampleFilter1Namespace::SubsamplerTester<SampleType>;


  auto sample = SampleType::New();

  auto filter = FilterType::New();

  // Test GetInput() before setting the input
  if (filter->GetInput() != nullptr)
  {
    std::cerr << "GetInput() should have returned nullptr" << std::endl;
    return EXIT_FAILURE;
  }

  // Test GetOutput() before creating the output
  if (filter->GetOutput() == nullptr)
  {
    std::cerr << "GetOutput() should have returned NON-nullptr" << std::endl;
    return EXIT_FAILURE;
  }

  filter->SetInput(sample);

  if (filter->GetInput() != sample.GetPointer())
  {
    std::cerr << "GetInput() didn't matched SetInput()" << std::endl;
    return EXIT_FAILURE;
  }


  //
  // Exercise the Print() method
  //
  filter->Print(std::cout);


  filter->Update();


  // Exercise the GetNameOfClass() method in the
  // SampleToSubsampleFilter:
  std::cout << "Classname  " << filter->GetNameOfClass() << std::endl;
  std::cout << "Superclass " << filter->FilterType::Superclass::GetNameOfClass() << std::endl;

  std::cout << "Test Passed !" << std::endl;
  return EXIT_SUCCESS;
}
