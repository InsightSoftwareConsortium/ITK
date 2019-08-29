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

#include <iostream>
#include "itkSample.h"
#include "itkObjectFactory.h"
#include "itkMath.h"

namespace itk
{
namespace Statistics
{
namespace SampleTest
{

template <typename TMeasurementVector>
class MySample : public Sample<TMeasurementVector>
{
public:
  /** Standard class type alias. */
  using Self = MySample;

  using Superclass = Sample<TMeasurementVector>;

  using Pointer = SmartPointer<Self>;

  using ConstPointer = SmartPointer<const Self>;

  /** Standard macros */
  itkTypeMacro(MySample, Sample);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  using MeasurementVectorType = typename Superclass::MeasurementVectorType;

  using TotalAbsoluteFrequencyType = typename Superclass::TotalAbsoluteFrequencyType;

  using AbsoluteFrequencyType = typename Superclass::AbsoluteFrequencyType;

  using InstanceIdentifier = typename Superclass::InstanceIdentifier;

  /** Get the size of the sample (number of measurements) */
  InstanceIdentifier
  Size() const override
  {
    return static_cast<InstanceIdentifier>(m_Values.size());
  }

  /** Get the measurement associated with a particular
   * InstanceIdentifier. */
  const MeasurementVectorType &
  GetMeasurementVector(InstanceIdentifier id) const override
  {
    return m_Values[id];
  }

  /** Get the frequency of a measurement specified by instance
   * identifier. */
  AbsoluteFrequencyType
  GetFrequency(InstanceIdentifier id) const override
  {
    return m_Frequencies[id];
  }

  /** Get the total frequency of the sample. */
  TotalAbsoluteFrequencyType
  GetTotalFrequency() const override
  {
    TotalAbsoluteFrequencyType sum = NumericTraits<TotalAbsoluteFrequencyType>::ZeroValue();
    auto                       itr = m_Frequencies.begin();
    while (itr != m_Frequencies.end())
    {
      sum += *itr;
      ++itr;
    }
    return sum;
  }

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
    os << indent << m_Values.size() << std::endl;
    os << indent << m_Frequencies.size() << std::endl;
  }

  void
  AddMeasurementVector(const MeasurementVectorType & measure, AbsoluteFrequencyType frequency)
  {
    m_Values.push_back(measure);
    m_Frequencies.push_back(frequency);
  }

private:
  std::vector<TMeasurementVector> m_Values;

  std::vector<AbsoluteFrequencyType> m_Frequencies;
};

} // namespace SampleTest
} // namespace Statistics
} // namespace itk
int
itkSampleTest(int, char *[])
{

  constexpr unsigned int MeasurementVectorSize = 17;

  using MeasurementVectorType = itk::FixedArray<float, MeasurementVectorSize>;

  using SampleType = itk::Statistics::SampleTest::MySample<MeasurementVectorType>;

  SampleType::Pointer sample = SampleType::New();

  std::cout << sample->GetNameOfClass() << std::endl;
  std::cout << sample->SampleType::Superclass::GetNameOfClass() << std::endl;

  sample->Print(std::cout);

  sample->SetMeasurementVectorSize(MeasurementVectorSize); // for code coverage

  if (sample->GetMeasurementVectorSize() != MeasurementVectorSize)
  {
    std::cerr << "GetMeasurementVectorSize() Failed !" << std::endl;
    return EXIT_FAILURE;
  }


  std::cout << sample->Size() << std::endl;

  MeasurementVectorType measure;
  for (unsigned int i = 0; i < MeasurementVectorSize; i++)
  {
    measure[i] = 29 * i * i;
  }

  using AbsoluteFrequencyType = SampleType::AbsoluteFrequencyType;

  AbsoluteFrequencyType frequency = 17;

  sample->AddMeasurementVector(measure, frequency);

  MeasurementVectorType measureBack = sample->GetMeasurementVector(0);
  AbsoluteFrequencyType frequencyBack = sample->GetFrequency(0);

  if (frequencyBack != frequency)
  {
    std::cerr << "Error in GetFrequency()" << std::endl;
    return EXIT_FAILURE;
  }

  for (unsigned int j = 0; j < MeasurementVectorSize; j++)
  {
    if (itk::Math::NotExactlyEquals(measureBack[j], measure[j]))
    {
      std::cerr << "Error in Set/Get MeasurementVector()" << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << sample->GetTotalFrequency() << std::endl;

  // Test if an exception will be thrown if we try to resize the measurement vector
  // size
  try
  {
    sample->SetMeasurementVectorSize(MeasurementVectorSize + 1);
    std::cerr << "Exception should have been thrown since we are trying to resize\
                  non-resizeable measurement vector type "
              << std::endl;
    return EXIT_FAILURE;
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cerr << "Caughted expected exception: " << excp << std::endl;
  }

  return EXIT_SUCCESS;
}
