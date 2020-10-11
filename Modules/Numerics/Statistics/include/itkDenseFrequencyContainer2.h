/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkDenseFrequencyContainer2_h
#define itkDenseFrequencyContainer2_h

#include <map>
#include "itkValarrayImageContainer.h"
#include "itkMeasurementVectorTraits.h"
#include "ITKStatisticsExport.h"

namespace itk
{
namespace Statistics
{
/**
 *\class DenseFrequencyContainer2
 *  \brief This class is a container for frequencies of bins in an histogram.
 *
 * This class uses the ValarrayImageContainer class to store
 * frequencies. If the histogram has many zero frequency bins.
 * use SparseFrequencyContainer.  You should access each bin
 * by (InstanceIdentifier)index or measurement vector.
 *
 * \sa Histogram, SparseFrequencyContainer
 * \ingroup ITKStatistics
 */

class ITKStatistics_EXPORT DenseFrequencyContainer2 : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DenseFrequencyContainer2);

  /** Standard class type aliases */
  using Self = DenseFrequencyContainer2;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(DenseFrequencyContainer2, Object);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InstanceIdenfitifer type alias */
  using InstanceIdentifier = MeasurementVectorTraits::InstanceIdentifier;

  /** Absoluate Frequency type alias */
  using AbsoluteFrequencyType = MeasurementVectorTraits::AbsoluteFrequencyType;

  /** Absolute Total frequency type */
  using TotalAbsoluteFrequencyType = MeasurementVectorTraits::TotalAbsoluteFrequencyType;

  /** Relative Frequency type alias */
  using RelativeFrequencyType = MeasurementVectorTraits::RelativeFrequencyType;

  /** Relative Total frequency type */
  using TotalRelativeFrequencyType = MeasurementVectorTraits::TotalRelativeFrequencyType;

  /** Internal storage class type alias */
  using FrequencyContainerType = ValarrayImageContainer<InstanceIdentifier, AbsoluteFrequencyType>;

  using FrequencyContainerPointer = FrequencyContainerType::Pointer;

  /** Calls the Initialize method of superclass to generate the offset table
   * and prepare the frequency container */
  void
  Initialize(SizeValueType length);

  /** Calls the SetToZero method of superclass to initialize all the bins to Zero.
   *  This should be done before starting to call the IncreaseFrequency method. */
  void
  SetToZero();

  /** Sets the frequency of histogram using instance identifier. It returns
   * false when the Id is out of bounds. */
  bool
  SetFrequency(const InstanceIdentifier id, const AbsoluteFrequencyType value);

  /** Increases the frequency of a bin specified by the InstanceIdentifier by
   * one.  This function is convenient to create a histogram. It returns false
   * when the bin id is out of bounds. */
  bool
  IncreaseFrequency(const InstanceIdentifier id, const AbsoluteFrequencyType value);

  /** Method to get the frequency of a bin from the histogram. It returns zero
   * when the Id is out of bounds. */
  AbsoluteFrequencyType
  GetFrequency(const InstanceIdentifier id) const;

  /** Gets the sum of the frequencies */
  TotalAbsoluteFrequencyType
  GetTotalFrequency() const
  {
    return m_TotalFrequency;
  }

protected:
  DenseFrequencyContainer2();
  ~DenseFrequencyContainer2() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Internal storage */
  FrequencyContainerPointer  m_FrequencyContainer;
  TotalAbsoluteFrequencyType m_TotalFrequency;
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#endif
