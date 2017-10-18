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
#ifndef itkSparseFrequencyContainer2_h
#define itkSparseFrequencyContainer2_h

#include <map>
#include "itkObjectFactory.h"
#include "itkObject.h"
#include "itkNumericTraits.h"
#include "itkMeasurementVectorTraits.h"
#include "ITKStatisticsExport.h"

namespace itk
{
namespace Statistics
{
/** \class SparseFrequencyContainer2
 *  \brief his class is a container for an histogram.
 *
 *  This class uses an map to store histogram. If your histogram is dense
 *  use DenseHistogram.  You should access each bin by
 * (InstanceIdentifier)index or measurement vector.
 * \ingroup ITKStatistics
 */

class ITKStatistics_EXPORT SparseFrequencyContainer2:public Object
{
public:
  /** Standard class typedefs. */
  typedef SparseFrequencyContainer2  Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Standard macros */
  itkTypeMacro(SparseFrequencyContainer2, Object);
  itkNewMacro(Self);

  /** instance idenfitifer alias */
  typedef MeasurementVectorTraits::InstanceIdentifier InstanceIdentifier;

  /** Absolute frequency type alias */
  typedef MeasurementVectorTraits::AbsoluteFrequencyType AbsoluteFrequencyType;

  /** Absolute Total frequency type */
  typedef MeasurementVectorTraits::TotalAbsoluteFrequencyType TotalAbsoluteFrequencyType;

  /** Relative frequency type alias */
  typedef MeasurementVectorTraits::RelativeFrequencyType RelativeFrequencyType;

  /** Relative Relative frequency type */
  typedef MeasurementVectorTraits::TotalRelativeFrequencyType TotalRelativeFrequencyType;

  /** Histogram typedef support */
  typedef std::map< InstanceIdentifier, AbsoluteFrequencyType > FrequencyContainerType;
  typedef FrequencyContainerType::const_iterator
  FrequencyContainerConstIterator;

  /** prepares the frequency container */
  void Initialize(SizeValueType length);

  /** Calls the SetToZero method of superclass to initialize all the bins to Zero.
   *  This should be done before starting to call the IncreaseFrequency method. */
  void SetToZero();

  /** Method to set the frequency of histogram using instance identifier. It
   * returns false when the Id is out of bounds */
  bool SetFrequency(const InstanceIdentifier id, const AbsoluteFrequencyType value);

  /** Method to increase the frequency by one.  This function is convinent
   * to create a histogram. It returns false when the id is out of bounds. */
  bool IncreaseFrequency(const InstanceIdentifier id,
                         const AbsoluteFrequencyType value);

  /** Method to get the frequency of a bin from the histogram. It will return
   * zero when the Id is out of bounds.  */
  AbsoluteFrequencyType GetFrequency(const InstanceIdentifier id) const;

  TotalAbsoluteFrequencyType GetTotalFrequency()
  {
    return m_TotalFrequency;
  }

protected:
  SparseFrequencyContainer2();
  virtual ~SparseFrequencyContainer2() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SparseFrequencyContainer2);

  // Container of histogram
  FrequencyContainerType     m_FrequencyContainer;
  TotalAbsoluteFrequencyType m_TotalFrequency;
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#endif
