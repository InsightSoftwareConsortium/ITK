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
#ifndef itkSample_h
#define itkSample_h

#include "itkPoint.h"
#include "itkDataObject.h"
#include "itkMeasurementVectorTraits.h"
#include <vector> // for the size_type declaration

namespace itk
{
namespace Statistics
{
/**
 *\class Sample
 *  \brief A collection of measurements for statistical analysis
 *
 * Sample represents a set of measurements for statistical
 * analysis. Sample is templated over a measurement vector. The
 * measurement vector encapsulates the set of values associated with a
 * single measurement.  For instance, a measurement vector may contain
 * an image intensity of a pixel and the gradient magnitude at that pixel.
 *
 * Data within a sample can be accessed via an
 * InstanceIdentifier. InstanceIdentifiers have different forms and
 * meanings depending on the type of sample.  For ListSamples, the
 * InstanceIdentifier is an index into the corresponding list. In this
 * case, the InstanceIndentifier corresponds to a particular
 * measurement stored in the Sample. For Histograms, an
 * InstanceIdentifier corresponds to a particular bin in the
 * N-dimensional histogram. In other words, the InstanceIdentifier in
 * a histogram does not correspond to a specific measurement used to
 * build the histogram but to the "bin" in which a number of original
 * measurements were "accumulated".
 *
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * 'MeasurementVectorSize'  has been removed to allow the length of a measurement
 * vector to be specified at run time. Please use the function
 * GetMeasurementVectorSize() instead.
 *
 * \ingroup ITKStatistics
 */

template <typename TMeasurementVector>
class Sample : public DataObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(Sample);

  /** Standard class type aliases */
  using Self = Sample;
  using Superclass = DataObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(Sample, DataObject);

  /** MeasurementVector type alias support */
  using MeasurementVectorType = TMeasurementVector;

  /** ValueType of a measurement (ValueType of a component of the
   * MeasurementVector */
  using MeasurementType = typename MeasurementVectorTraitsTypes<MeasurementVectorType>::ValueType;

  /** Frequency value type */
  using AbsoluteFrequencyType = MeasurementVectorTraits::AbsoluteFrequencyType;

  /** Total frequency type */
  using TotalAbsoluteFrequencyType = NumericTraits<AbsoluteFrequencyType>::AccumulateType;

  /** InstanceIdentifier type alias. This identifier is a unique
   * sequential id for each measurement vector in a Sample subclass. */
  using InstanceIdentifier = typename MeasurementVectorTraits::InstanceIdentifier;

  /** Type of the length of each measurement vector */
  using MeasurementVectorSizeType = unsigned int;

  /** Get the size of the sample (number of measurements) */
  virtual InstanceIdentifier
  Size() const = 0;

  /** Get the measurement associated with a particular
   * InstanceIdentifier. */
  virtual const MeasurementVectorType &
  GetMeasurementVector(InstanceIdentifier id) const = 0;

  /** Get the frequency of a measurement specified by instance
   * identifier. */
  virtual AbsoluteFrequencyType
  GetFrequency(InstanceIdentifier id) const = 0;

  /** Get the total frequency of the sample. */
  virtual TotalAbsoluteFrequencyType
  GetTotalFrequency() const = 0;

  /** Set method for the length of the measurement vector */
  virtual void
  SetMeasurementVectorSize(MeasurementVectorSizeType s)
  {
    // Test whether the vector type is resizable or not
    MeasurementVectorType m;

    if (MeasurementVectorTraits::IsResizable(m))
    {
      // then this is a resizable vector type
      //
      // if the new size is the same as the previou size, just return
      if (s == this->m_MeasurementVectorSize)
      {
        return;
      }
      else
      {
        // If the new size is different from the current size, then
        // only change the measurement vector size if the container is empty.
        if (this->Size())
        {
          itkExceptionMacro("Attempting to change the measurement \
          vector size of a non-empty Sample");
        }
        else
        {
          this->m_MeasurementVectorSize = s;
          this->Modified();
        }
      }
    }
    else
    {
      // If this is a non-resizable vector type
      MeasurementVectorType     m3;
      MeasurementVectorSizeType defaultLength = NumericTraits<MeasurementVectorType>::GetLength(m3);
      // and the new length is different from the default one, then throw an
      // exception
      if (defaultLength != s)
      {
        itkExceptionMacro("Attempting to change the measurement \
                           vector size of a non-resizable vector type");
      }
    }
  }

  /** Get method for the length of the measurement vector */
  itkGetConstMacro(MeasurementVectorSize, MeasurementVectorSizeType);

  /** Method to graft another sample */
  void
  Graft(const DataObject * thatObject) override
  {
    this->Superclass::Graft(thatObject);

    const auto * thatConst = dynamic_cast<const Self *>(thatObject);
    if (thatConst)
    {
      this->SetMeasurementVectorSize(thatConst->GetMeasurementVectorSize());
    }
  }

protected:
  Sample() { m_MeasurementVectorSize = NumericTraits<MeasurementVectorType>::GetLength(MeasurementVectorType()); }

  ~Sample() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "Length of measurement vectors in the sample: " << m_MeasurementVectorSize << std::endl;
  }

private:
  MeasurementVectorSizeType m_MeasurementVectorSize;
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#endif
