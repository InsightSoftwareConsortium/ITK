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
#ifndef itkMembershipSample_h
#define itkMembershipSample_h

#include <unordered_map>
#include "itkSubsample.h"

namespace itk
{
namespace Statistics
{
/**
 *\class MembershipSample
 * \brief Container for storing the instance-identifiers of other sample with
 * their associated class labels.
 *
 * This class does not store any measurement data. In a sense, you can
 * think it as an additional information to basic samples (such as Histogram,
 * PointSetListSampleAdaptor, and ImageToListSampleAdaptor). The additional
 * information is a class label for a measurement vector. Obviously without
 * such basic types of sample, this one is meaningless. You can call any
 * basic methods that has been defined in the Sample class such as
 * GetMeasurementVector and GetFrequency. You can query the class label for
 * an instance using an instance-identifier. Another new and important method
 * is the GetClassSample method. With a given class label, it returns a
 * pointer to the Subsample object that has all the instance-identifiers
 * of instances that belong to the class.
 *
 * This class is templated over the type of the basic sample. To use all
 * the method, you should first plug in a basic type sample using the
 * SetSample method
 *
 * \ingroup ITKStatistics
 *
 * \sphinx
 * \sphinxexample{Numerics/Statistics/CreateListOfSamplesWithIDs,Create List Of Samples With Associated ID's}
 * \endsphinx
 */

template <typename TSample>
class ITK_TEMPLATE_EXPORT MembershipSample : public DataObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MembershipSample);

  /** Standard class type aliases. */
  using Self = MembershipSample;
  using Superclass = DataObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard macros */
  itkTypeMacro(MembershipSample, DataObject);
  itkNewMacro(Self);

  /** Typedefs for Measurement vector, measurement, Instance Identifier,
   * frequency, size, size element value from the template argument TSample */
  using SampleType = TSample;
  using MeasurementVectorType = typename SampleType::MeasurementVectorType;
  using MeasurementType = typename SampleType::MeasurementType;
  using InstanceIdentifier = typename SampleType::InstanceIdentifier;
  using SampleConstPointer = typename SampleType::ConstPointer;

  using AbsoluteFrequencyType = typename SampleType::AbsoluteFrequencyType;
  using TotalAbsoluteFrequencyType = typename SampleType::TotalAbsoluteFrequencyType;

  using ClassLabelType = IdentifierType;
  /** vector of unique class labels that will be used for mapping internal
   * continuous class label with real class labels */
  using UniqueClassLabelsType = std::vector<ClassLabelType>;

  /** Typedef for the storage that holds a class label for each instance.
   * The relationship between instances and class label is one-to-one */
  using ClassLabelHolderType = std::unordered_map<InstanceIdentifier, ClassLabelType>;

  /** Typedef for each subsample that stores instance identifiers of instances
   * that belong to a class */
  using ClassSampleType = Subsample<SampleType>;
  using ClassSamplePointer = typename ClassSampleType::Pointer;
  using ClassSampleConstPointer = typename ClassSampleType::ConstPointer;

  /** Set/Get the actual sample data */
  itkSetConstObjectMacro(Sample, SampleType);
  itkGetConstObjectMacro(Sample, SampleType);

  /** Sets the number of classes (class labels) */
  void
  SetNumberOfClasses(unsigned int numberOfClasses);

  /** Gets the number of classes (class labels) */
  itkGetConstMacro(NumberOfClasses, unsigned int);

  /** Adds an instance from the source sample to this container. The
   * first argument is the class label for that instance. The second
   * argument is the instance identifier from the source identifier that
   * is going to be included this container. */
  void
  AddInstance(const ClassLabelType & classLabel, const InstanceIdentifier & id);

  /** Gets the class label for the instance that has the instance
   *   identifier, id. */
  unsigned int
  GetClassLabel(const InstanceIdentifier & id) const;

  /** Gets the Subsample that includes only the instances that belong
   *   to the classLabel. If classLabel does not exist, nullptr is returned. */
  const ClassSampleType *
  GetClassSample(const ClassLabelType & classLabel) const;

  /** Gets the class labels that corresponding to the each instance in
   *   this container. */
  const ClassLabelHolderType
  GetClassLabelHolder() const;

  /** returns the measurement of the instance which is identified
   * by the 'id' */
  const MeasurementVectorType &
  GetMeasurementVector(const InstanceIdentifier & id) const;

  /** returns the measurement element which is the 'n'-th element
   * in the 'd' dimension of the measurement vector */
  MeasurementType
  GetMeasurement(const InstanceIdentifier & id, const unsigned int & dimension);

  /** returns the frequency of the instance which is identified by the 'id' */
  AbsoluteFrequencyType
  GetFrequency(const InstanceIdentifier & id) const;

  /** returns the total frequency for the 'd' dimension */
  TotalAbsoluteFrequencyType
  GetTotalFrequency() const;

  /** Method to graft another sample */
  void
  Graft(const DataObject * thatObject) override;

  //  void PrintSelf(std::ostream& os, Indent indent) const;

  class ConstIterator
  {
    friend class MembershipSample;

  public:
    ConstIterator(const Self * sample) { *this = sample->Begin(); }

    ConstIterator(const ConstIterator & iter)
    {
      m_Sample = iter.m_Sample;
      m_MembershipSample = iter.m_MembershipSample;
      m_InstanceIdentifier = iter.m_InstanceIdentifier;
    }

    ConstIterator &
    operator=(const ConstIterator & iter)
    {
      m_Sample = iter.m_Sample;
      m_MembershipSample = iter.m_MembershipSample;
      m_InstanceIdentifier = iter.m_InstanceIdentifier;
      return *this;
    }

    bool
    operator!=(const ConstIterator & it) const
    {
      return (m_InstanceIdentifier != it.m_InstanceIdentifier);
    }

    bool
    operator==(const ConstIterator & it) const
    {
      return (m_InstanceIdentifier == it.m_InstanceIdentifier);
    }

    ConstIterator &
    operator++()
    {
      ++m_InstanceIdentifier;
      return *this;
    }

    AbsoluteFrequencyType
    GetFrequency() const
    {
      return m_Sample->GetFrequency(m_InstanceIdentifier);
    }

    const MeasurementVectorType &
    GetMeasurementVector() const
    {
      return m_Sample->GetMeasurementVector(m_InstanceIdentifier);
    }

    InstanceIdentifier
    GetInstanceIdentifier() const
    {
      return m_InstanceIdentifier;
    }

    unsigned int
    GetClassLabel() const
    {
      return m_MembershipSample->GetClassLabel(m_InstanceIdentifier);
    }

  protected:
    // Purposely not implemented
    ConstIterator();

    // Only to be called from the MembershipSample
    ConstIterator(const Self * memberSample, InstanceIdentifier iid)
      : m_Sample(memberSample->GetSample())
      , m_MembershipSample(memberSample)
      , m_InstanceIdentifier(iid)
    {}

    // typename SampleType::ConstIterator m_Iter;
    const TSample *          m_Sample;
    const MembershipSample * m_MembershipSample;
    InstanceIdentifier       m_InstanceIdentifier;
  };

  class Iterator : public ConstIterator
  {
    friend class MembershipSample;

  public:
    Iterator(Self * sample)
      : ConstIterator(sample)
    {}

    Iterator(const Iterator & iter)
      : ConstIterator(iter)
    {}

    Iterator &
    operator=(const Iterator & iter)
    {
      this->ConstIterator::operator=(iter);
      return *this;
    }

  protected:
    // To ensure const-correctness these method must not be in the public API.
    // The are purposly not implemented, since they should never be called.
    Iterator();
    Iterator(const Self * sample);
    Iterator(const ConstIterator & it);
    ConstIterator &
    operator=(const ConstIterator & it);

    // Only to be called from the MembershipSample
    Iterator(Self * memberSample, InstanceIdentifier iid)
      : ConstIterator(memberSample, iid)
    {}

  private:
  };

  /** This method returns an iterator to the beginning of the
      measurement vectors */
  Iterator
  Begin()
  {
    Iterator iter(this, 0);

    return iter;
  }

  /** This method returns an iterator to the beginning of the
      measurement vectors */
  Iterator
  End()
  {
    Iterator iter(this, m_Sample->Size());

    return iter;
  }

  ConstIterator
  Begin() const
  {
    ConstIterator iter(this, 0);

    return iter;
  }

  ConstIterator
  End() const
  {
    ConstIterator iter(this, m_Sample->Size());

    return iter;
  }

protected:
  MembershipSample();
  ~MembershipSample() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Gets the internal continuous class label from the class labels that
   *   are used for AddInstance method. */
  int
  GetInternalClassLabel(const ClassLabelType classLabel) const;

  UniqueClassLabelsType           m_UniqueClassLabels;
  ClassLabelHolderType            m_ClassLabelHolder;
  std::vector<ClassSamplePointer> m_ClassSamples;
  SampleConstPointer              m_Sample;
  unsigned int                    m_NumberOfClasses;
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMembershipSample.hxx"
#endif

#endif
