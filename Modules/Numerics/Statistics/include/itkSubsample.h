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
#ifndef itkSubsample_h
#define itkSubsample_h

#include "itkSample.h"
#include "itkMacro.h"
#include "itkObjectFactory.h"

namespace itk
{
namespace Statistics
{
/** \class Subsample
 * \brief This class stores a subset of instance identifiers from another sample
 * object. You can create a subsample out of another sample object or another
 * subsample object. The class is useful when storing or extracting a portion
 * of a sample object. Note that when the elements of a subsample are sorted,
 * the instance identifiers of the subsample are sorted without changing the
 * original source sample. Most Statistics algorithms (that derive from
 * StatisticsAlgorithmBase accept Subsample objects as inputs).
 *
 * \ingroup ITKStatistics
 */
template< typename TSample >
class ITK_TEMPLATE_EXPORT Subsample:
  public TSample
{
public:
  /** Standard class typedefs */
  typedef Subsample                  Self;
  typedef TSample                    Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Subsample, TSample);

  /** standard New() method support */
  itkNewMacro(Self);

  /** Smart pointer to the actual sample data holder */
  typedef typename TSample::Pointer SamplePointer;

  /** Typedefs for Measurement vector, measurement, Instance Identifier,
   * frequency, size, size element value from the template argument TSample */
  typedef typename TSample::MeasurementVectorType MeasurementVectorType;
  typedef typename TSample::MeasurementType       MeasurementType;
  typedef typename TSample::InstanceIdentifier    InstanceIdentifier;
  typedef MeasurementVectorType                   ValueType;

  typedef typename TSample::AbsoluteFrequencyType      AbsoluteFrequencyType;
  typedef typename TSample::TotalAbsoluteFrequencyType TotalAbsoluteFrequencyType;

  /** Type of the storage for instances that belong to the class
   * represented by a Subsample object. A Subsample object stores
   * only the InstanceIdentifiers. The actual data is still in the Sample
   * object */
  typedef std::vector< InstanceIdentifier > InstanceIdentifierHolder;

// Disable clang warning false positive.
// <https://llvm.org/bugs/show_bug.cgi?id=22582>
#if defined(__clang__) && defined(__has_warning)
# if __has_warning("-Winconsistent-missing-override")
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Winconsistent-missing-override"
# endif
#endif

  /** Get the Id Holder */
  virtual const InstanceIdentifierHolder & GetIdHolder() const
  {
    return this->m_IdHolder;
  }

#if defined(__clang__) && defined(__has_warning)
# if __has_warning("-Winconsistent-missing-override")
#  pragma clang diagnostic pop
# endif
#endif

  /** Plug in the actual sample data */
  void SetSample(const TSample *sample);

  const TSample * GetSample() const;

  /** Initialize the subsample with all instances of the sample */
  void InitializeWithAllInstances();

  /** Add instance to the subsample */
  void AddInstance(InstanceIdentifier id);

  /** returns SizeType object whose each element is the number of
   * elements in each dimension */
  InstanceIdentifier Size() const ITK_OVERRIDE;

  /** Clear the subsample */
  void Clear();

  /** returns the measurement of the instance which is identified
   * by the 'id' */
  const MeasurementVectorType & GetMeasurementVector(InstanceIdentifier id) const ITK_OVERRIDE;

  /** returns the frequency of the instance which is identified by the 'id' */
  AbsoluteFrequencyType GetFrequency(InstanceIdentifier id) const ITK_OVERRIDE;

  /** returns the total frequency for the 'd' dimension */
  TotalAbsoluteFrequencyType GetTotalFrequency() const ITK_OVERRIDE;

  void Swap(unsigned int index1, unsigned int index2);

  InstanceIdentifier GetInstanceIdentifier(unsigned int index);

  const MeasurementVectorType & GetMeasurementVectorByIndex(unsigned int index) const;

  AbsoluteFrequencyType GetFrequencyByIndex(unsigned int index) const;

  /** Method to graft another sample */
  virtual void Graft(const DataObject *thatObject) ITK_OVERRIDE;

  class ConstIterator
  {
    friend class Subsample;

public:

    ConstIterator(const Self *sample)
    {
      *this = sample->Begin();
    }

    ConstIterator(const ConstIterator & iter)
    {
      m_Iter = iter.m_Iter;
      m_Subsample = iter.m_Subsample;
      m_Sample = iter.m_Sample;
    }

    ConstIterator & operator=(const ConstIterator & iter)
    {
      m_Iter = iter.m_Iter;
      m_Subsample = iter.m_Subsample;
      m_Sample = iter.m_Sample;
      return *this;
    }

    bool operator!=(const ConstIterator & it)
    {
      return ( m_Iter != it.m_Iter );
    }

    bool operator==(const ConstIterator & it)
    {
      return ( m_Iter == it.m_Iter );
    }

    ConstIterator & operator++()
    {
      ++m_Iter;
      return *this;
    }

    AbsoluteFrequencyType GetFrequency() const
    {
      return m_Sample->GetFrequency(*m_Iter);
    }

    const MeasurementVectorType & GetMeasurementVector() const
    {
      return m_Sample->GetMeasurementVector(*m_Iter);
    }

    InstanceIdentifier GetInstanceIdentifier() const
    {
      return ( m_Iter - m_Subsample->GetIdHolder().begin() );
    }

protected:
    // Purposely not implemented
    ConstIterator();

    // Only to be called from the Subsample
    ConstIterator(typename InstanceIdentifierHolder::const_iterator iter,
                  const Self *classSample):
      m_Iter(iter), m_Subsample(classSample), m_Sample( classSample->GetSample() )
    {}

    // ConstIterator pointing to ImageToListSampleAdaptor
    typename InstanceIdentifierHolder::const_iterator m_Iter;

    // Pointer to Subsample object
    const Self *   m_Subsample;
    const TSample *m_Sample;

private:
  };

  class Iterator:public ConstIterator
  {
    friend class Subsample;

public:

    Iterator(Self *sample):ConstIterator(sample)
    {}

    Iterator(const Iterator & iter):ConstIterator(iter)
    {}

    Iterator & operator=(const Iterator & iter)
    {
      this->ConstIterator::operator=(iter);
      return *this;
    }

protected:
    // To ensure const-correctness these method must not be in the public API.
    // The are purposly not implemented, since they should never be called.
    Iterator();
    Iterator(const Self *sample);
    Iterator(typename InstanceIdentifierHolder::const_iterator iter,
             const Self *classSample);
    Iterator(const ConstIterator & it);
    ConstIterator & operator=(const ConstIterator & it);

    // Only to be called from the Subsample
    Iterator(typename InstanceIdentifierHolder::iterator iter,
             Self *classSample):
      ConstIterator(iter, classSample)
    {}

private:
  };

  /** This method returns an iterator to the beginning of the
      measurement vectors */
  Iterator Begin()
  {
    Iterator iter(m_IdHolder.begin(), this);

    return iter;
  }

  /** This method returns an iterator to the beginning of the
      measurement vectors */
  Iterator  End()
  {
    Iterator iter(m_IdHolder.end(), this);

    return iter;
  }

  ConstIterator Begin() const
  {
    ConstIterator iter(m_IdHolder.begin(), this);

    return iter;
  }

  ConstIterator  End()  const
  {
    ConstIterator iter(m_IdHolder.end(), this);

    return iter;
  }

protected:
  Subsample();
  virtual ~Subsample() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Subsample);

  const TSample *            m_Sample;
  InstanceIdentifierHolder   m_IdHolder;
  unsigned int               m_ActiveDimension;
  TotalAbsoluteFrequencyType m_TotalFrequency;
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSubsample.hxx"
#endif

#endif
