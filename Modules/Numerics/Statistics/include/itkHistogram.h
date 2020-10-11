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
#ifndef itkHistogram_h
#define itkHistogram_h

#include <vector>

#include "itkArray.h"
#include "itkSample.h"
#include "itkDenseFrequencyContainer2.h"
#include "itkSparseFrequencyContainer2.h"

namespace itk
{
namespace Statistics
{

/**
 *\class Histogram
 *  \brief This class stores measurement vectors in the context of n-dimensional histogram.
 *
 * Histogram represents an ND histogram.  Histogram bins can be
 * regularly or irregularly spaced. The storage for the histogram is
 * managed via the FrequencyContainer specified by the template
 * argument.  The default frequency container is a
 * DenseFrequencyContainer. A SparseFrequencyContainer can be used as
 * an alternative.
 *
 * Frequencies of a bin (SetFrequency(), IncreaseFrequency()) can be
 * specified by measurement, index, or instance identifier.
 *
 * Measurements can be queried by bin index or instance
 * identifier. In this case, the measurement returned is the centroid
 * of the histogram bin.
 *
 * The Initialize() method is used to specify the number of bins for
 * each dimension of the histogram. An overloaded version also allows
 * for regularly spaced bins to defined.  To define irregularly sized
 * bins, use the SetBinMin()/SetBinMax() methods.
 *
 * If you do not know the length of the measurement vector at compile time, you
 * should use the VariableDimensionHistogram class, instead of the Histogram
 * class.
 *
 * If you know the length of the measurement vector at compile time, it can
 * conveniently be obtained from MeasurementVectorTraits. For instance,
 * instantiate a histogram as below:
 *
   \code
   using HistogramType = Histogram< THistogramMeasurement, typename TFrequencyContainer >;
   \endcode
 *
 * \sa Sample, DenseFrequencyContainer, SparseFrequencyContainer, VariableDimensionHistogram
 * \ingroup ITKStatistics
 *
 * \sphinx
 * \sphinxexample{Numerics/Statistics/HistogramCreationAndBinAccess,Histogram Creation And Bin Access}
 * \endsphinx
 */

template <typename TMeasurement = float, typename TFrequencyContainer = DenseFrequencyContainer2>
class ITK_TEMPLATE_EXPORT Histogram : public Sample<Array<TMeasurement>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(Histogram);

  // This type serves as the indirect definition of MeasurementVectorType
  using ArrayType = Array<TMeasurement>;

  /** Standard type alias */
  using Self = Histogram;
  using Superclass = Sample<ArrayType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Histogram, Sample);

  /** standard New() method support */
  itkNewMacro(Self);

  /** type of an element of a measurement vector */
  using MeasurementType = TMeasurement;

  /** Common sample class type alias */
  using MeasurementVectorType = typename Superclass::MeasurementVectorType;
  using InstanceIdentifier = typename Superclass::InstanceIdentifier;
  using MeasurementVectorSizeType = typename Superclass::MeasurementVectorSizeType;

  using ValueType = MeasurementVectorType;

  /** frequency container type alias */
  using FrequencyContainerType = TFrequencyContainer;
  using FrequencyContainerPointer = typename FrequencyContainerType::Pointer;

  /** Frequency and TotalFrequency value type from superclass */
  using AbsoluteFrequencyType = typename FrequencyContainerType::AbsoluteFrequencyType;
  using TotalAbsoluteFrequencyType = typename FrequencyContainerType::TotalAbsoluteFrequencyType;
  using RelativeFrequencyType = typename FrequencyContainerType::RelativeFrequencyType;
  using TotalRelativeFrequencyType = typename FrequencyContainerType::TotalRelativeFrequencyType;

  /** Index type alias support An index is used to access pixel values. */
  using IndexType = Array<::itk::IndexValueType>;
  using IndexValueType = typename IndexType::ValueType;

  /** size array type */
  using SizeType = Array<::itk::SizeValueType>;
  using SizeValueType = typename SizeType::ValueType;

  /** bin min max value storage types */
  using BinMinVectorType = std::vector<MeasurementType>;
  using BinMaxVectorType = std::vector<MeasurementType>;
  using BinMinContainerType = std::vector<BinMinVectorType>;
  using BinMaxContainerType = std::vector<BinMaxVectorType>;

  /** Initialize the histogram, generating the offset table and
   * preparing the frequency container. Subclasses should call this
   * method in their Initialize() method. */
  void
  Initialize(const SizeType & size);

  /** Initialize the histogram using equal size bins. To assign bin's
   * min and max values along each dimension use SetBinMin() and
   * SetBinMax() functions. */
  void
  Initialize(const SizeType & size, MeasurementVectorType & lowerBound, MeasurementVectorType & upperBound);

  /** Initialize the values of the histogram bins to zero */
  void
  SetToZero();

  /** Get the index of histogram corresponding to the specified
   *  measurement value. Returns true if index is valid and false if
   *  the measurement is outside the histogram */
  bool
  GetIndex(const MeasurementVectorType & measurement, IndexType & index) const;

  /** Get the index that is uniquely labelled by an instance identifier
   * The corresponding id is the offset of the index
   * This method uses ImageBase::ComputeIndex() method */
  const IndexType &
  GetIndex(InstanceIdentifier id) const;

  /** Is set to false if the bins at edges of the histogram extend to
   *   +/- infinity. */
  itkGetConstMacro(ClipBinsAtEnds, bool);

  /** Set to false to have the bins at edges of the histogram extend to
   *   +/- infinity. */
  itkSetMacro(ClipBinsAtEnds, bool);

  /** Returns true if the given index is out of bound meaning one of index
   * is not between [0, last index] */
  bool
  IsIndexOutOfBounds(const IndexType & index) const;

  /** Get the instance identifier of the bin that is indexed by the
   * index. The corresponding instance identifier is the offset of the index
   * This method uses ImageBase::ComputeIndex() method */
  InstanceIdentifier
  GetInstanceIdentifier(const IndexType & index) const;

  /** Returns the number of instances (bins or cells) in this container */
  InstanceIdentifier
  Size() const override;

  /** Get the size (N-dimensional) of the histogram  */
  const SizeType &
  GetSize() const;

  /** Get the size of histogram along a specified dimension */
  SizeValueType
  GetSize(unsigned int dimension) const;

  /** Get the minimum value of nth bin of dimension d */
  const MeasurementType &
  GetBinMin(unsigned int dimension, InstanceIdentifier nbin) const;

  /** Get the maximum value of nth bin of dimension d */
  const MeasurementType &
  GetBinMax(unsigned int dimension, InstanceIdentifier nbin) const;

  /** Set the minimum value of nth bin of dimension d */
  void
  SetBinMin(unsigned int dimension, InstanceIdentifier nbin, MeasurementType min);

  /** Set the maximum value of nth bin of dimension d */
  void
  SetBinMax(unsigned int dimension, InstanceIdentifier nbin, MeasurementType max);

  /** Get the minimum of the bin along dimension d corresponding to a
   * particular measurement. */
  const MeasurementType &
  GetBinMinFromValue(unsigned int dimension, float value) const;

  /** Get the maximum of the bin along dimension d corresponding to a
   * particular measurement. */
  const MeasurementType &
  GetBinMaxFromValue(unsigned int dimension, float value) const;

  /** Get the vector of bin minimums along a dimension  */
  const BinMinVectorType &
  GetDimensionMins(unsigned int dimension) const;

  /** Get the vector of maximums along a dimension  */
  const BinMaxVectorType &
  GetDimensionMaxs(unsigned int dimension) const;

  /** Get the minimums of the bins  */
  const BinMinContainerType &
  GetMins() const;

  /** Method the maximums of the bins  */
  const BinMaxContainerType &
  GetMaxs() const;

  /** Get the minimums of the bin corresponding to a particular index */
  const MeasurementVectorType &
  GetHistogramMinFromIndex(const IndexType & index) const;

  /** Get the maximums of the bin corresponding to a particular index  */
  const MeasurementVectorType &
  GetHistogramMaxFromIndex(const IndexType & index) const;

  /** Get the frequency of an instance identifier */
  AbsoluteFrequencyType
  GetFrequency(InstanceIdentifier id) const override;

  /** Get the frequency of an index */
  AbsoluteFrequencyType
  GetFrequency(const IndexType & index) const;

  /** Set all the bins in the histogram to a specified frequency */
  void
  SetFrequency(AbsoluteFrequencyType value);

  /** Set the frequency of an instance identifier.  Returns false if the bin is
   * out of bounds. */
  bool
  SetFrequency(InstanceIdentifier id, AbsoluteFrequencyType value);

  /** Set the frequency of an index. Returns false if the bin is
   * out of bounds. */
  bool
  SetFrequencyOfIndex(const IndexType & index, AbsoluteFrequencyType value);

  /** Set the frequency of a measurement. Returns false if the bin is
   * out of bounds. */
  bool
  SetFrequencyOfMeasurement(const MeasurementVectorType & measurement, AbsoluteFrequencyType value);

  /** Increase the frequency of an instance identifier.
   * Frequency is increased by the specified value. Returns false if
   * the bin is out of bounds. */
  bool
  IncreaseFrequency(InstanceIdentifier id, AbsoluteFrequencyType value);

  /** Increase the frequency of an index.  Frequency is
   * increased by the specified value. Returns false if the bin is out
   * of bounds. */
  bool
  IncreaseFrequencyOfIndex(const IndexType & index, AbsoluteFrequencyType value);

  /** Increase the frequency of a measurement.  Frequency is
   * increased by the specified value. Returns false if the
   * measurement is outside the bounds of the histogram.
   *
   * \warning This function performs a dynamic allocation for the
   * index length, and should not be used in tight per-pixel loops.
   */
  bool
  IncreaseFrequencyOfMeasurement(const MeasurementVectorType & measurement, AbsoluteFrequencyType value);

  /** Get the measurement of an instance identifier. This is the
   * centroid of the bin.
   */
  const MeasurementVectorType &
  GetMeasurementVector(InstanceIdentifier id) const override;

  /** Get the measurement of an index. This is the centroid of the bin. */
  const MeasurementVectorType &
  GetMeasurementVector(const IndexType & index) const;

  /** Get the measurement a bin along a specified dimension.  This is
   * the midpoint of the bin along that dimension. */
  MeasurementType
  GetMeasurement(InstanceIdentifier n, unsigned int dimension) const;

  /** Get the total frequency in the histogram */
  TotalAbsoluteFrequencyType
  GetTotalFrequency() const override;

  /** Get the frequency of a dimension's nth element. */
  AbsoluteFrequencyType
  GetFrequency(InstanceIdentifier n, unsigned int dimension) const;

  /** Get the pth percentile value for a dimension.
   *
   * Let assume n = the index of the bin where the p-th percentile value is,
   * min = min value of the dimension of the bin,
   * max = max value of the dimension of the bin,
   * interval = max - min ,
   * pp = cumulated proportion until n-1 bin;
   * and pb = frequency of the bin / total frequency of the dimension.
   *
   * If p is less than 0.5,
   * the percentile value =
   * min + ((p - pp ) / pb) * interval
   * If p is greater than or equal to 0.5
   * the percentile value =
   * max - ((pp - p) / pb) * interval  */
  double
  Quantile(unsigned int dimension, double p) const;

  /** Get the mean value for a dimension */
  double
  Mean(unsigned int dimension) const;

  /** Method to graft another histogram's output */
  void
  Graft(const DataObject *) override;

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

public:
  /**
   *\class ConstIterator
   * \brief class that walks through the elements of the histogram.
   * \ingroup ITKStatistics
   */
  class ConstIterator
  {
  public:
    friend class Histogram;

    ConstIterator(const Self * histogram)
    {
      m_Id = 0;
      m_Histogram = histogram;
    }

    ConstIterator(const ConstIterator & it)
    {
      m_Id = it.m_Id;
      m_Histogram = it.m_Histogram;
    }

    ConstIterator &
    operator=(const ConstIterator & it)
    {
      m_Id = it.m_Id;
      m_Histogram = it.m_Histogram;
      return *this;
    }

    AbsoluteFrequencyType
    GetFrequency() const
    {
      return m_Histogram->GetFrequency(m_Id);
    }

    InstanceIdentifier
    GetInstanceIdentifier() const
    {
      return m_Id;
    }

    const MeasurementVectorType &
    GetMeasurementVector() const
    {
      return m_Histogram->GetMeasurementVector(m_Id);
    }

    const IndexType &
    GetIndex() const
    {
      return m_Histogram->GetIndex(m_Id);
    }

    ConstIterator &
    operator++()
    {
      ++m_Id;
      return *this;
    }

    bool
    operator!=(const ConstIterator & it) const
    {
      return (m_Id != it.m_Id);
    }

    bool
    operator==(const ConstIterator & it) const
    {
      return (m_Id == it.m_Id);
    }

  protected:
    ConstIterator(InstanceIdentifier id, const Self * histogram)
      : m_Id(id)
      , m_Histogram(histogram)
    {}

    // ConstIterator pointing DenseFrequencyContainer
    InstanceIdentifier m_Id;

    // Pointer of DenseFrequencyContainer
    const Self * m_Histogram;

  private:
    ConstIterator() = delete;
  }; // end of iterator class

  /**
   *\class Iterator
   * \brief class that walks through the elements of the histogram.
   * \ingroup ITKStatistics
   */
  class Iterator : public ConstIterator
  {
  public:
    Iterator() = delete;
    Iterator(const Self * histogram) = delete;
    Iterator(InstanceIdentifier id, const Self * histogram) = delete;
    Iterator(const ConstIterator & it) = delete;
    ConstIterator &
    operator=(const ConstIterator & it) = delete;

    Iterator(Self * histogram)
      : ConstIterator(histogram)
    {}

    Iterator(InstanceIdentifier id, Self * histogram)
      : ConstIterator(id, histogram)
    {}

    Iterator(const Iterator & it)
      : ConstIterator(it)
    {}

    Iterator &
    operator=(const Iterator & it)
    {
      this->ConstIterator::operator=(it);
      return *this;
    }

    bool
    SetFrequency(const AbsoluteFrequencyType value)
    {
      auto * histogram = const_cast<Self *>(this->m_Histogram);

      return histogram->SetFrequency(this->m_Id, value);
    }
  }; // end of iterator class

  Iterator
  Begin()
  {
    Iterator iter(0, this);

    return iter;
  }

  Iterator
  End()
  {
    return Iterator(m_OffsetTable[this->GetMeasurementVectorSize()], this);
  }

  ConstIterator
  Begin() const
  {
    ConstIterator iter(0, this);

    return iter;
  }

  ConstIterator
  End() const
  {
    return ConstIterator(m_OffsetTable[this->GetMeasurementVectorSize()], this);
  }

protected:
  Histogram();
  ~Histogram() override = default;

  // The number of bins for each dimension
  SizeType m_Size;

private:
  using OffsetTableType = std::vector<InstanceIdentifier>;
  OffsetTableType           m_OffsetTable;
  FrequencyContainerPointer m_FrequencyContainer;
  unsigned int              m_NumberOfInstances{ 0 };

  // This method is provided here just to avoid a "hidden" warning
  // related to the virtual method available in DataObject.
  void
  Initialize() override
  {}

  // lower bound of each bin
  std::vector<std::vector<MeasurementType>> m_Min;

  // upper bound of each bin
  std::vector<std::vector<MeasurementType>> m_Max;

  mutable MeasurementVectorType m_TempMeasurementVector;
  mutable IndexType             m_TempIndex;

  bool m_ClipBinsAtEnds{ true };
};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHistogram.hxx"
#endif

#endif
