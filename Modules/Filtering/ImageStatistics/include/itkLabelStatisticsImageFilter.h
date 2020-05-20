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
#ifndef itkLabelStatisticsImageFilter_h
#define itkLabelStatisticsImageFilter_h

#include "itkImageSink.h"
#include "itkNumericTraits.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itkHistogram.h"
#include <mutex>
#include <unordered_map>
#include <vector>

namespace itk
{
/** \class LabelStatisticsImageFilter
 * \brief Given an intensity image and a label map, compute min, max, variance and mean of the pixels associated with
 * each label or segment
 *
 * LabelStatisticsImageFilter computes the minimum, maximum, sum,
 * mean, median, variance and sigma of regions of an intensity image, where
 * the regions are defined via a label map (a second input).  The
 * label image should be integral type. The filter needs all of its
 * input image.  It behaves as a filter with an input and output. Thus
 * it can be inserted in a pipline with other filters and the
 * statistics will only be recomputed if a downstream filter changes.
 *
 * Optionally, the filter also computes intensity histograms on each
 * object. If histograms are enabled, a median intensity value can
 * also be computed, although its accuracy is limited to the bin width
 * of the histogram. If histograms are not enabled, the median returns
 * zero.
 *
 * This filter is automatically multi-threaded and can stream its
 * input when NumberOfStreamDivisions is set to more than
 * 1. Statistics are independently computed for each streamed and
 * threaded region then merged.
 *
 * \ingroup MathematicalStatisticsImageFilters
 * \ingroup ITKImageStatistics
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageStatistics/StatisticalPropertiesOfRegions,Statistical Properties Of Labeled Regions}
 * \endsphinx
 */
template <typename TInputImage, typename TLabelImage>
class ITK_TEMPLATE_EXPORT LabelStatisticsImageFilter : public ImageSink<TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelStatisticsImageFilter);

  /** Standard Self type alias */
  using Self = LabelStatisticsImageFilter;
  using Superclass = ImageSink<TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelStatisticsImageFilter, ImageToImageFilter);

  /** Image related type alias. */
  using InputImagePointer = typename TInputImage::Pointer;
  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename TInputImage::SizeType;
  using IndexType = typename TInputImage::IndexType;
  using PixelType = typename TInputImage::PixelType;

  /** Label image related type alias. */
  using LabelImageType = TLabelImage;
  using LabelImagePointer = typename TLabelImage::Pointer;
  using LabelRegionType = typename TLabelImage::RegionType;
  using LabelSizeType = typename TLabelImage::SizeType;
  using LabelIndexType = typename TLabelImage::IndexType;
  using LabelPixelType = typename TLabelImage::PixelType;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Type to use for computations. */
  using RealType = typename NumericTraits<PixelType>::RealType;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = typename DataObject::Pointer;

  /** Type of DataObjects used for scalar outputs */
  using RealObjectType = SimpleDataObjectDecorator<RealType>;

  /** Bounding Box-related type alias */
  using BoundingBoxType = std::vector<IndexValueType>;

  /** Histogram-related type alias */
  using HistogramType = itk::Statistics::Histogram<RealType>;
  using HistogramPointer = typename HistogramType::Pointer;

  /** \class LabelStatistics
   * \brief Statistics stored per label
   * \ingroup ITKImageStatistics
   */
  class LabelStatistics
  {
  public:
    // default constructor
    LabelStatistics()
    {
      // initialized to the default values
      m_Count = NumericTraits<IdentifierType>::ZeroValue();
      m_Sum = NumericTraits<RealType>::ZeroValue();
      m_SumOfSquares = NumericTraits<RealType>::ZeroValue();

      // Set such that the first pixel encountered can be compared
      m_Minimum = NumericTraits<RealType>::max();
      m_Maximum = NumericTraits<RealType>::NonpositiveMin();

      // Default these to zero
      m_Mean = NumericTraits<RealType>::ZeroValue();
      m_Sigma = NumericTraits<RealType>::ZeroValue();
      m_Variance = NumericTraits<RealType>::ZeroValue();

      const unsigned int imageDimension = Self::ImageDimension;
      m_BoundingBox.resize(imageDimension * 2);
      for (unsigned int i = 0; i < imageDimension * 2; i += 2)
      {
        m_BoundingBox[i] = NumericTraits<IndexValueType>::max();
        m_BoundingBox[i + 1] = NumericTraits<IndexValueType>::NonpositiveMin();
      }
      m_Histogram = nullptr;
    }

    // constructor with histogram enabled
    LabelStatistics(int size, RealType lowerBound, RealType upperBound)
    {
      // initialized to the default values
      m_Count = NumericTraits<IdentifierType>::ZeroValue();
      m_Sum = NumericTraits<RealType>::ZeroValue();
      m_SumOfSquares = NumericTraits<RealType>::ZeroValue();

      // Set such that the first pixel encountered can be compared
      m_Minimum = NumericTraits<RealType>::max();
      m_Maximum = NumericTraits<RealType>::NonpositiveMin();

      // Default these to zero
      m_Mean = NumericTraits<RealType>::ZeroValue();
      m_Sigma = NumericTraits<RealType>::ZeroValue();
      m_Variance = NumericTraits<RealType>::ZeroValue();

      const unsigned int imageDimension = Self::ImageDimension;
      m_BoundingBox.resize(imageDimension * 2);
      for (unsigned int i = 0; i < imageDimension * 2; i += 2)
      {
        m_BoundingBox[i] = NumericTraits<IndexValueType>::max();
        m_BoundingBox[i + 1] = NumericTraits<IndexValueType>::NonpositiveMin();
      }

      // Histogram
      m_Histogram = HistogramType::New();
      typename HistogramType::SizeType              hsize;
      typename HistogramType::MeasurementVectorType lb;
      typename HistogramType::MeasurementVectorType ub;
      hsize.SetSize(1);
      lb.SetSize(1);
      ub.SetSize(1);
      m_Histogram->SetMeasurementVectorSize(1);
      hsize[0] = size;
      lb[0] = lowerBound;
      ub[0] = upperBound;
      m_Histogram->Initialize(hsize, lb, ub);
    }

    // need copy constructor because of smart pointer to histogram
    LabelStatistics(const LabelStatistics & l)
    {
      m_Count = l.m_Count;
      m_Minimum = l.m_Minimum;
      m_Maximum = l.m_Maximum;
      m_Mean = l.m_Mean;
      m_Sum = l.m_Sum;
      m_SumOfSquares = l.m_SumOfSquares;
      m_Sigma = l.m_Sigma;
      m_Variance = l.m_Variance;
      m_BoundingBox = l.m_BoundingBox;
      m_Histogram = l.m_Histogram;
    }

    LabelStatistics(LabelStatistics &&) = default;

    // added for completeness
    LabelStatistics &
    operator=(const LabelStatistics & l)
    {
      if (this != &l)
      {
        m_Count = l.m_Count;
        m_Minimum = l.m_Minimum;
        m_Maximum = l.m_Maximum;
        m_Mean = l.m_Mean;
        m_Sum = l.m_Sum;
        m_SumOfSquares = l.m_SumOfSquares;
        m_Sigma = l.m_Sigma;
        m_Variance = l.m_Variance;
        m_BoundingBox = l.m_BoundingBox;
        m_Histogram = l.m_Histogram;
      }
      return *this;
    }

    IdentifierType                  m_Count;
    RealType                        m_Minimum;
    RealType                        m_Maximum;
    RealType                        m_Mean;
    RealType                        m_Sum;
    RealType                        m_SumOfSquares;
    RealType                        m_Sigma;
    RealType                        m_Variance;
    BoundingBoxType                 m_BoundingBox;
    typename HistogramType::Pointer m_Histogram;
  };

  /** Type of the map used to store data per label */
  using MapType = std::unordered_map<LabelPixelType, LabelStatistics>;
  using MapIterator = typename MapType::iterator;
  using MapConstIterator = typename MapType::const_iterator;
  using MapSizeType = IdentifierType;

  /** Type of the container used to store valid label values */
  using ValidLabelValuesContainerType = std::vector<LabelPixelType>;

  // macros for Histogram enables
  itkSetMacro(UseHistograms, bool);
  itkGetConstMacro(UseHistograms, bool);
  itkBooleanMacro(UseHistograms);


  virtual const ValidLabelValuesContainerType &
  GetValidLabelValues() const
  {
    return m_ValidLabelValues;
  }

  /** Set the label image */
  itkSetInputMacro(LabelInput, TLabelImage);
  itkGetInputMacro(LabelInput, TLabelImage);

  /** Does the specified label exist? Can only be called after a call
   * a call to Update(). */
  bool
  HasLabel(LabelPixelType label) const
  {
    return m_LabelStatistics.find(label) != m_LabelStatistics.end();
  }

  /** Get the number of labels used */
  MapSizeType
  GetNumberOfObjects() const
  {
    return static_cast<MapSizeType>(m_LabelStatistics.size());
  }

  MapSizeType
  GetNumberOfLabels() const
  {
    return static_cast<MapSizeType>(this->GetNumberOfObjects());
  }

  /** Return the computed Minimum for a label. */
  RealType
  GetMinimum(LabelPixelType label) const;

  /** Return the computed Maximum for a label. */
  RealType
  GetMaximum(LabelPixelType label) const;

  /** Return the computed Mean for a label. */
  RealType
  GetMean(LabelPixelType label) const;

  /** Return the computed Median for a label. Requires histograms to be enabled!
   */
  RealType
  GetMedian(LabelPixelType label) const;

  /** Return the computed Standard Deviation for a label. */
  RealType
  GetSigma(LabelPixelType label) const;

  /** Return the computed Variance for a label. */
  RealType
  GetVariance(LabelPixelType label) const;

  /** Return the computed bounding box for a label. A vector of
   * minIndex, maxIndex pairs for each axis. The intervals include
   * the endpoints.*/
  BoundingBoxType
  GetBoundingBox(LabelPixelType label) const;

  /** Return the computed region. */
  RegionType
  GetRegion(LabelPixelType label) const;

  /** Return the compute Sum for a label. */
  RealType
  GetSum(LabelPixelType label) const;

  /** Return the number of pixels for a label. */
  MapSizeType
  GetCount(LabelPixelType label) const;

  /** Return the histogram for a label */
  HistogramPointer
  GetHistogram(LabelPixelType label) const;

  /** specify Histogram parameters  */
  void
  SetHistogramParameters(const int numBins, RealType lowerBound, RealType upperBound);

  // Change the access from protected to public to expose streaming option, a using statement can not be used due to
  // limitations of wrapping.
  void
  SetNumberOfStreamDivisions(const unsigned int n) override
  {
    Superclass::SetNumberOfStreamDivisions(n);
  }
  unsigned int
  GetNumberOfStreamDivisions() const override
  {
    return Superclass::GetNumberOfStreamDivisions();
  }


#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<PixelType>));
  // End concept checking
#endif

protected:
  LabelStatisticsImageFilter();
  ~LabelStatisticsImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  BeforeStreamedGenerateData() override
  {
    this->AllocateOutputs();
    m_LabelStatistics.clear();
  }

  /** Do final mean and variance computation from data accumulated in threads.
   */
  void
  AfterStreamedGenerateData() override;

  void
  ThreadedStreamedGenerateData(const RegionType &) override;

private:
  void
  MergeMap(MapType &, MapType &) const;

  MapType                       m_LabelStatistics;
  ValidLabelValuesContainerType m_ValidLabelValues;

  bool m_UseHistograms;

  typename HistogramType::SizeType m_NumBins;

  RealType m_LowerBound;
  RealType m_UpperBound;

  std::mutex m_Mutex;

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelStatisticsImageFilter.hxx"
#endif

#endif
