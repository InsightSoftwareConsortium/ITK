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
#ifndef itkLabelStatisticsImageFilter_h
#define itkLabelStatisticsImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itksys/hash_map.hxx"
#include "itkHistogram.h"
#include "itkFastMutexLock.h"
#include <vector>

namespace itk
{
/** \class LabelStatisticsImageFilter
 * \brief Given an intensity image and a label map, compute min, max, variance and mean of the pixels associated with each label or segment
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
 * The filter passes its intensity input through unmodified.  The filter is
 * threaded. It computes statistics in each thread then combines them in
 * its AfterThreadedGenerate method.
 *
 * \ingroup MathematicalStatisticsImageFilters
 * \ingroup ITKImageStatistics
 *
 * \wiki
 * \wikiexample{ImageProcessing/LabelStatisticsImageFilter,Get statistical properties of labeled regions in an image}
 * \endwiki
 */
template< typename TInputImage, typename TLabelImage >
class ITK_TEMPLATE_EXPORT LabelStatisticsImageFilter:
  public ImageToImageFilter< TInputImage, TInputImage >
{
public:
  /** Standard Self typedef */
  typedef LabelStatisticsImageFilter                     Self;
  typedef ImageToImageFilter< TInputImage, TInputImage > Superclass;
  typedef SmartPointer< Self >                           Pointer;
  typedef SmartPointer< const Self >                     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelStatisticsImageFilter, ImageToImageFilter);

  /** Image related typedefs. */
  typedef typename TInputImage::Pointer    InputImagePointer;
  typedef typename TInputImage::RegionType RegionType;
  typedef typename TInputImage::SizeType   SizeType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::PixelType  PixelType;

  /** Label image related typedefs. */
  typedef TLabelImage                      LabelImageType;
  typedef typename TLabelImage::Pointer    LabelImagePointer;
  typedef typename TLabelImage::RegionType LabelRegionType;
  typedef typename TLabelImage::SizeType   LabelSizeType;
  typedef typename TLabelImage::IndexType  LabelIndexType;
  typedef typename TLabelImage::PixelType  LabelPixelType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Type to use for computations. */
  typedef typename NumericTraits< PixelType >::RealType RealType;

  /** Smart Pointer type to a DataObject. */
  typedef typename DataObject::Pointer DataObjectPointer;

  /** Type of DataObjects used for scalar outputs */
  typedef SimpleDataObjectDecorator< RealType > RealObjectType;

  /** Bounding Box-related typedefs */
  typedef std::vector< IndexValueType > BoundingBoxType;

  /** Histogram-related typedefs */
  typedef itk::Statistics::Histogram< RealType > HistogramType;
  typedef typename HistogramType::Pointer        HistogramPointer;

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
      m_Count = NumericTraits< IdentifierType >::ZeroValue();
      m_Sum = NumericTraits< RealType >::ZeroValue();
      m_SumOfSquares = NumericTraits< RealType >::ZeroValue();

      // Set such that the first pixel encountered can be compared
      m_Minimum = NumericTraits< RealType >::max();
      m_Maximum = NumericTraits< RealType >::NonpositiveMin();

      // Default these to zero
      m_Mean = NumericTraits< RealType >::ZeroValue();
      m_Sigma = NumericTraits< RealType >::ZeroValue();
      m_Variance = NumericTraits< RealType >::ZeroValue();

      const unsigned int imageDimension = itkGetStaticConstMacro(ImageDimension);
      m_BoundingBox.resize(imageDimension * 2);
      for ( unsigned int i = 0; i < imageDimension * 2; i += 2 )
        {
        m_BoundingBox[i] = NumericTraits< IndexValueType >::max();
        m_BoundingBox[i + 1] = NumericTraits< IndexValueType >::NonpositiveMin();
        }
      m_Histogram = ITK_NULLPTR;
    }

    // constructor with histogram enabled
    LabelStatistics(int size, RealType lowerBound, RealType upperBound)
    {
      // initialized to the default values
      m_Count = NumericTraits< IdentifierType >::ZeroValue();
      m_Sum = NumericTraits< RealType >::ZeroValue();
      m_SumOfSquares = NumericTraits< RealType >::ZeroValue();

      // Set such that the first pixel encountered can be compared
      m_Minimum = NumericTraits< RealType >::max();
      m_Maximum = NumericTraits< RealType >::NonpositiveMin();

      // Default these to zero
      m_Mean = NumericTraits< RealType >::ZeroValue();
      m_Sigma = NumericTraits< RealType >::ZeroValue();
      m_Variance = NumericTraits< RealType >::ZeroValue();

      const unsigned int imageDimension = itkGetStaticConstMacro(ImageDimension);
      m_BoundingBox.resize(imageDimension * 2);
      for ( unsigned int i = 0; i < imageDimension * 2; i += 2 )
        {
        m_BoundingBox[i] = NumericTraits< IndexValueType >::max();
        m_BoundingBox[i + 1] = NumericTraits< IndexValueType >::NonpositiveMin();
        }

      // Histogram
      m_Histogram = HistogramType::New();
      typename HistogramType::SizeType hsize;
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

    // added for completeness
    LabelStatistics &operator= (const LabelStatistics& l)
    {
      if(this != &l)
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

    IdentifierType  m_Count;
    RealType        m_Minimum;
    RealType        m_Maximum;
    RealType        m_Mean;
    RealType        m_Sum;
    RealType        m_SumOfSquares;
    RealType        m_Sigma;
    RealType        m_Variance;
    BoundingBoxType m_BoundingBox;
    typename HistogramType::Pointer m_Histogram;
  };

  /** Type of the map used to store data per label */
  typedef itksys::hash_map< LabelPixelType, LabelStatistics >                          MapType;
  typedef typename itksys::hash_map< LabelPixelType, LabelStatistics >::iterator       MapIterator;
  typedef typename itksys::hash_map< LabelPixelType, LabelStatistics >::const_iterator MapConstIterator;
  typedef IdentifierType                                                               MapSizeType;

  /** Type of the container used to store valid label values */
  typedef std::vector<LabelPixelType> ValidLabelValuesContainerType;

  // macros for Histogram enables
  itkSetMacro(UseHistograms, bool);
  itkGetConstMacro(UseHistograms, bool);
  itkBooleanMacro(UseHistograms);


  virtual const ValidLabelValuesContainerType &GetValidLabelValues() const
  {
    return m_ValidLabelValues;
  }

  /** Set the label image */
  void SetLabelInput(const TLabelImage *input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput( 1, const_cast< TLabelImage * >( input ) );
  }

  /** Get the label image */
  const LabelImageType * GetLabelInput() const
  {
    return itkDynamicCastInDebugMode< LabelImageType * >( const_cast< DataObject * >( this->ProcessObject::GetInput(1) ) );
  }

  /** Does the specified label exist? Can only be called after a call
   * a call to Update(). */
  bool HasLabel(LabelPixelType label) const
  {
    return m_LabelStatistics.find(label) != m_LabelStatistics.end();
  }

  /** Get the number of labels used */
  MapSizeType GetNumberOfObjects() const
  {
    return static_cast<MapSizeType>( m_LabelStatistics.size() );
  }

  MapSizeType GetNumberOfLabels() const
  {
    return static_cast<MapSizeType>( this->GetNumberOfObjects() );
  }

  /** Return the computed Minimum for a label. */
  RealType GetMinimum(LabelPixelType label) const;

  /** Return the computed Maximum for a label. */
  RealType GetMaximum(LabelPixelType label) const;

  /** Return the computed Mean for a label. */
  RealType GetMean(LabelPixelType label) const;

  /** Return the computed Median for a label. Requires histograms to be enabled!
    */
  RealType GetMedian(LabelPixelType label) const;

  /** Return the computed Standard Deviation for a label. */
  RealType GetSigma(LabelPixelType label) const;

  /** Return the computed Variance for a label. */
  RealType GetVariance(LabelPixelType label) const;

  /** Return the computed bounding box for a label. A vector of
   * minIndex, maxIndex pairs for each axis. The intervals include
   * the endpoints.*/
  BoundingBoxType GetBoundingBox(LabelPixelType label) const;

  /** Return the computed region. */
  RegionType GetRegion(LabelPixelType label) const;

  /** Return the compute Sum for a label. */
  RealType GetSum(LabelPixelType label) const;

  /** Return the number of pixels for a label. */
  MapSizeType GetCount(LabelPixelType label) const;

  /** Return the histogram for a label */
  HistogramPointer GetHistogram(LabelPixelType label) const;

  /** specify Histogram parameters  */
  void SetHistogramParameters(const int numBins, RealType lowerBound,
                              RealType upperBound);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< PixelType > ) );
  // End concept checking
#endif

protected:
  LabelStatisticsImageFilter();
  ~LabelStatisticsImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Pass the input through unmodified. Do this by Grafting in the
    AllocateOutputs method. */
  void AllocateOutputs() ITK_OVERRIDE;

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** Do final mean and variance computation from data accumulated in threads.
    */
  void AfterThreadedGenerateData() ITK_OVERRIDE;

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const RegionType &
                             outputRegionForThread,
                             ThreadIdType threadId) ITK_OVERRIDE;

  // Override since the filter produces all of its output
  void EnlargeOutputRequestedRegion(DataObject *data) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelStatisticsImageFilter);

  std::vector< MapType >        m_LabelStatisticsPerThread;
  MapType                       m_LabelStatistics;
  ValidLabelValuesContainerType m_ValidLabelValues;

  bool m_UseHistograms;

  typename HistogramType::SizeType m_NumBins;

  RealType            m_LowerBound;
  RealType            m_UpperBound;
  SimpleFastMutexLock m_Mutex;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelStatisticsImageFilter.hxx"
#endif

#endif
