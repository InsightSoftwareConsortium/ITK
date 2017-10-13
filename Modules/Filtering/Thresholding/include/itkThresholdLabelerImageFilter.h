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
#ifndef itkThresholdLabelerImageFilter_h
#define itkThresholdLabelerImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class ThresholdLabelerImageFilter
 *
 * \brief Label an input image according to a set of thresholds.
 *
 * This filter produces an output image whose pixels are labeled
 * progressively according to the classes identified by a set of thresholds.
 * Values equal to a threshold is considered to be in the lower class.
 *
 * This filter is templated over the input image type
 * and the output image type.
 *
 * The filter expect both images to have the same number of dimensions.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKThresholding
 */
namespace Functor
{
template< typename TInput, typename TOutput >
class ITK_TEMPLATE_EXPORT ThresholdLabeler
{
public:
  ThresholdLabeler() { m_LabelOffset = NumericTraits< TOutput >::OneValue(); }
  ~ThresholdLabeler() {}

  typedef typename NumericTraits< TInput >::RealType RealThresholdType;
  typedef std::vector< RealThresholdType >           RealThresholdVector;

  /** Set the vector of thresholds. */
  void SetThresholds(const RealThresholdVector & thresholds)
  { m_Thresholds = thresholds; }

  /** Set the offset which labels have to start from. */
  void SetLabelOffset(const TOutput & labelOffset)
  { m_LabelOffset = labelOffset; }

  bool operator!=(const ThresholdLabeler & other) const
  {
    if ( m_Thresholds != other.m_Thresholds
         || m_LabelOffset != other.m_LabelOffset )
      {
      return true;
      }
    return false;
  }

  bool operator==(const ThresholdLabeler & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    size_t size = m_Thresholds.size();

    if ( size == 0 )
      {
      return m_LabelOffset;
      }
    if ( A <= m_Thresholds[0] )
      {
      return m_LabelOffset;
      }
    for ( size_t i = 0; i < size - 1; i++ )
      {
      /* Value is in this class if it equals the upper bound. */
      if ( m_Thresholds[i] < A && A <= m_Thresholds[i + 1] )
        {
        return static_cast< TOutput >( i + 1 ) + m_LabelOffset;
        }
      }
    return static_cast< TOutput >( size ) + m_LabelOffset;
  }

private:

  RealThresholdVector m_Thresholds;
  TOutput             m_LabelOffset;
};
}

template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT ThresholdLabelerImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::ThresholdLabeler<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef ThresholdLabelerImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::ThresholdLabeler<
      typename TInputImage::PixelType,
      typename TOutputImage::PixelType >
    >                                  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ThresholdLabelerImageFilter, UnaryFunctorImageFilter);

  /** Pixel types. */
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  /** Threshold vector types. */
  typedef std::vector< InputPixelType >                      ThresholdVector;
  typedef typename NumericTraits< InputPixelType >::RealType RealThresholdType;
  typedef std::vector< RealThresholdType >                   RealThresholdVector;

  /** The input and output pixel types must support comparison operators. */
#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( PixelTypeComparable,
                   ( Concept::Comparable< InputPixelType > ) );
  itkConceptMacro( OutputPixelTypeComparable,
                   ( Concept::Comparable< OutputPixelType > ) );
  itkConceptMacro( OutputPixelTypeOStreamWritable,
                   ( Concept::OStreamWritable< OutputPixelType > ) );
  // End concept checking
#endif

  /** Set the vector of thresholds. */
  void SetThresholds(const ThresholdVector & thresholds)
  {
    m_Thresholds = thresholds;
    m_RealThresholds.clear();
    typename ThresholdVector::const_iterator itr = m_Thresholds.begin();
    while ( itr != m_Thresholds.end() )
      {
      m_RealThresholds.push_back( static_cast< RealThresholdType >( *itr ) );
      ++itr;
      }
    this->Modified();
  }

  /** Get the vector of thresholds. */
  const ThresholdVector & GetThresholds() const
  { return m_Thresholds; }

  /** Set the vector of real type thresholds. */
  void SetRealThresholds(const RealThresholdVector & thresholds)
  {
    m_RealThresholds = thresholds;
    m_Thresholds.clear();
    typename RealThresholdVector::const_iterator itr = m_RealThresholds.begin();
    while ( itr != m_RealThresholds.end() )
      {
      m_Thresholds.push_back( static_cast< InputPixelType >( *itr ) );
      ++itr;
      }
    this->Modified();
  }

  /** Get the vector of real thresholds. */
  const RealThresholdVector & GetRealThresholds() const
  { return m_RealThresholds; }

  /** Set the offset which labels have to start from. */
  itkSetClampMacro( LabelOffset, OutputPixelType, NumericTraits< OutputPixelType >::ZeroValue(),
                    NumericTraits< OutputPixelType >::max() );
  itkGetConstMacro(LabelOffset, OutputPixelType);

protected:
  ThresholdLabelerImageFilter();
  virtual ~ThresholdLabelerImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** This method is used to set the state of the filter before
   * multi-threading. */
  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ThresholdLabelerImageFilter);

  ThresholdVector     m_Thresholds;
  RealThresholdVector m_RealThresholds;
  OutputPixelType     m_LabelOffset;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThresholdLabelerImageFilter.hxx"
#endif

#endif
