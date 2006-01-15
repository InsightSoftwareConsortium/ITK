/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdLabelerImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkThresholdLabelerImageFilter_h
#define __itkThresholdLabelerImageFilter_h

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
 * \ingroup IntensityImageFilters  Multithreaded
 */
namespace Functor {  
  
template< class TInput, class TOutput >
class ThresholdLabeler
{
public:
  ThresholdLabeler() { m_LabelOffset = NumericTraits<TOutput>::One; }
  ~ThresholdLabeler() {};

  typedef std::vector<TInput> ThresholdVector;

  /** Set the vector of thresholds. */
  void SetThresholds( const ThresholdVector & thresholds )
    { m_Thresholds = thresholds; }

  /** Set the offset which labels have to start from. */
  void SetLabelOffset( const TOutput & labelOffset )
    { m_LabelOffset = labelOffset; }

  bool operator!=( const ThresholdLabeler & other ) const
  {
    if( m_Thresholds != other.m_Thresholds ||
        m_LabelOffset != other.m_LabelOffset )
        {
        return true;
        }
    return false;
   }
  bool operator==( const ThresholdLabeler & other ) const
  {
    return !(*this != other);
  }

  inline TOutput operator()( const TInput & A )
    {
    unsigned int size = m_Thresholds.size();
    if (size == 0)
      {
      return m_LabelOffset;
      }
    if (A <= m_Thresholds[0])
      {
      return m_LabelOffset;
      }
    for (unsigned int i=0; i<size-1; i++)
      {
      /* Value is in this class if it equals the upper bound. */
      if (m_Thresholds[i] < A && A <= m_Thresholds[i+1])
        {
        return static_cast<TOutput>(i+1) + m_LabelOffset;
        }
      }
    return static_cast<TOutput>(size) + m_LabelOffset;
    }

private:

  ThresholdVector m_Thresholds;
  TOutput m_LabelOffset;
};
}

template <class TInputImage,class TOutputImage>
class ThresholdLabelerImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Functor::ThresholdLabeler< 
  typename TInputImage::PixelType,
  typename TOutputImage::PixelType> >
{
public:
  /** Standard class typedefs. */
  typedef ThresholdLabelerImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Functor::ThresholdLabeler< 
    typename TInputImage::PixelType,
    typename TOutputImage::PixelType>   
  >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ThresholdLabelerImageFilter, UnaryFunctorImageFilter);

  /** Pixel types. */
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef typename TOutputImage::PixelType  OutputPixelType;

  /** Threshold vector types. */
  typedef std::vector<InputPixelType> ThresholdVector;

  /** The input pixel type must support comparison operators. */
  itkConceptMacro(PixelTypeComparable, (Concept::Comparable<InputPixelType>));

  /** Set the vector of thresholds. */
  void SetThresholds( const ThresholdVector & thresholds )
  { m_Thresholds = thresholds; }
  /** Get the vector of thresholds. */
  const ThresholdVector & GetThresholds()
  { return m_Thresholds; }

  /** Set the offset which labels have to start from. */
  itkSetClampMacro(LabelOffset,OutputPixelType, NumericTraits<OutputPixelType>::Zero,NumericTraits<OutputPixelType>::max() );
  itkGetMacro(LabelOffset,OutputPixelType);

protected:
  ThresholdLabelerImageFilter();
  virtual ~ThresholdLabelerImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** This method is used to set the state of the filter before 
   * multi-threading. */
  virtual void BeforeThreadedGenerateData();

private:
  ThresholdLabelerImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  ThresholdVector m_Thresholds;
  OutputPixelType m_LabelOffset;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThresholdLabelerImageFilter.txx"
#endif

#endif
