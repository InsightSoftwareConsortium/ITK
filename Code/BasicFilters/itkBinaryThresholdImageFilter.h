/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThresholdImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryThresholdImageFilter_h
#define __itkBinaryThresholdImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
  
/** \class BinaryThresholdImageFilter
 *
 * \brief Binarize an input image by thresholding.
 *
 * This filter produces an output image whose pixels
 * are either one of two values ( OutsideValue or InsideValue ), 
 * depending on whether of not the corresponding input image pixel
 * lie between the two thresholds ( LowerThreshold and UpperThreshold ).
 * Values equal to either threshold is considered to be between the thresholds.
 * 
 * This filter is templated over the input image type
 * and the output image type.
 * 
 * The filter expect both images to have the same number of dimensions.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */
namespace Functor {  
  
template< class TInput, class TOutput>
class BinaryThreshold
{
public:
  BinaryThreshold() {};
  ~BinaryThreshold() {};

  void SetLowerThreshold( const TInput & thresh )
  { m_LowerThreshold = thresh; }
  void SetUpperThreshold( const TInput & thresh )
  { m_UpperThreshold = thresh; }
  void SetInsideValue( const TOutput & value )
  { m_InsideValue = value; }
  void SetOutsideValue( const TOutput & value )
  { m_OutsideValue = value; }

  inline TOutput operator()( const TInput & A )
  {
    if ( m_LowerThreshold <= A && A <= m_UpperThreshold )
      {
      return m_InsideValue;
      }
    return m_OutsideValue;
  }

private:
  TInput      m_LowerThreshold;
  TInput      m_UpperThreshold;
  TOutput     m_InsideValue;
  TOutput     m_OutsideValue;

};
}

template <class TInputImage, class TOutputImage>
class ITK_EXPORT BinaryThresholdImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Functor::BinaryThreshold< 
  typename TInputImage::PixelType, 
  typename TOutputImage::PixelType> >
{
public:
  /** Standard class typedefs. */
  typedef BinaryThresholdImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Functor::BinaryThreshold< 
    typename TInputImage::PixelType, 
    typename TOutputImage::PixelType>   
  >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryThresholdImageFilter, UnaryFunctorImageFilter);

  /** Pixel types. */
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  /** The input pixel type must support comparison operators. */
  itkConceptMacro(PixelTypeComparable, (Concept::Comparable<InputPixelType>));

  /** Set the "outside" pixel value. The default value 
   * NumericTraits<OutputPixelType>::Zero. */
  itkSetMacro(OutsideValue,OutputPixelType);
  
  /** Get the "outside" pixel value. */
  itkGetMacro(OutsideValue,OutputPixelType);

  /** Set the "inside" pixel value. The default value 
   * NumericTraits<OutputPixelType>::max() */
  itkSetMacro(InsideValue,OutputPixelType);
  
  /** Get the "inside" pixel value. */
  itkGetMacro(InsideValue,OutputPixelType);

  /** Set the thresholds. The default lower threshold  
   * is NumericTraits<InputPixelType>::NonpositiveMin(). The default upper
   * threshold is NumericTraits<InputPixelType>::max. An execption is thrown
   * if the lower threshold is greater than the upper threshold. */
  itkSetMacro( UpperThreshold, InputPixelType );
  itkSetMacro( LowerThreshold, InputPixelType );
                 
  /** Get the threshold values. */
  itkGetMacro( UpperThreshold, InputPixelType );
  itkGetMacro( LowerThreshold, InputPixelType );

  
protected:
  BinaryThresholdImageFilter();
  virtual ~BinaryThresholdImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** This method is used to set the state of the filter before 
   * multi-threading. */
  virtual void BeforeThreadedGenerateData();

private:
  BinaryThresholdImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  InputPixelType      m_LowerThreshold;
  InputPixelType      m_UpperThreshold;
  OutputPixelType     m_InsideValue;
  OutputPixelType     m_OutsideValue;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryThresholdImageFilter.txx"
#endif

#endif
