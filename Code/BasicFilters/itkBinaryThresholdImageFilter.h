/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThresholdImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryThresholdImageFilter_h
#define __itkBinaryThresholdImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkConceptChecking.h"
#include "itkSimpleDataObjectDecorator.h"

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

  bool operator!=( const BinaryThreshold & other ) const
  {
    if( m_LowerThreshold != other.m_LowerThreshold ||
        m_UpperThreshold != other.m_UpperThreshold ||
        m_InsideValue    != other.m_InsideValue    ||
        m_OutsideValue   != other.m_OutsideValue  )
        {
        return true;
        }
    return false;
   }
  bool operator==( const BinaryThreshold & other ) const
  {
    return !(*this != other);
  }

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

  /** Type of DataObjects to use for scalar inputs */
  typedef SimpleDataObjectDecorator<InputPixelType> InputPixelObjectType;
  
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
  virtual void SetUpperThreshold(const InputPixelType threshold);
  virtual void SetUpperThresholdInput( const InputPixelObjectType *);
  virtual void SetLowerThreshold(const InputPixelType threshold);
  virtual void SetLowerThresholdInput( const InputPixelObjectType *);
                 
  /** Get the threshold values. */
  virtual InputPixelType GetUpperThreshold() const;
  virtual InputPixelObjectType *GetUpperThresholdInput();
  virtual const InputPixelObjectType *GetUpperThresholdInput() const;
  virtual InputPixelType GetLowerThreshold() const;
  virtual InputPixelObjectType *GetLowerThresholdInput();
  virtual const InputPixelObjectType *GetLowerThresholdInput() const;

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(OutputEqualityComparableCheck,
                  (Concept::EqualityComparable<OutputPixelType>));
  itkConceptMacro(PixelTypeComparable,
                  (Concept::Comparable<InputPixelType>));
  /** End concept checking */
#endif

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

  OutputPixelType     m_InsideValue;
  OutputPixelType     m_OutsideValue;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryThresholdImageFilter.txx"
#endif

#endif
