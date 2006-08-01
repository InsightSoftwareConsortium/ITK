/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRescaleIntensityImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRescaleIntensityImageFilter_h
#define __itkRescaleIntensityImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{

// This functor class applies a linear transformation A.x + B 
// to input values.
namespace Functor {  
 
template< typename TInput, typename  TOutput>
class IntensityLinearTransform
{
public:
  typedef typename NumericTraits< TInput >::RealType RealType;
  IntensityLinearTransform()
  {
    m_Factor = 1.0;
    m_Offset = 0.0;
    m_Minimum = NumericTraits<TOutput>::NonpositiveMin();
    m_Maximum = NumericTraits<TOutput>::max();
  }
  ~IntensityLinearTransform() {}
  void SetFactor( RealType a ) { m_Factor = a; }
  void SetOffset( RealType b ) { m_Offset = b; }
  void SetMinimum( TOutput min ) { m_Minimum = min; }
  void SetMaximum( TOutput max ) { m_Maximum = max; }
  bool operator!=( const IntensityLinearTransform & other ) const
  {
    if( m_Factor != other.m_Factor ||
        m_Offset != other.m_Offset ||
        m_Maximum != other.m_Maximum    ||
        m_Minimum != other.m_Minimum )
      {
      return true;
      }
    return false;
   }
  bool operator==( const IntensityLinearTransform & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput & x )
  {
    RealType value  = static_cast<RealType>(x) * m_Factor + m_Offset;
    TOutput  result = static_cast<TOutput>( value );
    result = ( result > m_Maximum ) ? m_Maximum : result;
    result = ( result < m_Minimum ) ? m_Minimum : result;
    return result;
  }
private:
  RealType m_Factor;
  RealType m_Offset;
  TOutput  m_Maximum;
  TOutput  m_Minimum;
}; 

}  // end namespace functor


/** \class RescaleIntensityImageFilter
 * \brief Applies a linear transformation to the intensity levels of the
 * input Image. 
 *
 * RescaleIntensityImageFilter applies pixel-wise a linear transformation
 * to the intensity values of input image pixels. The linear transformation
 * is defined by the user in terms of the minimum and maximum values that 
 * the output image should have.
 * 
 * All computations are performed in the precison of the input pixel's 
 * RealType. Before assigning the computed value to the output pixel. 
 *
 * NOTE: In this filter the minimum and maximum values of the input image are
 * computed internally using the MinimumMaximumImageCalculator. Users are not
 * supposed to set those values in this filter. If you need a filter where you
 * can set the minimum and maximum values of the input, please use the
 * IntensityWindowingImageFilter. If you want a filter that can use a
 * user-defined linear transformation for the intensity, then please use teh
 * ShiftScaleImageFilter.
 *
 * \sa IntensityWindowingImageFilter
 * 
 * \ingroup IntensityImageFilters  Multithreaded
 *
 */
template <typename  TInputImage, typename  TOutputImage=TInputImage>
class ITK_EXPORT RescaleIntensityImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Functor::IntensityLinearTransform< 
  typename TInputImage::PixelType, 
  typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef RescaleIntensityImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Functor::IntensityLinearTransform< 
    typename TInputImage::PixelType, 
    typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef typename NumericTraits<InputPixelType>::RealType RealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  itkSetMacro( OutputMinimum, OutputPixelType );
  itkSetMacro( OutputMaximum, OutputPixelType );
  itkGetConstReferenceMacro( OutputMinimum, OutputPixelType );
  itkGetConstReferenceMacro( OutputMaximum, OutputPixelType );

  /** Get the Scale and Shift used for the linear transformation
      of gray level values. 
   \warning These Values are only valid after the filter has been updated */
  itkGetConstReferenceMacro( Scale, RealType );
  itkGetConstReferenceMacro( Shift, RealType );

  /** Get the Minimum and Maximum values of the input image.
   \warning These Values are only valid after the filter has been updated */
  itkGetConstReferenceMacro( InputMinimum, InputPixelType );
  itkGetConstReferenceMacro( InputMaximum, InputPixelType );

  /** Process to execute before entering the multithreaded section */
  void BeforeThreadedGenerateData(void);

  /** Print internal ivars */
  void PrintSelf(std::ostream& os, Indent indent) const;

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<InputPixelType>));
  itkConceptMacro(OutputHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<OutputPixelType>));
  itkConceptMacro(RealTypeMultiplyOperatorCheck,
                  (Concept::MultiplyOperator<RealType>));
  itkConceptMacro(RealTypeAdditiveOperatorsCheck,
                  (Concept::AdditiveOperators<RealType>));
  /** End concept checking */
#endif

protected:
  RescaleIntensityImageFilter();
  virtual ~RescaleIntensityImageFilter() {};

private:
  RescaleIntensityImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  RealType m_Scale;
  RealType m_Shift;

  InputPixelType        m_InputMinimum;
  InputPixelType        m_InputMaximum;

  OutputPixelType       m_OutputMinimum;
  OutputPixelType       m_OutputMaximum;

};


  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRescaleIntensityImageFilter.txx"
#endif
  
#endif
