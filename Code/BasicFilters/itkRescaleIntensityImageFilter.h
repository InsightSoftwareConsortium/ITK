/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRescaleIntensityImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
  IntensityLinearTransform() {}
  ~IntensityLinearTransform() {}
  void SetFactor( RealType a ) { m_Factor = a; }
  void SetOffset( RealType b ) { m_Offset = b; }
  void SetMinimum( TOutput min ) { m_Minimum = min; }
  void SetMaximum( TOutput max ) { m_Maximum = max; }
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
  itkGetConstMacro( OutputMinimum, OutputPixelType );
  itkGetConstMacro( OutputMaximum, OutputPixelType );

  /** Get the Scale and Shift used for the linear transformation
      of gray level values. 
   \warning These Values are only valid after the filter has been updated */
  itkGetConstMacro( Scale, RealType );
  itkGetConstMacro( Shift, RealType );

  /** Get the Minimum and Maximum values of the input image.
   \warning These Values are only valid after the filter has been updated */
  itkGetConstMacro( InputMinimum, InputPixelType );
  itkGetConstMacro( InputMaximum, InputPixelType );


  /** Process to execute before entering the multithreaded section */
  void BeforeThreadedGenerateData(void);

  /** Print internal ivars */
  void PrintSelf(std::ostream& os, Indent indent) const;

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
