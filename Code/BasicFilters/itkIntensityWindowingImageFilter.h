/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIntensityWindowingImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkIntensityWindowingImageFilter_h
#define __itkIntensityWindowingImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{

// This functor class applies a linear transformation A.x + B inside a specified 
// range. Values below the range are mapped to a constant. Values over the range
// are mapped to another constant.
namespace Functor {  
 
template< typename TInput, typename  TOutput>
class IntensityWindowingTransform
{
public:
  typedef typename NumericTraits< TInput >::RealType RealType;
  IntensityWindowingTransform() {}
  ~IntensityWindowingTransform() {}
  void SetFactor( RealType a ) { m_Factor = a; }
  void SetOffset( RealType b ) { m_Offset = b; }
  void SetOutputMinimum( TOutput min ) { m_OutputMinimum = min; }
  void SetOutputMaximum( TOutput max ) { m_OutputMaximum = max; }
  void SetWindowMinimum( TInput  min ) { m_WindowMinimum = min; }
  void SetWindowMaximum( TInput  max ) { m_WindowMaximum = max; }
  inline TOutput operator()( const TInput & x )
  {
    if( x < m_WindowMinimum )
      {
      return m_OutputMinimum;
      }
    if( x > m_WindowMaximum )
      {
      return m_OutputMaximum;
      }
    const RealType value  = static_cast<RealType>(x) * m_Factor + m_Offset;
    const TOutput  result = static_cast<TOutput>( value );
    return result;
  }
private:
  RealType m_Factor;
  RealType m_Offset;
  TOutput  m_OutputMaximum;
  TOutput  m_OutputMinimum;
  TInput   m_WindowMaximum;
  TInput   m_WindowMinimum;
}; 

}  // end namespace functor


/** \class IntensityWindowingImageFilter
 * \brief Applies a linear transformation to the intensity levels of the
 * input Image that are inside a user-defined interval. Values below this
 * interval are mapped to a constant. Values over the interval are mapped 
 * to another constant. 
 *
 * IntensityWindowingImageFilter applies pixel-wise a linear transformation
 * to the intensity values of input image pixels. The linear transformation
 * is defined by the user in terms of the minimum and maximum values that 
 * the output image should have and the lower and upper limits of the intensity
 * window of the input image. This operation is very common in visualization,
 * and can also be applied as a convenient preprocessing operation for image
 * segmentation.
 * 
 * All computations are performed in the precison of the input pixel's 
 * RealType. Before assigning the computed value to the output pixel. 
 *
 * \ingroup IntensityImageFilters  Multithreaded
 *
 */
template <typename  TInputImage, typename  TOutputImage=TInputImage>
class ITK_EXPORT IntensityWindowingImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Functor::IntensityWindowingTransform< 
  typename TInputImage::PixelType, 
  typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef IntensityWindowingImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Functor::IntensityWindowingTransform< 
    typename TInputImage::PixelType, 
    typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef typename NumericTraits<InputPixelType>::RealType RealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Set/Get the values of the maximum and minimum 
   *  intensities of the outputimage */
  itkSetMacro( OutputMinimum, OutputPixelType );
  itkSetMacro( OutputMaximum, OutputPixelType );
  itkGetConstMacro( OutputMinimum, OutputPixelType );
  itkGetConstMacro( OutputMaximum, OutputPixelType );

  /** Set/Get the values of the maximum and minimum 
   *  intensities of the input intensity window */
  itkSetMacro( WindowMinimum, InputPixelType );
  itkSetMacro( WindowMaximum, InputPixelType );
  itkGetConstMacro( WindowMinimum, InputPixelType );
  itkGetConstMacro( WindowMaximum, InputPixelType );

  /** Get the Scale and Shift used for the linear transformation
      of gray level values. 
   \warning These Values are only valid after the filter has been updated */
  itkGetConstMacro( Scale, RealType );
  itkGetConstMacro( Shift, RealType );

  /** Process to execute before entering the multithreaded section */
  void BeforeThreadedGenerateData(void);

  /** Print internal ivars */
  void PrintSelf(std::ostream& os, Indent indent) const;

protected:
  IntensityWindowingImageFilter();
  virtual ~IntensityWindowingImageFilter() {};

private:
  IntensityWindowingImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  RealType m_Scale;
  RealType m_Shift;

  InputPixelType        m_WindowMinimum;
  InputPixelType        m_WindowMaximum;

  OutputPixelType       m_OutputMinimum;
  OutputPixelType       m_OutputMaximum;

};


  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkIntensityWindowingImageFilter.txx"
#endif
  
#endif
