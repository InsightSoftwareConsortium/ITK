/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarToArrayCastImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkScalarToArrayCastImageFilter_h
#define __itkScalarToArrayCastImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
  
/** \class ScalarToArrayCastImageFilter
 *
 * \brief Casts input vector pixels to output vector pixel type.
 *
 * This filter is templated over the input image type and 
 * output image type.
 * 
 * The filter expect the input image' pixel type is of scalar type and 
 * the output image' pixel type is one dimensional array (subclasses of 
 * FixedArray) of the scalar type.
 *
 * \sa FixedArray
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */

namespace Functor {  
  
  template< class TInput, class TOutput>
  class ScalarToArrayCast
  {
  public:
    ScalarToArrayCast() {}
    ~ScalarToArrayCast() {}
    inline TOutput operator()( const TInput & A )
      {
      typedef typename TOutput::ValueType OutputValueType;

      TOutput value;
      value[0] = static_cast<OutputValueType>( A );
      return value;
    }
  }; 
}

template <class TInputImage, class TOutputImage>
class ITK_EXPORT ScalarToArrayCastImageFilter :
    public
    UnaryFunctorImageFilter<TInputImage,TOutputImage, 
    Functor::ScalarToArrayCast< typename TInputImage::PixelType, 
                         typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef ScalarToArrayCastImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
    Functor::ScalarToArrayCast< typename TInputImage::PixelType, 
    typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  ScalarToArrayCastImageFilter() {}
  virtual ~ScalarToArrayCastImageFilter() {}

private:
  ScalarToArrayCastImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
