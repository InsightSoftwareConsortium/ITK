/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorCastImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorCastImageFilter_h
#define __itkVectorCastImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
  
/** \class VectorCastImageFilter
 *
 * \brief Casts input vector pixels to output vector pixel type.
 *
 * This filter is templated over the input image type and 
 * output image type.
 * 
 * The filter expect both images to have the same number of dimensions,
 * and that both the input and output have itk::Vector pixel types
 * of the same VectorDimension.
 *
 * \sa Vector
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */
namespace Functor {  
  
template< class TInput, class TOutput>
class VectorCast
{
public:
  VectorCast() {}
  ~VectorCast() {}
  inline TOutput operator()( const TInput & A ) const
  {
    typedef typename TOutput::ValueType OutputValueType;

    TOutput value;
    for( unsigned int k = 0; k < TOutput::Dimension; k++ )
      { value[k] = static_cast<OutputValueType>( A[k] ); }
    return value;
  }
}; 
}

template <class TInputImage, class TOutputImage>
class ITK_EXPORT VectorCastImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Functor::VectorCast< typename TInputImage::PixelType, 
                                             typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef VectorCastImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Functor::VectorCast< typename TInputImage::PixelType, 
                                                       typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  VectorCastImageFilter() {}
  virtual ~VectorCastImageFilter() {}

private:
  VectorCastImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
