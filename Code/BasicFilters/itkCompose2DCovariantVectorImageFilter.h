/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompose2DCovariantVectorImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCompose2DCovariantVectorImageFilter_h
#define __itkCompose2DCovariantVectorImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkCovariantVector.h"

/** \class Compose2DCovariantVectorImageFilter
 * \brief Implements pixel-wise composition of an 2D covariant vector pixel from two scalar images.
 *
 * This filter receives two scalar images as input. Each image containing
 * one of the 2D covariant vector components. The filter produces as output a
 * 2D covariant vector image in which the two components have been unified. The Component
 * type is preserved from the PixelType of the input images.
 *
 * \ingroup IntensityImageFilters
 */

namespace itk
{
  
namespace Function {  
  
template< class TInput >
class Compose2DCovariantVector
{
public:
  typedef CovariantVector<TInput,2> OutputType;
  Compose2DCovariantVector() {}
  ~Compose2DCovariantVector() {}
  bool operator!=( const Compose2DCovariantVector & other ) const
  {
    return false;
  }
  bool operator==( const Compose2DCovariantVector & other ) const
  {
    return !(*this != other);
  }
  inline OutputType operator()(  const TInput & s1, 
                                 const TInput & s2 )
  {
    OutputType v;
    v[0] = s1;
    v[1] = s2;
    return v;
  }
}; 
}

template <typename TInputImage, 
          typename TOutputImage= 
          Image< CovariantVector< ITK_TYPENAME TInputImage::PixelType,2 >,
                 ::itk::GetImageDimension<TInputImage>::ImageDimension > >
class ITK_EXPORT Compose2DCovariantVectorImageFilter :
    public
BinaryFunctorImageFilter<TInputImage,TInputImage,
                          TOutputImage, 
                          Function::Compose2DCovariantVector< ITK_TYPENAME TInputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef Compose2DCovariantVectorImageFilter  Self;
  typedef BinaryFunctorImageFilter<TInputImage,TInputImage,
                                    TOutputImage, 
                                    Function::Compose2DCovariantVector< 
    ITK_TYPENAME TInputImage::PixelType > 
  >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef typename Superclass::OutputImageType OutputImageType;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  Compose2DCovariantVectorImageFilter() {}
  virtual ~Compose2DCovariantVectorImageFilter() {}

private:
  Compose2DCovariantVectorImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


};

} // end namespace itk


#endif
