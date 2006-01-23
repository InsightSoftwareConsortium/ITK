/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompose3DCovariantVectorImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCompose3DCovariantVectorImageFilter_h
#define __itkCompose3DCovariantVectorImageFilter_h

#include "itkTernaryFunctorImageFilter.h"
#include "itkCovariantVector.h"

/** \class Compose3DCovariantVectorImageFilter
 * \brief Implements pixel-wise composition of an 3D covariant vector pixel from three scalar images.
 *
 * This filter receives three scalar images as input. Each image
 * containing one of the 3D covariant vector components. The filter produces as
 * output a 3D covariant vector image in which the three components have been
 * unified. The Component type is preserved from the PixelType of the
 * input images.
 *
 * \ingroup IntensityImageFilters
 */

namespace itk
{
  
namespace Function {  
  
template< class TInput >
class Compose3DCovariantVector
{
public:
  typedef CovariantVector<TInput,3> OutputType;
  Compose3DCovariantVector() {}
  ~Compose3DCovariantVector() {}
  bool operator!=( const Compose3DCovariantVector & ) const
  {
    return false;
  }
  bool operator==( const Compose3DCovariantVector & other ) const
  {
    return !(*this != other);
  }
  inline OutputType operator()(  const TInput & s1, 
                                 const TInput & s2,
                                 const TInput & s3)
  {
    OutputType v;
    v[0] = s1;
    v[1] = s2;
    v[2] = s3;
    return v;
  }
}; 
}

template <typename TInputImage, 
          typename TOutputImage= 
          Image< CovariantVector< ITK_TYPENAME TInputImage::PixelType,3 >,
                 ::itk::GetImageDimension<TInputImage>::ImageDimension > >
class ITK_EXPORT Compose3DCovariantVectorImageFilter :
    public
TernaryFunctorImageFilter<TInputImage,TInputImage,
                          TInputImage,TOutputImage, 
                          Function::Compose3DCovariantVector< ITK_TYPENAME TInputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef Compose3DCovariantVectorImageFilter  Self;
  typedef TernaryFunctorImageFilter<TInputImage,TInputImage,TInputImage,
                                    TOutputImage, 
                                    Function::Compose3DCovariantVector< 
    ITK_TYPENAME TInputImage::PixelType > 
  >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef typename Superclass::OutputImageType OutputImageType;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  Compose3DCovariantVectorImageFilter() {}
  virtual ~Compose3DCovariantVectorImageFilter() {}

private:
  Compose3DCovariantVectorImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


};

} // end namespace itk


#endif
