/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAbsImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAbsImageFilter_h
#define __itkAbsImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
  
/** \class AbsImageFilter
 * \brief Computes the ABS(x) pixel-wise
 * 
 * \ingroup IntensityImageFilters  Multithreaded
 */
namespace Function {  
  
template< class TInput, class TOutput>
class Abs
{
public:
  Abs() {}
  ~Abs() {}
  bool operator!=( const Abs & other ) const
  {
    return false;
   }
  bool operator==( const Abs & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput & A )
  { return (TOutput)( ( A > 0 ) ? A : -A ); }
}; 
}

template <class TInputImage, class TOutputImage>
class ITK_EXPORT AbsImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Function::Abs< 
  typename TInputImage::PixelType, 
  typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef AbsImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Function::Abs< typename TInputImage::PixelType, 
                                                 typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  AbsImageFilter() {}
  virtual ~AbsImageFilter() {}

private:
  AbsImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
