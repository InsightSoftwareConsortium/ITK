/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExpImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkExpImageFilter_h
#define __itkExpImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "vnl/vnl_math.h"

#if defined(_MSC_VER) && (_MSC_VER >= 1300)
#pragma function(exp)
#endif

namespace itk
{
  
/** \class ExpImageFilter
 * \brief Computes the exp(x) pixel-wise
 *
 * 
 * \ingroup IntensityImageFilters  Multithreaded
 *
 */

namespace Function {  
  template< class TInput, class TOutput>
  class Exp
  {
  public:
    Exp() {};
    ~Exp() {};
    inline TOutput operator()( const TInput & A )
    {
      return (TOutput)exp((double)A);
    }
  }; 
}
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ExpImageFilter :
    public
    UnaryFunctorImageFilter<TInputImage,TOutputImage, 
    Function::Exp< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef ExpImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
    Function::Exp< typename TInputImage::PixelType, 
                   typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  ExpImageFilter() {}
  virtual ~ExpImageFilter() {}

private:
  ExpImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


};

} // end namespace itk


#endif
