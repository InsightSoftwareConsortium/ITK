/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSquareImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSquareImageFilter_h
#define __itkSquareImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

  
/** \class SquareImageFilter
 * \brief Computes the square of the intensity values pixel-wise
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */

namespace itk
{
namespace Function {  
  
  template< class TInput, class TOutput>
  class Square
  {
  public:
    typedef typename NumericTraits<TInput>::RealType RealType;
    Square() {}
    ~Square() {}
    inline TOutput operator()( const TInput & A )
    {
      const RealType ra = static_cast<RealType>( A );
      return static_cast<TOutput>( ra * ra );
    }
  }; 
}
template <class TInputImage, class TOutputImage>
class ITK_EXPORT SquareImageFilter :
    public
    UnaryFunctorImageFilter<TInputImage,TOutputImage, 
    Function::Square< typename TInputImage::PixelType, 
                    typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef SquareImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
    Function::Square< typename TInputImage::PixelType, 
                    typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  SquareImageFilter() {}
  virtual ~SquareImageFilter() {}

private:
  SquareImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
