/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEdgePotentialImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkEdgePotentialImageFilter_h
#define __itkEdgePotentialImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
  
/** \class EdgePotentialImageFilter
 *
 * \brief Computes the edge potential of an image from the image gradient.
 *
 * Input to this filter should be a CovariantVector image representing
 * the image gradient.
 * 
 * The filter expect both the input and output images to have the same 
 * number of dimensions, and the output to be of a scalar image type.
 *
 */
namespace Functor {  
  
template< class TInput, class TOutput>
class EdgePotential
{
public:
  EdgePotential() {};
  ~EdgePotential() {};
  inline TOutput operator()( const TInput & A )
  {
    return static_cast<TOutput>( exp( -1.0 * A.GetNorm() ) );
  }
};
}

template <class TInputImage, class TOutputImage>
class ITK_EXPORT EdgePotentialImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Functor::EdgePotential< 
  typename TInputImage::PixelType, 
  typename TOutputImage::PixelType> >
{
public:
  /** Standard class typedefs. */
  typedef EdgePotentialImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Functor::EdgePotential< 
    typename TInputImage::PixelType, 
    typename TOutputImage::PixelType>   
  >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  EdgePotentialImageFilter() {}
  virtual ~EdgePotentialImageFilter() {}

private:
  EdgePotentialImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};


} // end namespace itk


#endif
