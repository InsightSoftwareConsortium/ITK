/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTensorFractionalAnisotropyImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTensorFractionalAnisotropyImageFilter_h
#define __itkTensorFractionalAnisotropyImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{

// This functor class invokes the computation of fractional anisotropy from
// every pixel.
namespace Functor {  
 
template< typename TInput >
class TensorFractionalAnisotropyFunction
{
public:
  typedef typename TInput::RealValueType  RealValueType;
  TensorFractionalAnisotropyFunction() {}
  ~TensorFractionalAnisotropyFunction() {}
  bool operator!=( const TensorFractionalAnisotropyFunction & ) const
  {
    return false;
  }
  bool operator==( const TensorFractionalAnisotropyFunction & other ) const
  {
    return !(*this != other);
  }
  inline RealValueType operator()( const TInput & x )
    {
    return x.GetFractionalAnisotropy();
    }
}; 

}  // end namespace functor


/** \class TensorFractionalAnisotropyImageFilter
 * \brief Computes the Fractional Anisotropy for every pixel of a input tensor image.
 *
 * TensorFractionalAnisotropyImageFilter applies pixel-wise the invokation for
 * computing the fractional anisotropy of every pixel. The pixel type of the
 * input image is expected to implement a method GetFractionalAnisotropy(), and
 * to specify its return type as  RealValueType.
 * 
 * \sa TensorRelativeAnisotropyImageFilter
 * \sa DiffusionTensor3D
 * 
 * \ingroup IntensityImageFilters  Multithreaded  TensorObjects
 *
 */
template <typename  TInputImage, typename  TOutputImage=TInputImage>
class ITK_EXPORT TensorFractionalAnisotropyImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Functor::TensorFractionalAnisotropyFunction< 
                                        typename TInputImage::PixelType> > 
{
public:
  /** Standard class typedefs. */
  typedef TensorFractionalAnisotropyImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Functor::TensorFractionalAnisotropyFunction< 
                                    typename TInputImage::PixelType> >  Superclass;

  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef typename Superclass::OutputImageType    OutputImageType;
  typedef typename TOutputImage::PixelType        OutputPixelType;
  typedef typename TInputImage::PixelType         InputPixelType;
  typedef typename InputPixelType::ValueType      InputValueType;


  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Print internal ivars */
  void PrintSelf(std::ostream& os, Indent indent) const
    { this->Superclass::PrintSelf( os, indent ); }
  

protected:
  TensorFractionalAnisotropyImageFilter() {};
  virtual ~TensorFractionalAnisotropyImageFilter() {};

private:
  TensorFractionalAnisotropyImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};


  
} // end namespace itk
  
#endif
