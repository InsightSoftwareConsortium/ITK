/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaskImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMaskImageFilter_h
#define __itkMaskImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkNumericTraits.h"


namespace itk
{
  
/** \class MaskImageFilter
 * \brief Implements an operator for pixel-wise masking of the input 
 * image with the mask.
 *
 * This class is parametrized over the types of the  
 * input image type, the mask image type and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The pixel type of the input 1 image must have a valid defintion of
 * the operator > with a pixel type of the image 2. This condition is 
 * required because internally this filter will perform the operation
 *
 *        if pixel_from_mask_image == 0 pixel_input_image = 0
 *
 * The result from the sum, is cast to the pixel type of the output image.
 *
 * Note that the input and the mask images must be of the same size.
 *
 * \warning Any pixel value other than 0 will not be masked out. 
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */
namespace Functor {  
  
template< class TInput, class TMask, class TOutput >
class MaskInput
{
public:
  typedef typename NumericTraits< TInput >::AccumulateType AccumulatorType;

  MaskInput() {};
  ~MaskInput() {};
  inline TOutput operator()( const TInput & A, const TMask & B)
  {
    if (B != 0) 
      {
      return static_cast<TOutput>( A );
      }
    else
      return NumericTraits< TOutput >::Zero;
        
  }
}; 

}
template <class TInputImage, class TMaskImage, class TOutputImage>
class ITK_EXPORT MaskImageFilter :
    public
BinaryFunctorImageFilter<TInputImage,TMaskImage,TOutputImage, 
                         Functor::MaskInput< 
  typename TInputImage::PixelType, 
  typename TMaskImage::PixelType,
  typename TOutputImage::PixelType>   >


{
public:
  /** Standard class typedefs. */
  typedef MaskImageFilter  Self;
  typedef BinaryFunctorImageFilter<TInputImage,TMaskImage,TOutputImage, 
                                   Functor::MaskInput< 
    typename TInputImage::PixelType, 
    typename TMaskImage::PixelType,
    typename TOutputImage::PixelType>   
  >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
protected:
  MaskImageFilter() {}
  virtual ~MaskImageFilter() {}

private:
  MaskImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
