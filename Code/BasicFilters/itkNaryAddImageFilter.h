/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNaryAddImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkNaryAddImageFilter_h
#define __itkNaryAddImageFilter_h

#include "itkNaryFunctorImageFilter.h"
#include "itkNumericTraits.h"


namespace itk
{
  
/** \class NaryAddImageFilter
 * \brief Implements an operator for pixel-wise addition of two images.
 *
 * This class is parametrized over the types of the two 
 * input images and the type of the output image. 
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The pixel type of the input 1 image must have a valid defintion of
 * the operator+ with a pixel type of the image 2. This condition is 
 * required because internally this filter will perform the operation
 *
 *        pixel_from_image_1 + pixel_from_image_2
 *
 * Additionally the type resulting from the sum, will be cast to
 * the pixel type of the output image.
 * 
 * The total operation over one pixel will be
 *
 *  output_pixel = static_cast<OutputPixelType>( input1_pixel + input2_pixel )
 *
 * For example, this filter could be used directly for adding images whose
 * pixels are vectors of the same dimension, and to store the resulting vector
 * in an output image of vector pixels.
 *
 * \warning No numeric overflow checking is performed in this filter.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 *
 */

namespace Functor {  
  
  template< class TInput, class TOutput >
  class Add1
  {
  public:
    typedef typename NumericTraits< TInput >::AccumulateType AccumulatorType;
    Add1() {};
    ~Add1() {};
    inline TOutput operator()( const TOutput & A, const TInput & B)
    {
      const AccumulatorType sum = A;
      return static_cast<TOutput>( sum + B );
    }
  }; 

}
// Wrap: NaryAddImageFilter<$Image,$Image,$Function>
// Wrap: <XML code for Function....>
// Wrap: NaryAddImageFilter<Image<$BasicPixel,$BasicDimension>,$Image,$Function>
template <class TInputImage, class TOutputImage>
class ITK_EXPORT NaryAddImageFilter :
    public
    NaryFunctorImageFilter<TInputImage,TOutputImage, 
    Functor::Add1<  typename TInputImage::PixelType, 
                    typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef NaryAddImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef NaryFunctorImageFilter<TInputImage,TOutputImage, 
    Functor::Add1< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   
                >  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
protected:

  NaryAddImageFilter() {}
  virtual ~NaryAddImageFilter() {}
  NaryAddImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
