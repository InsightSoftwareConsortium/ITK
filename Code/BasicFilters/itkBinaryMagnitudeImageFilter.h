/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Modulus:    itkBinaryMagnitudeImageFilter.h
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
#ifndef __itkBinaryMagnitudeImageFilter_h
#define __itkBinaryMagnitudeImageFilter_h

#include "itkBinaryImageFilter.h"

namespace itk
{
  
/** \class BinaryMagnitudeImageFilter
 * \brief Implements pixel-wise the computation of square root of the sum of squares.
 *
 * This filter is parametrized over the types of the two 
 * input images and the type of the output image. 
 *
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The filter will walk over all the pixels in the two input images, and for
 * each one of them it will do the following: 
 *
 * - cast the input 1 pixel value to \c double 
 * - cast the input 2 pixel value to \c double 
 * - compute the sum of squares of the two pixel values
 * - compute the square root of the sum
 * - cast the \c double value resulting from \c sqrt() to the pixel type of the output image 
 * - store the casted value into the output image.
 * 
 * The filter expect all images to have the same dimension (e.g. all 2D, or all 3D, or all ND)
 
 * 
 * \ingroup IntensityImageFilters
 *
 */

namespace Functor {  
  
  template< class TInput1, class TInput2, class TOutput>
  class Modulus2
  {
  public:
    Modulus2() {};
    ~Modulus2() {};
    inline TOutput operator()( const TInput1 & A, 
                               const TInput2 & B)
    {
      const double dA = static_cast<double>( A );
      const double dB = static_cast<double>( B );
      return static_cast<TOutput>( sqrt( dA*dA + dB*dB) );
    }
  }; 

}



template <class TInputImage1, class TInputImage2, class TOutputImage>
class ITK_EXPORT BinaryMagnitudeImageFilter :
    public
    BinaryImageFilter<TInputImage1,TInputImage2,TOutputImage, 
            Functor::Modulus2< 
                      typename TInputImage1::PixelType, 
                      typename TInputImage2::PixelType,
                      typename TOutputImage::PixelType>   >


{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef BinaryMagnitudeImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef BinaryImageFilter<TInputImage1,TInputImage2,TOutputImage, 
                      Functor::Modulus2< 
                      typename TInputImage1::PixelType, 
                      typename TInputImage2::PixelType,
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

  BinaryMagnitudeImageFilter() {}
  virtual ~BinaryMagnitudeImageFilter() {}
  BinaryMagnitudeImageFilter(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk


#endif
