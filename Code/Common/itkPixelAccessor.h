/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPixelAccessor.h
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
#ifndef __itkPixelAccessor_h
#define __itkPixelAccessor_h


namespace itk
{

/**
 * \class PixelAccessor
 * \brief Give access to partial aspects of a type
 *
 * PixelAccessor is templated over an internal type and an
 * external type representation. This class encapsulates a
 * customized convertion between the internal and external
 * type representations.
 *
 * PixelAccessor is designed to be used in conjuntion with
 * ImageAdaptors. An ImageAdaptor take an image and present it
 * as another image in which the pixels are a pixel-to-pixel
 * modification of the original image.
 *
 * ImageAdaptors are intended to perform task similar to ImageFilters,
 * but reducing the overhead in memory allocation, at the price of some
 * reduction in performance.
 *
 * ImageAdaptors are templated over a PixelAccessor class that
 * will define what kind of transformation is applied to the 
 * pixel data.  Typical uses of PixelAccessor include basic type
 * casting, (e.g. make a float image looks like a unsigned int image).
 *
 * Every Image has a default PixelAccessor that performs an identity
 * operation. ImageIterators use the PixelAccessor defined by the image
 * in order to get and set the values of pixels.
 *
 */

template <class TInternalType, class TExternalType >
class ITK_EXPORT PixelAccessor  
{
public:

 /** 
   * External typedef. It defines the external aspect
   * that this class will exhibit.
   */
  typedef TExternalType ExternalType;

  /** 
   * Internal typedef. It defines the internal real
   * representation of data.
   */
  typedef TInternalType InternalType;


  inline void Set(TInternalType & output, const TExternalType & input) const
    {output = (TInternalType) input;}

  inline TExternalType Get( const TInternalType & input ) const
    {return (TExternalType)input;}

};

  
} // end namespace itk
  

#endif

