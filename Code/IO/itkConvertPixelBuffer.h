/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConvertPixelBuffer.h
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
#ifndef __itkConvertPixelBuffer_h
#define __itkConvertPixelBuffer_h

#include "itkObject.h"

namespace itk
{
/**
 * \brief Class to convert blocks of data from one type to another.
 *
 * ConvertPixelBuffer has on static method Convert().  It is used by
 * itkImageFileReader to convert from an unknown type at run-time to the
 * compile-time template pixel type in itkImageFileReader.  To work with
 * complex pixel types like RGB and RGBA a traits class is used.
 * OutputConvertTraits() is the traits class.  The default one used is
 * DefaultConvertPixelTraits.  
 */
template <
  typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
class ConvertPixelBuffer
{
public:
  typedef typename OutputConvertTraits::ComponentType OutputComponentType;

  static void Convert(InputPixelType* inputData, 
                      int inputNumberOfComponents, 
                      OutputPixelType* outputData , int size);
protected:
  // Convert to Gray output
  static void ConvertGrayToGray(InputPixelType* inputData, 
                                OutputPixelType* outputData , int size);
  static void ConvertRGBToGray(InputPixelType* inputData, 
                               OutputPixelType* outputData , int size);
  static void ConvertRGBAToGray(InputPixelType* inputData, 
                               OutputPixelType* outputData , int size);
  static void ConvertMultiComponentToGray(InputPixelType* inputData, 
                                          int inputNumberOfComponents,
                                          OutputPixelType* outputData , 
                                          int size);
  // convert to RGB output
  static void ConvertGrayToRGB(InputPixelType* inputData, 
                               OutputPixelType* outputData , int size);
  static void ConvertRGBToRGB(InputPixelType* inputData, 
                               OutputPixelType* outputData , int size);
  static void ConvertRGBAToRGB(InputPixelType* inputData, 
                               OutputPixelType* outputData , int size);
  static void ConvertMultiComponentToRGB(InputPixelType* inputData, 
                                         int inputNumberOfComponents,
                                         OutputPixelType* outputData , 
                                         int size);
  // convert to RGBA output
  static void ConvertGrayToRGBA(InputPixelType* inputData, 
                                OutputPixelType* outputData , int size);
  static void ConvertRGBToRGBA(InputPixelType* inputData, 
                               OutputPixelType* outputData , int size);
  static void ConvertRGBAToRGBA(InputPixelType* inputData, 
                                OutputPixelType* outputData , int size);
  static void ConvertMultiComponentToRGBA(InputPixelType* inputData, 
                                          int inputNumberOfComponents,
                                          OutputPixelType* outputData , 
                                          int size);
  
private:
  ConvertPixelBuffer();
  ~ConvertPixelBuffer();
};
} //namespace ITK


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConvertPixelBuffer.txx"
#endif

#endif // __itkConvertPixelBuffer_h
