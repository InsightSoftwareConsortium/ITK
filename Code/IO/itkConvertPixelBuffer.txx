/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConvertPixelBuffer.txx
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
#ifndef __itkConvertPixelBuffer_txx
#define __itkConvertPixelBuffer_txx

#include "itkRGBPixel.h"

namespace itk
{
template < typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
void
ConvertPixelBuffer<InputPixelType, OutputPixelType, OutputConvertTraits>
::Convert(InputPixelType* inputData, 
          int inputNumberOfComponents, 
          OutputPixelType* outputData , int size)
{
  switch(OutputConvertTraits::GetNumberOfComponents())
    {
    // output number of components is 1
    case 1:
    {
    switch(inputNumberOfComponents)
      {
      case 1:
        ConvertGrayToGray(inputData, outputData, size);
        break;
      case 3:
        ConvertRGBToGray(inputData, outputData, size);
        break;
      case 4:
        ConvertRGBAToGray(inputData, outputData, size);
        break;
      default:
        ConvertMultiComponentToGray(inputData, inputNumberOfComponents,
                                    outputData, size);
        break;
      }
    break;
    }
    // output number of components is 3 RGB
    case 3:
    {
    switch(inputNumberOfComponents)
      {
      case 1:
        ConvertGrayToRGB(inputData, outputData, size);
        break;
      case 3:
        ConvertRGBToRGB(inputData, outputData, size);
        break;
      case 4:
        ConvertRGBAToRGB(inputData, outputData, size);
      default:
        ConvertMultiComponentToRGB(inputData, inputNumberOfComponents,
                                   outputData, size);
      }
    break;
    }
    // output number of components is 4 RGBA
    case 4:
    {
    switch(inputNumberOfComponents)
      {
      case 1:
        ConvertGrayToRGBA(inputData, outputData, size);
        break;
      case 3:
        ConvertRGBToRGBA(inputData, outputData, size);
        break;
      case 4:
        ConvertRGBAToRGBA(inputData, outputData, size);
        break;
      default:
        ConvertMultiComponentToRGBA(inputData, inputNumberOfComponents,
                                    outputData, size);
      }
    break;
    }
    }
}
  
  
template < typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
void
ConvertPixelBuffer<InputPixelType, OutputPixelType, OutputConvertTraits>
::ConvertGrayToGray(InputPixelType* inputData, 
                                OutputPixelType* outputData , int size)
{
  InputPixelType* endInput = inputData + size;
  while(inputData != endInput)
    {
    OutputConvertTraits
      ::SetNthComponent(0, *outputData++,
                        static_cast<OutputComponentType>
                        (*inputData++));
    }
}


template < typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
void
ConvertPixelBuffer<InputPixelType, OutputPixelType, OutputConvertTraits>
::ConvertRGBToGray(InputPixelType* inputData, 
                   OutputPixelType* outputData , int size)
{   
  // Weights convert from linear RGB to CIE luminance assuming a
  // modern monitor.  See Charles Pontyon's Colour FAQ
  // http://www.inforamp.net/~poynton/notes/colour_and_gamma/ColorFAQ.html
  InputPixelType* endInput = inputData + size*3;
  while(inputData != endInput)
    {
    OutputComponentType val = 
      0.2125 * static_cast<OutputComponentType>(*inputData++) +
      0.7154 * static_cast<OutputComponentType>(*inputData++) +
      0.072 * static_cast<OutputComponentType>(*inputData++);
    OutputConvertTraits::SetNthComponent(0, *outputData++, val);
    }
  
}


template < typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
void
ConvertPixelBuffer<InputPixelType, OutputPixelType, OutputConvertTraits>
::ConvertRGBAToGray(InputPixelType* inputData, 
                    OutputPixelType* outputData , int size)
{   
  // Weights convert from linear RGB to CIE luminance assuming a
  // modern monitor.  See Charles Pontyon's Colour FAQ
  // http://www.inforamp.net/~poynton/notes/colour_and_gamma/ColorFAQ.html
  InputPixelType* endInput = inputData + size*4;
  while(inputData != endInput)
    {
    OutputComponentType val = 
      0.2125 * static_cast<OutputComponentType>(*inputData++) +
      0.7154 * static_cast<OutputComponentType>(*inputData++) +
      0.072 * static_cast<OutputComponentType>(*inputData++);
    val *= static_cast<OutputComponentType>(*inputData++);
    OutputConvertTraits::SetNthComponent(0, *outputData++, val);
    }
  
}



template < typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
void
ConvertPixelBuffer<InputPixelType, OutputPixelType, OutputConvertTraits>
::ConvertMultiComponentToGray(InputPixelType* inputData, 
                              int inputNumberOfComponents,
                              OutputPixelType* outputData , int size)
{
  // 2 components assumed intensity and alpha
  if(inputNumberOfComponents == 2)
    {
    InputPixelType* endInput = inputData + size;
    while(inputData != endInput)
      {
      OutputComponentType val = 
        static_cast<OutputComponentType>(*inputData++) *
        static_cast<OutputComponentType>(*inputData++);
      OutputConvertTraits::SetNthComponent(0, *outputData++, val);
      }
    }
  // just skip the rest of the data
  else
    {
    // Weights convert from linear RGB to CIE luminance assuming a
    // modern monitor.  See Charles Pontyon's Colour FAQ
    // http://www.inforamp.net/~poynton/notes/colour_and_gamma/ColorFAQ.html
    int diff = inputNumberOfComponents - 4;
    InputPixelType* endInput = inputData + size*inputNumberOfComponents;
    while(inputData != endInput)
      {
      OutputComponentType val = 
        0.2125 * static_cast<OutputComponentType>(*inputData++) +
        0.7154 * static_cast<OutputComponentType>(*inputData++) +
        0.072 * static_cast<OutputComponentType>(*inputData++);
      val *= static_cast<OutputComponentType>(*inputData++);
      inputData += diff;
      OutputConvertTraits::SetNthComponent(0, *outputData++, val);
      }
    }
}

  
template < typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
void
ConvertPixelBuffer<InputPixelType, OutputPixelType, OutputConvertTraits>
::ConvertGrayToRGB(InputPixelType* inputData, 
                 OutputPixelType* outputData , int size)
{
  InputPixelType* endInput = inputData + size;
  while(inputData != endInput)
    {
    OutputConvertTraits
      ::SetNthComponent(0, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData));
    OutputConvertTraits
      ::SetNthComponent(1, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData));
    OutputConvertTraits
      ::SetNthComponent(2, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData));
    inputData++;
    outputData++;
    }
}


template < typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
void
ConvertPixelBuffer<InputPixelType, OutputPixelType, OutputConvertTraits>
::ConvertRGBToRGB(InputPixelType* inputData, 
                  OutputPixelType* outputData , int size)
{
  InputPixelType* endInput = inputData + size * 3;
  while(inputData != endInput)
    {
    OutputConvertTraits
      ::SetNthComponent(0, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData++));
    OutputConvertTraits
      ::SetNthComponent(1, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData++));
    OutputConvertTraits
      ::SetNthComponent(2, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData++));
    outputData++;
    }
}

template < typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
void
ConvertPixelBuffer<InputPixelType, OutputPixelType, OutputConvertTraits>
::ConvertRGBAToRGB(InputPixelType* inputData, 
                   OutputPixelType* outputData , int size)
{  
  InputPixelType* endInput = inputData + size* 4;
  while(inputData != endInput)
    {
    OutputComponentType alpha =
      static_cast<OutputComponentType>(inputData[3]);
    OutputConvertTraits
      ::SetNthComponent(0, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData++)*alpha);
    OutputConvertTraits
      ::SetNthComponent(1, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData++)*alpha);
    OutputConvertTraits
      ::SetNthComponent(2, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData++)*alpha);
    inputData++; // skip alpha
    outputData++;
    }
}

template < typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
void
ConvertPixelBuffer<InputPixelType, OutputPixelType, OutputConvertTraits>
::ConvertMultiComponentToRGB(InputPixelType* inputData, 
                             int inputNumberOfComponents,
                             OutputPixelType* outputData , 
                             int size)
{
  // assume intensity alpha
  if(inputNumberOfComponents == 2)
    {
    InputPixelType* endInput = inputData + size;
    while(inputData != endInput)
      {
      OutputComponentType val =  
        static_cast<OutputComponentType>(*inputData++) * 
        static_cast<OutputComponentType>(*inputData++);
      OutputConvertTraits
        ::SetNthComponent(0, *outputData, val);
      OutputConvertTraits
        ::SetNthComponent(1, *outputData, val);
      OutputConvertTraits
        ::SetNthComponent(2, *outputData, val);
      outputData++;
      }
    }
  // just skip the rest of the data
  else
    {
    int diff = inputNumberOfComponents - 4;
    InputPixelType* endInput = inputData + size;
    while(inputData != endInput)
      {
      OutputConvertTraits
        ::SetNthComponent(0, *outputData, 
                          static_cast<OutputComponentType>
                          (*inputData++));
      OutputConvertTraits
        ::SetNthComponent(1, *outputData, 
                          static_cast<OutputComponentType>
                          (*inputData++));
      OutputConvertTraits
        ::SetNthComponent(2, *outputData, 
                          static_cast<OutputComponentType>
                          (*inputData++));
      inputData += diff;
      outputData++;
      }
    }
}


template < typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
void
ConvertPixelBuffer<InputPixelType, OutputPixelType, OutputConvertTraits>
::ConvertGrayToRGBA(InputPixelType* inputData, 
                                OutputPixelType* outputData , int size)
  
{
  InputPixelType* endInput = inputData + size;
  while(inputData != endInput)
    {
    OutputConvertTraits
      ::SetNthComponent(0, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData));
    OutputConvertTraits
      ::SetNthComponent(1, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData));
    OutputConvertTraits
      ::SetNthComponent(2, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData));
    OutputConvertTraits
      ::SetNthComponent(3, *outputData, 
                        static_cast<OutputComponentType>
                        (1));
    inputData++;
    outputData++;
    }
}

template < typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
void
ConvertPixelBuffer<InputPixelType, OutputPixelType, OutputConvertTraits>
::ConvertRGBToRGBA(InputPixelType* inputData, 
                   OutputPixelType* outputData , int size)
{
  InputPixelType* endInput = inputData + size * 4;
  while(inputData != endInput)
    {
    OutputComponentType alpha =
      static_cast<OutputComponentType>(inputData[3]);
    OutputConvertTraits
      ::SetNthComponent(0, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData++));
    OutputConvertTraits
      ::SetNthComponent(1, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData++));
    OutputConvertTraits
      ::SetNthComponent(2, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData++));
    OutputConvertTraits
      ::SetNthComponent(3, *outputData, 
                        static_cast<OutputComponentType>
                        (1));
    outputData++;
    }
}


template < typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
void
ConvertPixelBuffer<InputPixelType, OutputPixelType, OutputConvertTraits>
::ConvertRGBAToRGBA(InputPixelType* inputData, 
                  OutputPixelType* outputData , int size)
{
  InputPixelType* endInput = inputData + size*4;
  while(inputData != endInput)
    {
    OutputComponentType alpha =
      static_cast<OutputComponentType>(inputData[3]);
    OutputConvertTraits
      ::SetNthComponent(0, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData++));
    OutputConvertTraits
      ::SetNthComponent(1, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData++));
    OutputConvertTraits
      ::SetNthComponent(2, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData++));
    OutputConvertTraits
      ::SetNthComponent(3, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData++));
    outputData++;
    }
}

template < typename InputPixelType,
  typename OutputPixelType,
  class OutputConvertTraits
>
void
ConvertPixelBuffer<InputPixelType, OutputPixelType, OutputConvertTraits>
::ConvertMultiComponentToRGBA(InputPixelType* inputData, 
                              int inputNumberOfComponents,
                              OutputPixelType* outputData , 
                              int size)
{
  // equal weights for 2 components??
  if(inputNumberOfComponents == 2)
    {
    InputPixelType* endInput = inputData + size;
    while(inputData != endInput)
      { 
      OutputComponentType val = 
        static_cast<OutputComponentType>(*inputData++);
      OutputComponentType alpha = 
        static_cast<OutputComponentType>(*inputData++);
      OutputConvertTraits
        ::SetNthComponent(0, *outputData, val);
      OutputConvertTraits
        ::SetNthComponent(1, *outputData, val);
      OutputConvertTraits
        ::SetNthComponent(2, *outputData, val);
      OutputConvertTraits
        ::SetNthComponent(3, *outputData, alpha);
      }
    }
  else
    {
    int diff = inputNumberOfComponents - 4;
    InputPixelType* endInput = inputData + size;
    while(inputData != endInput)
      {
      OutputConvertTraits
        ::SetNthComponent(0, *outputData, 
                          static_cast<OutputComponentType>
                          (*inputData++));
      OutputConvertTraits
        ::SetNthComponent(1, *outputData, 
                          static_cast<OutputComponentType>
                          (*inputData++));
      OutputConvertTraits
        ::SetNthComponent(2, *outputData, 
                          static_cast<OutputComponentType>
                          (*inputData++));
      OutputConvertTraits
        ::SetNthComponent(3, *outputData, 
                          static_cast<OutputComponentType>
                          (*inputData++));
      inputData += diff;
      outputData++;
      }
    }
}

 

}// end namespace itk

#endif
