/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConvertPixelBuffer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConvertPixelBuffer_txx
#define __itkConvertPixelBuffer_txx
#include "itkConvertPixelBuffer.h"

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
                        (*inputData));
    inputData++;
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
    OutputComponentType val = static_cast<OutputComponentType>( 
      0.2125 * static_cast<OutputComponentType>(*inputData) +
      0.7154 * static_cast<OutputComponentType>(*(inputData+1)) +
      0.0721 * static_cast<OutputComponentType>(*(inputData+2))   );
    inputData += 3;
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
    double tempval = 
      (
        0.2125 * static_cast<double>(*inputData) +
        0.7154 * static_cast<double>(*(inputData+1)) +
        0.0721 * static_cast<double>(*(inputData+2))
        ) * static_cast<double>(*(inputData+3));
    inputData += 4;
    OutputComponentType val = static_cast<OutputComponentType>( tempval );
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
        static_cast<OutputComponentType>(*inputData) *
        static_cast<OutputComponentType>(*(inputData+1));
      inputData += 2;
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
      double tempval = 
        (
          0.2125 * static_cast<double>(*inputData) +
          0.7154 * static_cast<double>(*(inputData+1)) +
          0.0721 * static_cast<double>(*(inputData+2)) 
          ) * static_cast<double>(*(inputData+3));
      inputData += 4;
      OutputComponentType val = static_cast<OutputComponentType>( tempval );
      OutputConvertTraits::SetNthComponent(0, *outputData++, val);
      inputData += diff;
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
                        (*inputData));
    OutputConvertTraits
      ::SetNthComponent(1, *outputData, 
                        static_cast<OutputComponentType>
                        (*(inputData+1)));
    OutputConvertTraits
      ::SetNthComponent(2, *outputData, 
                        static_cast<OutputComponentType>
                        (*(inputData+2)));
    inputData += 3;
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
                        (*inputData)*alpha);
    OutputConvertTraits
      ::SetNthComponent(1, *outputData, 
                        static_cast<OutputComponentType>
                        (*(inputData+1))*alpha);
    OutputConvertTraits
      ::SetNthComponent(2, *outputData, 
                        static_cast<OutputComponentType>
                        (*(inputData+2))*alpha);
    inputData += 3;
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
        static_cast<OutputComponentType>(*inputData) * 
        static_cast<OutputComponentType>((*inputData+1));
      inputData += 2;
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
                          (*inputData));
      OutputConvertTraits
        ::SetNthComponent(1, *outputData, 
                          static_cast<OutputComponentType>
                          (*(inputData+1)));
      OutputConvertTraits
        ::SetNthComponent(2, *outputData, 
                          static_cast<OutputComponentType>
                          ((*inputData+2)));
      inputData += 3;
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
    OutputConvertTraits
      ::SetNthComponent(0, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData));
    OutputConvertTraits
      ::SetNthComponent(1, *outputData, 
                        static_cast<OutputComponentType>
                        (*(inputData+1)));
    OutputConvertTraits
      ::SetNthComponent(2, *outputData, 
                        static_cast<OutputComponentType>
                        (*(inputData+2)));
    OutputConvertTraits
      ::SetNthComponent(3, *outputData, 
                        static_cast<OutputComponentType>
                        (1));
    inputData += 3;
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
    OutputConvertTraits
      ::SetNthComponent(0, *outputData, 
                        static_cast<OutputComponentType>
                        (*inputData));
    OutputConvertTraits
      ::SetNthComponent(1, *outputData, 
                        static_cast<OutputComponentType>
                        (*(inputData+1)));
    OutputConvertTraits
      ::SetNthComponent(2, *outputData, 
                        static_cast<OutputComponentType>
                        (*(inputData+2)));
    OutputConvertTraits
      ::SetNthComponent(3, *outputData, 
                        static_cast<OutputComponentType>
                        (*(inputData+3)));
    inputData += 4;
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
        static_cast<OutputComponentType>(*inputData);
      OutputComponentType alpha = 
        static_cast<OutputComponentType>(*(inputData+1));
      inputData += 2;
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
                          (*inputData));
      OutputConvertTraits
        ::SetNthComponent(1, *outputData, 
                          static_cast<OutputComponentType>
                          (*(inputData+1)));
      OutputConvertTraits
        ::SetNthComponent(2, *outputData, 
                          static_cast<OutputComponentType>
                          (*(inputData+2)));
      OutputConvertTraits
        ::SetNthComponent(3, *outputData, 
                          static_cast<OutputComponentType>
                          (*(inputData+3)));
      inputData += 4;
      inputData += diff;
      outputData++;
      }
    }
}

 

}// end namespace itk

#endif
