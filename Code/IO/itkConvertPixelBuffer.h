/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConvertPixelBuffer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConvertPixelBuffer_h
#define __itkConvertPixelBuffer_h

#include "itkObject.h"

namespace itk
{
/** \brief Class to convert blocks of data from one type to another.
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
  /** Determine the output data type. */
  typedef typename OutputConvertTraits::ComponentType OutputComponentType;

  /** General method converts from one type to another. */
  static void Convert(InputPixelType* inputData, 
                      int inputNumberOfComponents, 
                      OutputPixelType* outputData , int size);
protected:
  /** Convert to Gray output. */
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
  
  /** Convert to RGB output. */
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
    
  /** Convert to RGBA output. */
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
