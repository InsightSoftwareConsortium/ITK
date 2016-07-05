/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkConvertVariableLengthVectorPixelBuffer_h
#define itkConvertVariableLengthVectorPixelBuffer_h

#include "itkConvertPixelBuffer.h"
#include "itkVariableLengthVector.h"

namespace itk
{
/**
 * \class ConvertPixelBuffer
 *  \brief Class to convert blocks of data from one type to another.
 *
 * Derived from ConvertPixelBuffer has a static method Convert().  It is used
 * to work with pixel type as VariableLengthVector type.
 * \ingroup ITKIOMeshBase
 */
template <typename InputPixelType, typename T, typename OutputConvertTraits>
class ConvertPixelBuffer<InputPixelType, VariableLengthVector<T>, OutputConvertTraits>
{
public:
  /** Determine the output data type. */
  typedef typename OutputConvertTraits::ComponentType OutputComponentType;

  /** General method converts from one type to another. */
  static void Convert(InputPixelType* inputData,
                      int inputNumberOfComponents,
                      VariableLengthVector<T>* outputData ,
                      size_t size);

private:
  ConvertPixelBuffer();
  ~ConvertPixelBuffer();
};
} //namespace ITK

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConvertVariableLengthVectorPixelBuffer.hxx"
#endif

#endif // itkConvertVariableLengthVectorPixelBuffer_h
