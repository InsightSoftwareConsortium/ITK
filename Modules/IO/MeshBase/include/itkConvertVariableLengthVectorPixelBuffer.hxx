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
#ifndef itkConvertVariableLengthVectorPixelBuffer_hxx
#define itkConvertVariableLengthVectorPixelBuffer_hxx

#include "itkConvertVariableLengthVectorPixelBuffer.h"

namespace itk
{
template< typename InputPixelType, typename T, typename OutputConvertTraits >
void
ConvertPixelBuffer< InputPixelType, VariableLengthVector< T >, OutputConvertTraits >
::Convert(InputPixelType *inputData, int inputNumberOfComponents, VariableLengthVector< T > *outputData, size_t size)
{
  InputPixelType *endInput = inputData + size * static_cast< size_t >( inputNumberOfComponents );

  while ( inputData != endInput )
    {
    ( *outputData ).SetSize(inputNumberOfComponents);
    for ( int ii = 0; ii < inputNumberOfComponents; ++ii )
      {
      OutputConvertTraits::SetNthComponent( ii, *outputData, static_cast< T >( *inputData++ ) );
      }

    outputData++;
    }
}
} // end namespace itk

#endif
