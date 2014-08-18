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
#ifndef __itkDCMTKTransformIO_hxx
#define __itkDCMTKTransformIO_hxx

#include "itkDCMTKTransformIO.h"

namespace itk
{

template <typename TInternalComputationValueType>
DCMTKTransformIO<TInternalComputationValueType>::DCMTKTransformIO()
{}


template <typename TInternalComputationValueType>
DCMTKTransformIO<TInternalComputationValueType>::~DCMTKTransformIO()
{}


template <typename TInternalComputationValueType>
bool
DCMTKTransformIO<TInternalComputationValueType>::CanReadFile(const char * fileName)
{
  return false;
}


template <typename TInternalComputationValueType>
bool
DCMTKTransformIO<TInternalComputationValueType>::CanWriteFile(const char * itkNotUsed(fileName))
{
  // Write to file has not yet been implemented.
  return false;
}


template <typename TInternalComputationValueType>
void
DCMTKTransformIO<TInternalComputationValueType>::Read()
{}


template <typename TInternalComputationValueType>
void
DCMTKTransformIO<TInternalComputationValueType>::Write()
{}

} // end namespace itk

#endif
