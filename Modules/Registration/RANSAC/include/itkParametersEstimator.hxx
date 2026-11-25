/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkParametersEstimator_hxx
#define itkParametersEstimator_hxx

#include "itkParametersEstimator.h"

namespace itk
{

template <typename T, typename SType>
void
ParametersEstimator<T, SType>::SetMinimalForEstimate(unsigned int inputMinForEstimate)
{
  if (inputMinForEstimate == 0)
    throw ExceptionObject(__FILE__, __LINE__, "Invalid minimal number of objects for exact estimate.");

  this->minForEstimate = inputMinForEstimate;
}


template <typename T, typename SType>
unsigned int
ParametersEstimator<T, SType>::GetMinimalForEstimate()
{
  return this->minForEstimate;
}


} // end namespace itk

#endif //_PARAMETERS_ESTIMATOR_HXX_
