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
#ifndef itkIsotropicWaveletFrequencyFunction_hxx
#define itkIsotropicWaveletFrequencyFunction_hxx

#include "itkIsotropicWaveletFrequencyFunction.h"

namespace itk
{
template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::IsotropicWaveletFrequencyFunction()
  : m_HighPassSubBands(1)
{}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::~IsotropicWaveletFrequencyFunction()
{}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
void
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::PrintSelf(std::ostream & os,
                                                                                      Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_HighPassSubBands: " << m_HighPassSubBands << std::endl;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
void
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::SetHighPassSubBands(
  const unsigned int & high_pass_bands)
{
  if (high_pass_bands == 0)
    itkExceptionMacro("HighPassSubands must be greater than zero. At least one high band must exist.");
  this->m_HighPassSubBands = high_pass_bands;
}

} // end namespace itk

#endif
