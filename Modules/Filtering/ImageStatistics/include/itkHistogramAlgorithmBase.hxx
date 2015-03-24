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
#ifndef itkHistogramAlgorithmBase_hxx
#define itkHistogramAlgorithmBase_hxx

#include "itkHistogramAlgorithmBase.h"

namespace itk
{
template< typename TInputHistogram >
HistogramAlgorithmBase< TInputHistogram >
::HistogramAlgorithmBase()
{
  m_InputHistogram = ITK_NULLPTR;
}

template< typename TInputHistogram >
void
HistogramAlgorithmBase< TInputHistogram >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(InputHistogram);

}
} // end of namespace itk

#endif
