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
#ifndef itkMyFilter_hxx
#define itkMyFilter_hxx

#include "itkMyFilter.h"

namespace itk
{

template <typename TInputPointSet, typename TOutputPointSet>
MyFilter<TInputPointSet, TOutputPointSet>::MyFilter()
{}


template <typename TInputPointSet, typename TOutputPointSet>
void
MyFilter<TInputPointSet, TOutputPointSet>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template <typename TInputPointSet, typename TOutputPointSet>
void
MyFilter<TInputPointSet, TOutputPointSet>::GenerateData()
{
  auto input = this->GetInput();
  auto output = this->GetOutput();
  auto inPts = input->GetPoints();

  itkDebugMacro(<< "Executing connectivity");

  //  Check input/allocate storage
  IdentifierType numPts = input->GetNumberOfPoints();
  if (numPts < 1)
  {
    itkDebugMacro(<< "No data to connect!");
    return;
  }
  return;
}

} // end namespace itk

#endif // itkMyFilter_hxx
