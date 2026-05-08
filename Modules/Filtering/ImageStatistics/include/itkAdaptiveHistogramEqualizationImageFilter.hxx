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
#ifndef itkAdaptiveHistogramEqualizationImageFilter_hxx
#define itkAdaptiveHistogramEqualizationImageFilter_hxx

#include <map>
#include <set>
#include "itkMath.h"

#include "itkImageRegionIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkProgressReporter.h"
#include "itkMinimumMaximumImageFilter.h"
#include "itkPrintHelper.h"

namespace itk
{

template <typename TImageType, typename TKernel>
void
AdaptiveHistogramEqualizationImageFilter<TImageType, TKernel>::BeforeThreadedGenerateData()
{
  auto input = ImageType::New();
  input->Graft(const_cast<ImageType *>(this->GetInput()));

  // Calculate min and max gray level of an input image
  // NOTE: This computation of min/max means that this filter should
  // not be able to stream.
  using MinMaxFilter = MinimumMaximumImageFilter<ImageType>;
  auto minmax = MinMaxFilter::New();

  minmax->SetInput(input);
  minmax->Update();

  m_InputMinimum = minmax->GetMinimum();
  m_InputMaximum = minmax->GetMaximum();
}

template <typename TImageType, typename TKernel>
void
AdaptiveHistogramEqualizationImageFilter<TImageType, TKernel>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Alpha: " << m_Alpha << std::endl;
  os << indent << "Beta: " << m_Beta << std::endl;

  print_helper::PrintNumericTrait(os, indent, "InputMinimum", m_InputMinimum);
  print_helper::PrintNumericTrait(os, indent, "InputMaximum", m_InputMaximum);

  itkPrintSelfBooleanMacro(UseLookupTable);
}
} // namespace itk

#endif
