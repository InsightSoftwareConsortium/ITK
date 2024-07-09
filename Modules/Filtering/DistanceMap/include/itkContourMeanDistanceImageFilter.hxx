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
#ifndef itkContourMeanDistanceImageFilter_hxx
#define itkContourMeanDistanceImageFilter_hxx


#include "itkImageRegionIterator.h"
#include "itkProgressAccumulator.h"
#include "itkContourDirectedMeanDistanceImageFilter.h"

namespace itk
{
template <typename TInputImage1, typename TInputImage2>
ContourMeanDistanceImageFilter<TInputImage1, TInputImage2>::ContourMeanDistanceImageFilter()
{
  // this filter requires two input images
  this->SetNumberOfRequiredInputs(2);

  m_MeanDistance = RealType{};
}

template <typename TInputImage1, typename TInputImage2>
void
ContourMeanDistanceImageFilter<TInputImage1, TInputImage2>::SetInput1(const InputImage1Type * image)
{
  this->SetInput(image);
}

template <typename TInputImage1, typename TInputImage2>
void
ContourMeanDistanceImageFilter<TInputImage1, TInputImage2>::SetInput2(const TInputImage2 * image)
{
  this->SetNthInput(1, const_cast<TInputImage2 *>(image));
}

template <typename TInputImage1, typename TInputImage2>
auto
ContourMeanDistanceImageFilter<TInputImage1, TInputImage2>::GetInput1() -> const InputImage1Type *
{
  return this->GetInput();
}


template <typename TInputImage1, typename TInputImage2>
auto
ContourMeanDistanceImageFilter<TInputImage1, TInputImage2>::GetInput2() -> const InputImage2Type *
{
  return itkDynamicCastInDebugMode<const TInputImage2 *>(this->ProcessObject::GetInput(1));
}

template <typename TInputImage1, typename TInputImage2>
void
ContourMeanDistanceImageFilter<TInputImage1, TInputImage2>::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  // this filter requires:
  // - the largest possible region of the first image
  // - the corresponding region of the second image
  if (this->GetInput1())
  {
    InputImage1Pointer image1 = const_cast<InputImage1Type *>(this->GetInput1());
    image1->SetRequestedRegionToLargestPossibleRegion();

    if (this->GetInput2())
    {
      InputImage2Pointer image2 = const_cast<InputImage2Type *>(this->GetInput2());
      image2->SetRequestedRegion(this->GetInput1()->GetRequestedRegion());
    }
  }
}

template <typename TInputImage1, typename TInputImage2>
void
ContourMeanDistanceImageFilter<TInputImage1, TInputImage2>::EnlargeOutputRequestedRegion(DataObject * data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template <typename TInputImage1, typename TInputImage2>
void
ContourMeanDistanceImageFilter<TInputImage1, TInputImage2>::GenerateData()
{
  // Pass the first input through as the output
  InputImage1Pointer image = const_cast<TInputImage1 *>(this->GetInput1());

  this->GraftOutput(image);

  // Create a process accumulator for tracking the progress of this minipipeline
  auto progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  using Filter12Type = ContourDirectedMeanDistanceImageFilter<InputImage1Type, InputImage2Type>;

  auto filter12 = Filter12Type::New();

  filter12->SetInput1(this->GetInput1());
  filter12->SetInput2(this->GetInput2());

  filter12->SetUseImageSpacing(m_UseImageSpacing);

  using Filter21Type = ContourDirectedMeanDistanceImageFilter<InputImage2Type, InputImage1Type>;

  auto filter21 = Filter21Type::New();

  filter21->SetInput1(this->GetInput2());
  filter21->SetInput2(this->GetInput1());

  filter21->SetUseImageSpacing(m_UseImageSpacing);

  // Register the filter with the with progress accumulator using
  // equal weight proportion
  progress->RegisterInternalFilter(filter12, .5f);
  progress->RegisterInternalFilter(filter21, .5f);

  filter12->Update();
  RealType distance12 = filter12->GetContourDirectedMeanDistance();
  filter21->Update();
  RealType distance21 = filter21->GetContourDirectedMeanDistance();

  if (distance12 > distance21)
  {
    m_MeanDistance = distance12;
  }
  else
  {
    m_MeanDistance = distance21;
  }
}

template <typename TInputImage1, typename TInputImage2>
void
ContourMeanDistanceImageFilter<TInputImage1, TInputImage2>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfBooleanMacro(UseImageSpacing);
  os << indent << "MeanDistance: " << m_MeanDistance << std::endl;
}
} // end namespace itk
#endif
