/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkBSplineApproximationGradientImageFilter_hxx
#define itkBSplineApproximationGradientImageFilter_hxx

#include "itkBSplineApproximationGradientImageFilter.h"

namespace itk
{

template <typename TInputImage, typename TOutputValueType>
BSplineApproximationGradientImageFilter<TInputImage, TOutputValueType>::BSplineApproximationGradientImageFilter()
{
  this->m_ImageToPointSetFilter = ImageToPointSetFilterType::New();
  this->m_PointSetToGradientFilter = PointSetToGradientFilterType::New();
  this->m_PointSetToGradientFilter->SetInput(m_ImageToPointSetFilter->GetOutput());

  this->SetNumberOfIndexedOutputs(InputVectorDimension);
  // ImageSource only does this for the first output.
  for (unsigned int i = 1; i < InputVectorDimension; i++)
  {
    this->SetNthOutput(i, this->MakeOutput(i));
  }

  this->m_SplineOrder.Fill(3);
  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    this->m_ControlPointSpacingRatio[i] = 2.0;
  }
  this->m_NumberOfLevels.Fill(1);
}


template <typename TInputImage, typename TOutputValueType>
void
BSplineApproximationGradientImageFilter<TInputImage, TOutputValueType>::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  typename InputImageType::Pointer input = const_cast<InputImageType *>(this->GetInput());
  if (input.GetPointer() == nullptr)
  {
    return;
  }
  input->SetRequestedRegionToLargestPossibleRegion();
}


template <typename TInputImage, typename TOutputValueType>
void
BSplineApproximationGradientImageFilter<TInputImage, TOutputValueType>::EnlargeOutputRequestedRegion(DataObject * data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}


template <typename TInputImage, typename TOutputValueType>
void
BSplineApproximationGradientImageFilter<TInputImage, TOutputValueType>::GenerateData()
{
  typename InputImageType::ConstPointer input = this->GetInput();

  if (input.GetPointer() == nullptr)
  {
    return;
  }

  unsigned int i;

  m_ImageToPointSetFilter->SetInput(input);
  m_PointSetToGradientFilter->SetNumberOfLevels(m_NumberOfLevels);
  m_PointSetToGradientFilter->SetSplineOrder(m_SplineOrder);
  m_PointSetToGradientFilter->SetOrigin(input->GetOrigin());
  m_PointSetToGradientFilter->SetSpacing(input->GetSpacing());
  m_PointSetToGradientFilter->SetDirection(input->GetDirection());
  typename InputImageType::SizeType size = input->GetLargestPossibleRegion().GetSize();
  m_PointSetToGradientFilter->SetSize(size);

  ArrayType ncpts;
  for (i = 0; i < ImageDimension; ++i)
  {
    ncpts[i] = static_cast<typename ArrayType::ValueType>(vcl_floor(
      static_cast<double>(size[i]) /
      (m_ControlPointSpacingRatio[i] * static_cast<double>(std::pow(2., static_cast<int>(m_NumberOfLevels[i] - 1))))));
  }
  m_PointSetToGradientFilter->SetNumberOfControlPoints(ncpts);

  for (i = 0; i < InputVectorDimension; ++i)
  {
    m_PointSetToGradientFilter->GraftNthOutput(i, this->GetOutput(i));
  }
  m_PointSetToGradientFilter->Update();
  for (i = 0; i < InputVectorDimension; ++i)
  {
    this->GraftNthOutput(i, m_PointSetToGradientFilter->GetOutput(i));
  }
}

} // end namespace itk

#endif
