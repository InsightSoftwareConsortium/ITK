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
#ifndef itkMedialThicknessImageFilter3D_hxx
#define itkMedialThicknessImageFilter3D_hxx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
MedialThicknessImageFilter3D<TInputImage, TOutputImage>::MedialThicknessImageFilter3D()
{
  m_DistanceFilter = DistanceImageFilterType::New();
  m_DistanceFilter->ReleaseDataFlagOn();
  m_ThinningFilter = ThinningImageFilterType::New();
  m_ThinningFilter->ReleaseDataFlagOn();
  m_MaskFilter = MaskImageFilterType::New();
  m_MaskFilter->SetInput(m_DistanceFilter->GetOutput());
  m_MaskFilter->SetMaskImage(m_ThinningFilter->GetOutput());
  m_ThinningFilter->ReleaseDataFlagOn();
  m_MultiplyFilter = MultiplyImageFilterType::New();
  m_MultiplyFilter->SetInput(m_MaskFilter->GetOutput());
  m_MultiplyFilter->SetConstant(-2.0);
}

template <typename TInputImage, typename TOutputImage>
void
MedialThicknessImageFilter3D<TInputImage, TOutputImage>::GenerateData()
{
  typename InputImageType::Pointer input = InputImageType::New();
  input->Graft(dynamic_cast<const InputImageType *>(this->GetInput()));
  m_DistanceFilter->SetInput(input);
  m_ThinningFilter->SetInput(input);
  m_MultiplyFilter->Update();
  this->GraftOutput(m_MultiplyFilter->GetOutput());
}

template <typename TInputImage, typename TOutputImage>
void
MedialThicknessImageFilter3D<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif // itkMedialThicknessImageFilter3D_hxx
