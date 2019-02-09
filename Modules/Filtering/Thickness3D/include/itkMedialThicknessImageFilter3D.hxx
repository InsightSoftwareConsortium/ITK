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
#ifndef itkMedialThicknessImageFilter3D_hxx
#define itkMedialThicknessImageFilter3D_hxx


#include "itkMedialThicknessImageFilter3D.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"


namespace itk
{

template <typename TInputImage, typename TOutputImage>
MedialThicknessImageFilter3D<TInputImage, TOutputImage>::MedialThicknessImageFilter3D()
{
  m_DistanceFilter = DistanceType::New();
  m_DistanceFilter->ReleaseDataFlagOn();
  m_SkeletonFilter = SkeletonType::New();
  m_SkeletonFilter->ReleaseDataFlagOn();
  m_MaskFilter = MaskType::New();
  m_MaskFilter->SetInput(m_DistanceFilter->GetOutput());
  m_MaskFilter->SetMaskImage(m_SkeletonFilter->GetOutput());
}

template <typename TInputImage, typename TOutputImage>
void
MedialThicknessImageFilter3D<TInputImage, TOutputImage>::GenerateData()
{
  m_DistanceFilter->SetInput(this->GetInput());
  m_SkeletonFilter->SetInput(this->GetInput());

  m_MaskFilter->GraftOutput(this->GetOutput());
  m_MaskFilter->Update();
  this->GraftOutput(m_MaskFilter->GetOutput());
}

template <typename TInputImage, typename TOutputImage>
void
MedialThicknessImageFilter3D<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif // itkMedialThicknessImageFilter3D_hxx
