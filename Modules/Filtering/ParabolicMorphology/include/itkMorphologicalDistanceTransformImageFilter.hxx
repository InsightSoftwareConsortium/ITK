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
#ifndef itkMorphologicalDistanceTransformImageFilter_hxx
#define itkMorphologicalDistanceTransformImageFilter_hxx

#include "itkMorphologicalDistanceTransformImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
MorphologicalDistanceTransformImageFilter<TInputImage, TOutputImage>::MorphologicalDistanceTransformImageFilter()
{
  this->SetNumberOfRequiredOutputs(1);
  this->SetNumberOfRequiredInputs(1);

  m_Erode = ErodeType::New();
  m_Thresh = ThreshType::New();
  m_Sqrt = SqrtType::New();
  m_OutsideValue = 0;
  m_Erode->SetScale(0.5);
  this->SetUseImageSpacing(true);
  m_SqrDist = false;
}

template <typename TInputImage, typename TOutputImage>
void
MorphologicalDistanceTransformImageFilter<TInputImage, TOutputImage>::Modified() const
{
  Superclass::Modified();
  m_Erode->Modified();
  m_Thresh->Modified();
  m_Sqrt->Modified();
}

template <typename TInputImage, typename TOutputImage>
void
MorphologicalDistanceTransformImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);
  // these values are guesses at present - need to profile to get a
  // real idea
  progress->RegisterInternalFilter(m_Thresh, 0.1f);
  progress->RegisterInternalFilter(m_Erode, 0.8f);
  progress->RegisterInternalFilter(m_Sqrt, 0.1f);

  // std::cout << "DT" << std::endl;

  double                             MaxDist = 0.0;
  typename TOutputImage::SpacingType sp = this->GetOutput()->GetSpacing();
  typename TOutputImage::SizeType    sz = this->GetOutput()->GetLargestPossibleRegion().GetSize();
  if (this->GetUseImageSpacing())
  {
    for (unsigned k = 0; k < TOutputImage::ImageDimension; k++)
    {
      double thisdim = (sz[k] * sp[k]);
      MaxDist += thisdim * thisdim;
    }
  }
  else
  {
    for (unsigned k = 0; k < TOutputImage::ImageDimension; k++)
    {
      double thisdim = sz[k];
      MaxDist += thisdim * thisdim;
    }
  }

  //   double Wt = 0.0;
  //   if (this->GetUseImageSpacing())
  //     {
  //     for (unsigned k = 0; k < TOutputImage::ImageDimension; k++)
  //       {
  //       Wt += sp[k] * sp[k];
  //       }
  //     }
  //   else
  //     {
  //     for (unsigned k = 0; k < TOutputImage::ImageDimension; k++)
  //       {
  //       Wt += 1.0;
  //       }
  //     }
  //   Wt = sqrt(Wt);
  this->AllocateOutputs();

  m_Thresh->SetLowerThreshold(m_OutsideValue);
  m_Thresh->SetUpperThreshold(m_OutsideValue);
  m_Thresh->SetOutsideValue(MaxDist);
  m_Thresh->SetInsideValue(0);

  m_Thresh->SetInput(this->GetInput());
  m_Erode->SetInput(m_Thresh->GetOutput());

  if (m_SqrDist)
  {
    m_Erode->GraftOutput(this->GetOutput());
    m_Erode->Update();
    this->GraftOutput(m_Erode->GetOutput());
  }
  else
  {
    m_Sqrt->SetInput(m_Erode->GetOutput());
    m_Sqrt->GraftOutput(this->GetOutput());
    m_Sqrt->Update();
    this->GraftOutput(m_Sqrt->GetOutput());
  }
}

template <typename TInputImage, typename TOutputImage>
void
MorphologicalDistanceTransformImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Outside Value = " << (OutputPixelType)m_OutsideValue << std::endl;
  os << "ImageScale = " << m_Erode->GetUseImageSpacing() << std::endl;
}
} // namespace itk

#endif
