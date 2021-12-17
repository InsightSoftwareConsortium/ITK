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
#ifndef itkMorphologicalSignedDistanceTransformImageFilter_hxx
#define itkMorphologicalSignedDistanceTransformImageFilter_hxx

#include "itkProgressAccumulator.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
MorphologicalSignedDistanceTransformImageFilter<TInputImage,
                                                TOutputImage>::MorphologicalSignedDistanceTransformImageFilter()
{
  this->SetNumberOfRequiredOutputs(1);
  this->SetNumberOfRequiredInputs(1);

  m_Erode = ErodeType::New();
  m_Dilate = DilateType::New();
  m_Thresh = ThreshType::New();
  m_Helper = HelperType::New();
  m_Erode->SetScale(0.5);
  m_Dilate->SetScale(0.5);
  this->SetUseImageSpacing(true);
  this->SetInsideIsPositive(false);
  m_OutsideValue = 0;
  m_ParabolicAlgorithm = INTERSECTION;
}

template <typename TInputImage, typename TOutputImage>
void
MorphologicalSignedDistanceTransformImageFilter<TInputImage, TOutputImage>::Modified() const
{
  Superclass::Modified();
  m_Erode->Modified();
  m_Dilate->Modified();
  m_Thresh->Modified();
  m_Helper->Modified();
}

template <typename TInputImage, typename TOutputImage>
void
MorphologicalSignedDistanceTransformImageFilter<TInputImage, TOutputImage>::GenerateData(void)
{
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);
  // these values are guesses at present - need to profile to get a
  // real idea
  progress->RegisterInternalFilter(m_Thresh, 0.1f);
  progress->RegisterInternalFilter(m_Erode, 0.4f);
  progress->RegisterInternalFilter(m_Dilate, 0.4f);
  progress->RegisterInternalFilter(m_Helper, 0.1f);

  m_Erode->SetParabolicAlgorithm(m_ParabolicAlgorithm);
  m_Dilate->SetParabolicAlgorithm(m_ParabolicAlgorithm);

  this->AllocateOutputs();
  // figure out the maximum value of distance transform using the
  // image dimensions
  typename TOutputImage::SizeType    sz = this->GetOutput()->GetRequestedRegion().GetSize();
  typename TOutputImage::SpacingType sp = this->GetOutput()->GetSpacing();

  double MaxDist = 0.0;
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

  m_Thresh->SetLowerThreshold(m_OutsideValue);
  m_Thresh->SetUpperThreshold(m_OutsideValue);
  if (this->GetInsideIsPositive())
  {
    m_Thresh->SetOutsideValue(MaxDist);
    m_Thresh->SetInsideValue(-MaxDist);
  }
  else
  {
    m_Thresh->SetOutsideValue(-MaxDist);
    m_Thresh->SetInsideValue(MaxDist);
  }

  m_Thresh->SetInput(this->GetInput());
  m_Erode->SetInput(m_Thresh->GetOutput());
  m_Dilate->SetInput(m_Thresh->GetOutput());
#if 1
  m_Helper->SetInput(m_Erode->GetOutput());
  m_Helper->SetInput2(m_Dilate->GetOutput());
  m_Helper->SetInput3(m_Thresh->GetOutput());
  m_Helper->SetVal(MaxDist);
  m_Helper->GraftOutput(this->GetOutput());
  m_Helper->Update();
  this->GraftOutput(m_Helper->GetOutput());
#else
  m_Dilate->GraftOutput(this->GetOutput());
  m_Dilate->Update();
  this->GraftOutput(m_Dilate->GetOutput());
#endif
}

template <typename TInputImage, typename TOutputImage>
void
MorphologicalSignedDistanceTransformImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os,
                                                                                      Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Outside Value = " << (OutputPixelType)m_OutsideValue << std::endl;
  os << "ImageScale = " << m_Erode->GetUseImageSpacing() << std::endl;
}
} // namespace itk

#endif
