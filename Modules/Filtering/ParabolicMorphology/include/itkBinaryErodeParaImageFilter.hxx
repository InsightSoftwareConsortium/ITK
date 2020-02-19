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
#ifndef itkBinaryErodeParaImageFilter_hxx
#define itkBinaryErodeParaImageFilter_hxx

#include "itkProgressAccumulator.h"
#include "itkBinaryErodeParaImageFilter.h"
#include "itkParabolicErodeImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
BinaryErodeParaImageFilter<TInputImage, TOutputImage>::BinaryErodeParaImageFilter()
{
  this->SetNumberOfRequiredOutputs(1);
  this->SetNumberOfRequiredInputs(1);
  this->m_CircPara = CircParabolicType::New();
  this->m_CircCast = CCastType::New();

  this->m_RectPara = RectParabolicType::New();
  this->m_RectCast = RCastType::New();
  this->m_Circular = true;
  // Need to call this after filters are created
  this->SetUseImageSpacing(false);
}

template <typename TInputImage, typename TOutputImage>
void
BinaryErodeParaImageFilter<TInputImage, TOutputImage>::SetRadius(ScalarRealType radius)
{
  RadiusType s;

  s.Fill(radius);
  this->SetRadius(s);
}

template <typename TInputImage, typename TOutputImage>
void
BinaryErodeParaImageFilter<TInputImage, TOutputImage>::GenerateData(void)
{
  // Allocate the output
  this->AllocateOutputs();
  // set up the scaling before we pass control over to superclass
  if (this->m_RectPara->GetUseImageSpacing())
  {
    // radius is in mm
    RadiusType R;
    for (unsigned P = 0; P < InputImageType::ImageDimension; P++)
    {
      R[P] = 0.5 * m_Radius[P] * m_Radius[P];
      // this->SetScale(0.5*m_Radius[P] * m_Radius[P]);
    }
    m_RectPara->SetScale(R);
    m_CircPara->SetScale(R);
  }
  else
  {
    // radius is in pixels
    RadiusType R;
    // this gives us a little bit of a margin
    for (unsigned P = 0; P < InputImageType::ImageDimension; P++)
    {
      R[P] = (0.5 * m_Radius[P] * m_Radius[P] + 1);
    }
    // std::cout << "no image spacing " << m_Radius << R << std::endl;
    m_RectPara->SetScale(R);
    m_CircPara->SetScale(R);
  }

  if (m_Circular)
  {
    ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
    progress->SetMiniPipelineFilter(this);
    InputImageConstPointer inputImage;
    inputImage = this->GetInput();

    progress->RegisterInternalFilter(m_CircPara, 0.8f);
    progress->RegisterInternalFilter(m_CircCast, 0.2f);

    m_CircPara->SetInput(inputImage);
    m_CircCast->SetInput(m_CircPara->GetOutput());
    m_CircCast->SetVal(1.0);
    // m_CircCast->SetInsideValue(0);
    // m_CircCast->SetOutsideValue(1);
    // setting the correct threshold value is a little tricky - needs would
    // to produce a result matching a bresenham circle, but these
    // circles are such that the voxel centres need to be less than radius
    // m_CircCast->SetUpperThreshold(1 -
    // itk::NumericTraits<InternalRealType>::min());
    // m_CircCast->SetUpperThreshold(0.99);

    m_CircCast->GraftOutput(this->GetOutput());
    m_CircCast->Update();
    this->GraftOutput(m_CircCast->GetOutput());
  }
  else
  {
    ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
    progress->SetMiniPipelineFilter(this);
    InputImageConstPointer inputImage;
    inputImage = this->GetInput();

    progress->RegisterInternalFilter(m_RectPara, 0.8f);
    progress->RegisterInternalFilter(m_RectCast, 0.2f);

    m_RectPara->SetInput(inputImage);
    m_RectCast->SetInput(m_RectPara->GetOutput());
    m_RectCast->SetVal(1);
    m_RectCast->GraftOutput(this->GetOutput());
    m_RectCast->Update();
    this->GraftOutput(m_RectCast->GetOutput());
  }
}

template <typename TInputImage, typename TOutputImage>
void
BinaryErodeParaImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  if (this->m_CircPara->GetUseImageSpacing())
  {
    os << "Radius in world units: " << this->GetRadius() << std::endl;
  }
  else
  {
    os << "Radius in voxels: " << this->GetRadius() << std::endl;
  }
}

template <typename TInputImage, typename TOutputImage>
void
BinaryErodeParaImageFilter<TInputImage, TOutputImage>::Modified() const
{
  Superclass::Modified();
  m_CircPara->Modified();
  m_CircCast->Modified();
  m_RectPara->Modified();
  m_RectCast->Modified();
}
} // namespace itk
#endif
