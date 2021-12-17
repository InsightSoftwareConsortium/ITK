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
#ifndef itkParabolicOpenCloseSafeBorderImageFilter_hxx
#define itkParabolicOpenCloseSafeBorderImageFilter_hxx

#include "itkProgressAccumulator.h"

namespace itk
{
template <typename TInputImage, bool DoOpen, typename TOutputImage>
void
ParabolicOpenCloseSafeBorderImageFilter<TInputImage, DoOpen, TOutputImage>::GenerateData()
{
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();
  InputImageConstPointer           inputImage;
  typename PadFilterType::SizeType Bounds;
  typename PadFilterType::SizeType BoundsSize;
  if (this->m_SafeBorder)
  {
    auto localInput = TInputImage::New();
    localInput->Graft(this->GetInput());

    // need to compute some image statistics and determine the padding
    // extent. This will almost certainly be an over estimate
    m_StatsFilt->SetInput(localInput);
    m_StatsFilt->Update();
    InputPixelType                       range = m_StatsFilt->GetMaximum() - m_StatsFilt->GetMinimum();
    typename MorphFilterType::RadiusType Sigma = m_MorphFilt->GetScale();
    typename TInputImage::SpacingType    spcing = localInput->GetSpacing();
    for (unsigned s = 0; s < ImageDimension; s++)
    {
      if (m_MorphFilt->GetUseImageSpacing())
      {
        RealType image_scale = spcing[s];
        Bounds[s] = (unsigned long)ceil(sqrt(2 * (Sigma[s] / (image_scale * image_scale)) * range));
        BoundsSize[s] = Bounds[s];
      }
      else
      {
        Bounds[s] = (unsigned long)ceil(sqrt(2 * Sigma[s] * range));
        BoundsSize[s] = Bounds[s];
      }
    }
    m_PadFilt->SetPadLowerBound(Bounds);
    m_PadFilt->SetPadUpperBound(Bounds);

    // need to select between opening and closing here
    if (DoOpen)
    {
      // m_PadFilt->SetConstant(NumericTraits<InputPixelType>::max());
      m_PadFilt->SetConstant(m_StatsFilt->GetMaximum());
    }
    else
    {
      // m_PadFilt->SetConstant(NumericTraits<InputPixelType>::NonpositiveMin());
      m_PadFilt->SetConstant(m_StatsFilt->GetMinimum());
    }
    m_PadFilt->SetInput(localInput);
    progress->RegisterInternalFilter(m_PadFilt, 0.1f);
    inputImage = m_PadFilt->GetOutput();
  }
  else
  {
    auto localInput = TInputImage::New();
    localInput->Graft(this->GetInput());

    inputImage = localInput;
  }

  m_MorphFilt->SetInput(inputImage);
  m_MorphFilt->SetParabolicAlgorithm(m_ParabolicAlgorithm);

  progress->RegisterInternalFilter(m_MorphFilt, 0.8f);

  if (this->m_SafeBorder)
  {
    // crop
    m_CropFilt->SetInput(m_MorphFilt->GetOutput());
    m_CropFilt->SetUpperBoundaryCropSize(BoundsSize);
    m_CropFilt->SetLowerBoundaryCropSize(BoundsSize);
    progress->RegisterInternalFilter(m_CropFilt, 0.1f);
    m_CropFilt->GraftOutput(this->GetOutput());
    m_CropFilt->Update();
    this->GraftOutput(m_CropFilt->GetOutput());
  }
  else
  {
    m_MorphFilt->GraftOutput(this->GetOutput());
    m_MorphFilt->Update();
    this->GraftOutput(m_MorphFilt->GetOutput());
    // std::cout << "Finished grafting" << std::endl;
  }
}

template <typename TInputImage, bool DoOpen, typename TOutputImage>
void
ParabolicOpenCloseSafeBorderImageFilter<TInputImage, DoOpen, TOutputImage>::Modified() const
{
  Superclass::Modified();
  m_MorphFilt->Modified();
  m_PadFilt->Modified();
  m_CropFilt->Modified();
  m_StatsFilt->Modified();
}

///////////////////////////////////
template <typename TInputImage, bool DoOpen, typename TOutputImage>
void
ParabolicOpenCloseSafeBorderImageFilter<TInputImage, DoOpen, TOutputImage>::PrintSelf(std::ostream & os,
                                                                                      Indent         indent) const
{
  os << indent << "SafeBorder: " << m_SafeBorder << std::endl;
  if (this->GetUseImageSpacing())
  {
    os << "Scale in world units: " << this->GetScale() << std::endl;
  }
  else
  {
    os << "Scale in voxels: " << this->GetScale() << std::endl;
  }
}
} // namespace itk

#endif
