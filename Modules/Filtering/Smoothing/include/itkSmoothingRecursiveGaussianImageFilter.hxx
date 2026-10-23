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
#ifndef itkSmoothingRecursiveGaussianImageFilter_hxx
#define itkSmoothingRecursiveGaussianImageFilter_hxx

#include "itkProgressAccumulator.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
SmoothingRecursiveGaussianImageFilter<TInputImage, TOutputImage>::SmoothingRecursiveGaussianImageFilter()
  : m_FirstSmoothingFilter(FirstGaussianFilterType::New())
{
  // The highest eligible dimension benefits least from in-place execution, so process it first.
  m_FirstSmoothingFilter->SetOrder(GaussianOrderEnum::ZeroOrder);
  m_FirstSmoothingFilter->SetDirection(ImageDimension - 1);
  m_FirstSmoothingFilter->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
  m_FirstSmoothingFilter->ReleaseDataFlagOn();
  // InPlace will be set conditionally in the GenerateData method.

  for (unsigned int i = 0; i < ImageDimension - 1; ++i)
  {
    m_SmoothingFilters[i] = InternalGaussianFilterType::New();
    m_SmoothingFilters[i]->SetOrder(GaussianOrderEnum::ZeroOrder);
    m_SmoothingFilters[i]->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
    m_SmoothingFilters[i]->SetDirection(i);
    m_SmoothingFilters[i]->ReleaseDataFlagOn();
    m_SmoothingFilters[i]->InPlaceOn();
  }

  m_CastingFilter = CastingFilterType::New();
  m_CastingFilter->InPlaceOn();

  this->InPlaceOff();

  // NB: We must call SetSigma in order to initialize the smoothing
  // filters with the default scale.  However, m_Sigma must first be
  // initialized (it is used inside SetSigma), and it must be different
  // from 1.0 or the call will be ignored.
  this->m_Sigma.Fill(0.0);
  this->SetSigma(1.0);
}


template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveGaussianImageFilter<TInputImage, TOutputImage>::SetNumberOfWorkUnits(ThreadIdType nb)
{
  Superclass::SetNumberOfWorkUnits(nb);
  for (const auto & filter : m_SmoothingFilters)
  {
    filter->SetNumberOfWorkUnits(nb);
  }
  m_FirstSmoothingFilter->SetNumberOfWorkUnits(nb);
}


template <typename TInputImage, typename TOutputImage>
bool
SmoothingRecursiveGaussianImageFilter<TInputImage, TOutputImage>::CanRunInPlace() const
{
  // Note: There are two different ways this filter may try to run
  // in-place:
  // 1) Similar to the standard way, when the input and output image
  // are of the same type, they can share the bulk data. The output
  // will be grafted onto the last filter. In this fashion the input
  // and output will be the same bulk data, but the intermediate
  // mini-pipeline will use different data.
  // 2) If the input image is the same type as the RealImage used for
  // the mini-pipeline, then all the filters may re-use the same
  // bulk data, stealing it from the input then moving it down the
  // pipeline filter by filter. Additionally, if the output is also
  // RealType then the last filter will run in-place making the entire
  // pipeline in-place and only utilizing on copy of the bulk data.

  return m_FirstSmoothingFilter->CanRunInPlace() || this->Superclass::CanRunInPlace();
}


template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveGaussianImageFilter<TInputImage, TOutputImage>::SetSigma(ScalarRealType sigma)
{
  const SigmaArrayType sigmas(sigma);

  this->SetSigmaArray(sigmas);
}


template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveGaussianImageFilter<TInputImage, TOutputImage>::SetSigmaArray(const SigmaArrayType & sigma)
{
  if (this->m_Sigma != sigma)
  {
    this->m_Sigma = sigma;
    for (unsigned int i = 0; i < ImageDimension - 1; ++i)
    {
      m_SmoothingFilters[i]->SetSigma(m_Sigma[i]);
    }
    m_FirstSmoothingFilter->SetSigma(m_Sigma[ImageDimension - 1]);

    this->Modified();
  }
}


template <typename TInputImage, typename TOutputImage>
auto
SmoothingRecursiveGaussianImageFilter<TInputImage, TOutputImage>::GetSigmaArray() const -> SigmaArrayType
{
  return m_Sigma;
}


template <typename TInputImage, typename TOutputImage>
auto
SmoothingRecursiveGaussianImageFilter<TInputImage, TOutputImage>::GetSigma() const -> ScalarRealType
{
  return m_Sigma[0];
}


template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveGaussianImageFilter<TInputImage, TOutputImage>::SetNormalizeAcrossScale(bool normalize)
{
  m_NormalizeAcrossScale = normalize;

  for (const auto & filter : m_SmoothingFilters)
  {
    filter->SetNormalizeAcrossScale(normalize);
  }
  m_FirstSmoothingFilter->SetNormalizeAcrossScale(normalize);

  this->Modified();
}

template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveGaussianImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method. This should
  // copy the output requested region to the input requested region.
  Superclass::GenerateInputRequestedRegion();

  // This filter needs all of the input
  const typename SmoothingRecursiveGaussianImageFilter<TInputImage, TOutputImage>::InputImagePointer image =
    const_cast<InputImageType *>(this->GetInput());
  if (image)
  {
    image->SetRequestedRegion(this->GetInput()->GetLargestPossibleRegion());
  }
}

template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveGaussianImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject * output)
{
  auto * out = dynamic_cast<TOutputImage *>(output);

  if (out)
  {
    out->SetRequestedRegion(out->GetLargestPossibleRegion());
  }
}


template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveGaussianImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  itkDebugMacro("SmoothingRecursiveGaussianImageFilter generating data ");

  const typename TInputImage::ConstPointer inputImage(this->GetInput());

  const auto   size = inputImage->GetLargestPossibleRegion().GetSize();
  unsigned int numberOfActiveDimensions = 0;
  unsigned int firstDirection = 0;
  for (unsigned int d = 0; d < ImageDimension; ++d)
  {
    if (m_Sigma[d] <= 0.0)
    {
      itkExceptionStringMacro("Sigma must be greater than zero.");
    }
    if (size[d] == 0)
    {
      itkExceptionMacro("The number of pixels along dimension " << d << " is zero.");
    }
    if (size[d] >= 4)
    {
      firstDirection = d;
      ++numberOfActiveDimensions;
    }
  }

  auto progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  m_FirstSmoothingFilter->SetInput(inputImage);

  if (numberOfActiveDimensions == 0)
  {
    m_CastingFilter->SetInput(nullptr);
    m_CastingFilter->GetOutput()->ReleaseData();
    auto cast = CastImageFilter<InputImageType, OutputImageType>::New();
    cast->SetInput(inputImage);
    cast->SetInPlace(this->GetInPlace());
    cast->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
    progress->RegisterInternalFilter(cast, 1.0);
    cast->GraftOutput(this->GetOutput());
    cast->Update();
    this->GraftOutput(cast->GetOutput());
    return;
  }

  m_FirstSmoothingFilter->SetDirection(firstDirection);
  m_FirstSmoothingFilter->SetSigma(m_Sigma[firstDirection]);
  progress->RegisterInternalFilter(m_FirstSmoothingFilter, 1.0 / numberOfActiveDimensions);

  typename RealImageType::Pointer previousOutput = m_FirstSmoothingFilter->GetOutput();
  for (unsigned int d = 0; d < ImageDimension - 1; ++d)
  {
    if (d != firstDirection && size[d] >= 4)
    {
      auto & filter = m_SmoothingFilters[d];
      filter->SetSigma(m_Sigma[d]);
      filter->SetInput(previousOutput);
      progress->RegisterInternalFilter(filter, 1.0 / numberOfActiveDimensions);
      previousOutput = filter->GetOutput();
    }
    else
    {
      m_SmoothingFilters[d]->SetInput(nullptr);
    }
  }
  m_CastingFilter->SetInput(previousOutput);

  // If this filter is running in-place, then set the first smoothing
  // filter to steal the bulk data, by running in-place.
  if (this->CanRunInPlace() && this->GetInPlace())
  {
    m_FirstSmoothingFilter->InPlaceOn();

    // To make this filter's input and out share the same data, call
    // the InPlace's/Superclass's allocate methods, which takes care
    // of the needed bulk data sharing.
    this->AllocateOutputs();
  }
  else
  {
    m_FirstSmoothingFilter->InPlaceOff();
  }

  // If the last filter is running in-place then this bulk data is not
  // needed, release it to save memory.
  if (m_CastingFilter->CanRunInPlace())
  {
    this->GetOutput()->ReleaseData();
  }

  // Graft our output to the internal filter to force the proper regions
  // to be generated, and the bulk data which be be from the input due
  // to the in-place option.
  m_CastingFilter->GraftOutput(this->GetOutput());
  m_CastingFilter->Update();
  this->GraftOutput(m_CastingFilter->GetOutput());
}


template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveGaussianImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  for (unsigned int i = 0; i < ImageDimension - 1; ++i)
  {
    itkPrintSelfObjectMacro(SmoothingFilters[i]);
  }
  itkPrintSelfObjectMacro(FirstSmoothingFilter);
  itkPrintSelfObjectMacro(CastingFilter);

  os << indent << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
  os << indent << "Sigma: " << m_Sigma << std::endl;
}

} // end namespace itk

#endif
