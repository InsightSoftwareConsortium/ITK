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
#ifndef itkGaussianImageSource_hxx
#define itkGaussianImageSource_hxx

#include "itkGaussianSpatialFunction.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"
#include "itkObjectFactory.h"

namespace itk
{

template <typename TOutputImage>
GaussianImageSource<TOutputImage>::GaussianImageSource()

{
  // Gaussian parameters, defined so that the Gaussian
  // is centered in the default image
  m_Mean.Fill(32.0);
  m_Sigma.Fill(16.0);
}

template <typename TOutputImage>
void
GaussianImageSource<TOutputImage>::SetParameters(const ParametersType & parameters)
{
  ArrayType sigma;
  ArrayType mean;
  for (unsigned int i = 0; i < ArrayType::Length; ++i)
  {
    sigma[i] = parameters[i];
    mean[i] = parameters[i + ArrayType::Length];
  }
  this->SetSigma(sigma);
  this->SetMean(mean);

  const double scale = parameters[2 * ArrayType::Length];
  this->SetScale(scale);
}

template <typename TOutputImage>
auto
GaussianImageSource<TOutputImage>::GetParameters() const -> ParametersType
{
  ParametersType parameters(2 * ArrayType::Length + 1);
  for (unsigned int i = 0; i < ArrayType::Length; ++i)
  {
    parameters[i] = m_Sigma[i];
    parameters[i + ArrayType::Length] = m_Mean[i];
  }
  parameters[2 * ArrayType::Length] = m_Scale;

  return parameters;
}

template <typename TOutputImage>
unsigned int
GaussianImageSource<TOutputImage>::GetNumberOfParameters() const
{
  return 2 * ArrayType::Length + 1;
}

template <typename TOutputImage>
void
GaussianImageSource<TOutputImage>::GenerateData()
{
  TOutputImage * outputPtr = this->GetOutput();

  // Allocate the output buffer
  outputPtr->SetBufferedRegion(outputPtr->GetRequestedRegion());
  outputPtr->Allocate();

  // Create and initialize a new Gaussian function
  using FunctionType = GaussianSpatialFunction<double, NDimensions>;
  auto gaussian = FunctionType::New();

  gaussian->SetSigma(m_Sigma);
  gaussian->SetMean(m_Mean);
  gaussian->SetScale(m_Scale);
  gaussian->SetNormalized(m_Normalized);

  // Create an iterator that will walk the output region
  using OutputIterator = ImageRegionIterator<TOutputImage>;

  ProgressReporter progress(this, 0, outputPtr->GetRequestedRegion().GetNumberOfPixels());
  // Walk the output image, evaluating the spatial function at each pixel
  for (OutputIterator outIt(outputPtr, outputPtr->GetRequestedRegion()); !outIt.IsAtEnd(); ++outIt)
  {
    const typename TOutputImage::IndexType index = outIt.GetIndex();
    // The position at which the function is evaluated
    typename FunctionType::InputType evalPoint;
    outputPtr->TransformIndexToPhysicalPoint(index, evalPoint);
    const double value = gaussian->Evaluate(evalPoint);

    // Set the pixel value to the function value
    outIt.Set(static_cast<typename TOutputImage::PixelType>(value));
    progress.CompletedPixel();
  }
}

template <typename TOutputImage>
void
GaussianImageSource<TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Mean: " << m_Mean << std::endl;
  os << indent << "Sigma: " << m_Sigma << std::endl;
  os << indent << "Scale: " << m_Scale << std::endl;
  itkPrintSelfBooleanMacro(Normalized);
}
} // end namespace itk

#endif
