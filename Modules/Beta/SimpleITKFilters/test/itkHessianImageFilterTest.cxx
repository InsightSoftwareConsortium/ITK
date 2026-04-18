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
#include "itkHessianImageFilter.h"
#include "itkGaussianImageSource.h"
#include "itkImageFileReader.h"

int
itkHessianImageFilterTest(int, char *[])
{
  constexpr unsigned int Dimension = 3;
  using ImageType = itk::Image<float, Dimension>;

  constexpr unsigned int imageSize = 64;

  ImageType::SizeType size;
  size.Fill(imageSize);

  ImageType::SpacingType spacing;
  spacing.Fill(1.0);
  spacing[0] = 1.0;

  using GaussianSourceType = itk::GaussianImageSource<ImageType>;
  GaussianSourceType::Pointer gaussianSource = GaussianSourceType::New();
  gaussianSource->SetSize(size);
  gaussianSource->SetSpacing(spacing);
  gaussianSource->SetMean(itk::FixedArray<double, Dimension>(imageSize / 2));
  gaussianSource->SetSigma(itk::FixedArray<double, Dimension>(10.0));
  gaussianSource->SetNormalized(false);
  gaussianSource->SetScale(1.0); // dark blob

  using HessianFilterType = itk::HessianImageFilter<ImageType>;
  HessianFilterType::Pointer hessian = HessianFilterType::New();
  hessian->SetInput(gaussianSource->GetOutput());
  hessian->Update();

  HessianFilterType::OutputPixelType H;

  ImageType::IndexType idx;
  idx.Fill(imageSize / 2);
  H = hessian->GetOutput()->GetPixel(idx);


  --idx[0];
  std::cout << hessian->GetOutput()->GetPixel(idx) << std::endl;

  --idx[1];
  std::cout << hessian->GetOutput()->GetPixel(idx) << std::endl;

  return 0;
}
