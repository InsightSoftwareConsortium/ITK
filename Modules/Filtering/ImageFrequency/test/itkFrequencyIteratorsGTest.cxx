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

#include "gtest/gtest.h"

#include "itkCastImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkInverseFFTImageFilter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkFFTShiftImageFilter.h"
#include "itkFrequencyBandImageFilter.h" // Simplest of frequency filters for testing
#include "itkFrequencyFFTLayoutImageRegionConstIteratorWithIndex.h"
#include "itkFrequencyShiftedFFTLayoutImageRegionIteratorWithIndex.h"
#include "itkFrequencyHalfHermitianFFTLayoutImageRegionIteratorWithIndex.h"
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkTestingMacros.h"

template <typename TOutputImageType>
static typename TOutputImageType::Pointer
CreateImage(unsigned int size)
{
  using ImageType = itk::Image<char, TOutputImageType::ImageDimension>;
  typename ImageType::SizeType imageSize;
  imageSize.Fill(size);
  using RandomImageSourceType = itk::RandomImageSource<ImageType>;
  auto randomImageSource = RandomImageSourceType::New();
  randomImageSource->SetNumberOfWorkUnits(1); // to produce reproducible results
  randomImageSource->SetSize(imageSize);
  randomImageSource->Update();

  using CastFilterType = itk::CastImageFilter<ImageType, TOutputImageType>;
  auto castFilter = CastFilterType::New();
  castFilter->SetInput(randomImageSource->GetOutput());
  castFilter->Update();

  return castFilter->GetOutput();
}

template <typename ImageType>
bool
compareImages(ImageType * imageToTest, ImageType * knownImage, double differenceThreshold = 0.0)
{
  using DifferenceFilterType = itk::Testing::ComparisonImageFilter<ImageType, ImageType>;
  auto differenceFilter = DifferenceFilterType::New();
  differenceFilter->SetToleranceRadius(0);
  differenceFilter->SetDifferenceThreshold(differenceThreshold);
  differenceFilter->SetValidInput(knownImage);
  differenceFilter->SetTestInput(imageToTest);
  differenceFilter->Update();

  unsigned int numberOfDiffPixels = differenceFilter->GetNumberOfPixelsWithDifferences();
  if (numberOfDiffPixels > 0)
  {
    std::cerr << "Unequal images, with " << numberOfDiffPixels << " unequal pixels" << std::endl;
    return false;
  }
  else
  {
    return true;
  }
}

template <typename ImageType, typename ForwardFFTType, typename InverseFFTType, typename FrequencyIteratorType>
typename ImageType::Pointer
applyBandFilter(typename ImageType::Pointer image, bool shift = false)
{
  auto forwardFFT = ForwardFFTType::New();
  forwardFFT->SetInput(image);
  forwardFFT->Update();

  using ComplexImageType = typename ForwardFFTType::OutputImageType;
  typename ComplexImageType::Pointer forwardHandler = forwardFFT->GetOutput();

  if (shift)
  {
    using ShiftFilterType = itk::FFTShiftImageFilter<ComplexImageType, ComplexImageType>;
    auto shiftFilter = ShiftFilterType::New();
    shiftFilter->SetInput(forwardFFT->GetOutput());
    shiftFilter->Update();
    forwardHandler = shiftFilter->GetOutput();
  }

  using BandFilterType = itk::FrequencyBandImageFilter<ComplexImageType, FrequencyIteratorType>;
  auto bandFilter = BandFilterType::New();
  bandFilter->SetInput(forwardHandler);
  bandFilter->SetFrequencyThresholds(0.0, 0.25);
  bandFilter->Update();

  typename ComplexImageType::Pointer inverseHandler = bandFilter->GetOutput();

  if (shift)
  {
    using ShiftFilterType = itk::FFTShiftImageFilter<ComplexImageType, ComplexImageType>;
    auto shiftFilter = ShiftFilterType::New();
    shiftFilter->SetInput(inverseHandler);
    shiftFilter->SetInverse(true);
    shiftFilter->Update();
    inverseHandler = shiftFilter->GetOutput();
  }

  auto inverseFFT = InverseFFTType::New();
  inverseFFT->SetInput(inverseHandler);
  inverseFFT->Update();
  return inverseFFT->GetOutput();
}


/* Overload to handle the hermitian case */
template <typename ImageType>
typename ImageType::Pointer
applyBandFilterHermitian(typename ImageType::Pointer image)
{
  using ForwardFFTType = itk::RealToHalfHermitianForwardFFTImageFilter<ImageType>;
  using ComplexImageType = typename ForwardFFTType::OutputImageType;
  using InverseFFTType = itk::HalfHermitianToRealInverseFFTImageFilter<ComplexImageType, ImageType>;
  using FrequencyIteratorType = itk::FrequencyHalfHermitianFFTLayoutImageRegionIteratorWithIndex<ComplexImageType>;
  auto forwardFFT = ForwardFFTType::New();
  forwardFFT->SetInput(image);
  forwardFFT->Update();

  using ComplexImageType = typename ForwardFFTType::OutputImageType;
  typename ComplexImageType::Pointer forwardHandler = forwardFFT->GetOutput();

  using BandFilterType = itk::FrequencyBandImageFilter<ComplexImageType, FrequencyIteratorType>;
  auto bandFilter = BandFilterType::New();
  bandFilter->SetInput(forwardHandler);
  bandFilter->SetFrequencyThresholds(0.0, 0.25);
  bandFilter->SetActualXDimensionIsOdd(forwardFFT->GetActualXDimensionIsOdd());
  bandFilter->Update();

  typename ComplexImageType::Pointer inverseHandler = bandFilter->GetOutput();

  auto inverseFFT = InverseFFTType::New();
  inverseFFT->SetInput(inverseHandler);
  inverseFFT->SetActualXDimensionIsOdd(forwardFFT->GetActualXDimensionIsOdd());
  inverseFFT->Update();
  return inverseFFT->GetOutput();
}

// Require float/double PixelType
template <typename TImageType>
void
compareAllTypesOfIterators(typename TImageType::Pointer image, double differenceHermitianThreshold = 0.0001)
{
  using ImageType = TImageType;
  // Full Forward FFT
  using ForwardFFTFilterType = itk::ForwardFFTImageFilter<ImageType>;
  using ComplexImageType = typename ForwardFFTFilterType::OutputImageType;
  using InverseFFTFilterType = itk::InverseFFTImageFilter<ComplexImageType, ImageType>;
  using FrequencyIteratorType = itk::FrequencyFFTLayoutImageRegionIteratorWithIndex<ComplexImageType>;

  auto filteredImage =
    applyBandFilter<ImageType, ForwardFFTFilterType, InverseFFTFilterType, FrequencyIteratorType>(image);

  // Shifted Full Forward FFT
  using FrequencyShiftedIteratorType = itk::FrequencyShiftedFFTLayoutImageRegionIteratorWithIndex<ComplexImageType>;

  bool shifted = true;
  auto filteredShiftedImage =
    applyBandFilter<ImageType, ForwardFFTFilterType, InverseFFTFilterType, FrequencyShiftedIteratorType>(image,
                                                                                                         shifted);

  // Compare Full and Shifted
  bool fullAndShiftedAreEqual = compareImages<ImageType>(filteredShiftedImage, filteredImage);
  EXPECT_TRUE(fullAndShiftedAreEqual);

  // Hermitian overload
  auto filteredHermitianImage = applyBandFilterHermitian<ImageType>(image);

  // Compare full and hermitian
  // Hermitian saves memory, but at the cost of less precision
  // in the reconstruction (forward + inverse == original_image)
  // This is the minimun threshold for the comparison between
  // original and reconstructed image to be equal (without any extra band filter).
  bool fullAndHermitian = compareImages<ImageType>(filteredHermitianImage, filteredImage, differenceHermitianThreshold);
  EXPECT_TRUE(fullAndHermitian);
}

TEST(FrequencyIterators, Even3D)
{
  constexpr unsigned int ImageDimension = 3;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  auto   image = CreateImage<ImageType>(16);
  double differenceHermitianThreshold = 0.00001;
  compareAllTypesOfIterators<ImageType>(image, differenceHermitianThreshold);
}

TEST(FrequencyIterators, Even2D)
{
  constexpr unsigned int ImageDimension = 2;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  auto   image = CreateImage<ImageType>(16);
  double differenceHermitianThreshold = 0.00001;
  compareAllTypesOfIterators<ImageType>(image, differenceHermitianThreshold);
}

TEST(FrequencyIterators, Odd3D)
{
  constexpr unsigned int ImageDimension = 3;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  auto   image = CreateImage<ImageType>(15);
  double differenceHermitianThreshold = 0.00001;
  compareAllTypesOfIterators<ImageType>(image, differenceHermitianThreshold);
}

TEST(FrequencyIterators, Odd2D)
{
  constexpr unsigned int ImageDimension = 2;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  auto   image = CreateImage<ImageType>(27);
  double differenceHermitianThreshold = 0.0001;
  compareAllTypesOfIterators<ImageType>(image, differenceHermitianThreshold);
}
