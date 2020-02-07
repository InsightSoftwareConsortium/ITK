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

#include <iostream>

#include "itkChangeInformationImageFilter.h"
#include "itkHalfToFullHermitianImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkTestingMacros.h"

int
itkHalfToFullHermitianImageFilterTest(int argc, char * argv[])
{
  // Print usage information.
  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " <test image size x> <test image size y>"
              << std::endl;
    return EXIT_FAILURE;
  }

  // Read in image.
  using ImageType = itk::Image<float, 2>;
  using ComplexImageType = itk::Image<std::complex<float>, 2>;
  using RandomSourceType = itk::RandomImageSource<ImageType>;

  RandomSourceType::Pointer  source = RandomSourceType::New();
  RandomSourceType::SizeType size;
  size[0] = std::stoi(argv[1]);
  size[1] = std::stoi(argv[2]);
  source->SetMin(0.0f);
  source->SetMax(1.0f);
  source->SetSize(size);
  source->Update();

  // Change the index of the image's largest possible region to test
  // generality of the filters.
  using ChangeFilterType = itk::ChangeInformationImageFilter<ImageType>;
  ChangeFilterType::Pointer changer = ChangeFilterType::New();
  changer->ChangeRegionOn();
  ChangeFilterType::OutputImageOffsetValueType indexShift[2];
  indexShift[0] = -3;
  indexShift[1] = 5;
  changer->SetOutputOffset(indexShift);
  changer->SetInput(source->GetOutput());

  // Compute frequency image, yielding the non-redundant half of the
  // full complex image.
  using FFTFilter = itk::RealToHalfHermitianForwardFFTImageFilter<ImageType, ComplexImageType>;
  FFTFilter::Pointer fft = FFTFilter::New();
  fft->SetInput(changer->GetOutput());

  // Expand the non-redundant half to the full complex image.
  using HalfToFullFilterType = itk::HalfToFullHermitianImageFilter<ComplexImageType>;
  HalfToFullFilterType::Pointer halfToFullFilter = HalfToFullFilterType::New();
  halfToFullFilter->SetActualXDimensionIsOdd(fft->GetActualXDimensionIsOddOutput());
  halfToFullFilter->SetInput(fft->GetOutput());
  halfToFullFilter->Update();
  halfToFullFilter->Print(std::cout);

  ComplexImageType::SizeType fftSize = fft->GetOutput()->GetLargestPossibleRegion().GetSize();

  // Test that the output is the expected size.
  ComplexImageType::RegionType halfToFullOutputRegion = halfToFullFilter->GetOutput()->GetLargestPossibleRegion();
  ComplexImageType::SizeType   halfToFullOutputSize = halfToFullOutputRegion.GetSize();
  if (halfToFullOutputSize != size)
  {
    std::cerr << "HalfToFullHermitianImageFilter did not produce an image of the expected size. " << std::endl;
    std::cerr << "Expected size " << size << ", output size is " << halfToFullOutputSize << std::endl;
    return EXIT_FAILURE;
  }

  // Test that the full image has the Hermitian property.
  ComplexImageType::IndexType conjugateRegionIndex;
  conjugateRegionIndex.Fill(0);
  conjugateRegionIndex[0] = static_cast<ComplexImageType::IndexValueType>(fftSize[0]) + indexShift[0];
  conjugateRegionIndex[1] = indexShift[1];
  ComplexImageType::SizeType conjugateRegionSize(size);
  conjugateRegionSize[0] -= fftSize[0];
  itk::ImageRegion<ComplexImageType::ImageDimension> conjugateRegion(conjugateRegionIndex, conjugateRegionSize);

  using ComplexIteratorType = itk::ImageRegionConstIteratorWithIndex<ComplexImageType>;
  ComplexIteratorType it(halfToFullFilter->GetOutput(), conjugateRegion);
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    ComplexImageType::IndexType conjugateIndex = it.GetIndex();

    // Flip the indices about the center of the image.
    ComplexImageType::IndexType index(conjugateIndex);
    for (unsigned int i = 0; i < ComplexImageType::ImageDimension; ++i)
    {
      if (conjugateIndex[i] != indexShift[i])
      {
        index[i] = size[i] - conjugateIndex[i] + 2 * indexShift[i];
      }
    }
    if (it.Get() != std::conj(halfToFullFilter->GetOutput()->GetPixel(index)))
    {
      std::cerr << std::endl
                << "Mismatch found in conjugate index " << conjugateIndex << " (original index " << index
                << "). Expected " << std::conj(halfToFullFilter->GetOutput()->GetPixel(index)) << ", got " << it.Get()
                << "." << std::endl;
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
