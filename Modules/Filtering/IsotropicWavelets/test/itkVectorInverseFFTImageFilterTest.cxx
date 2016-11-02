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
#include <string>
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"

#include "itkVectorInverseFFTImageFilter.h"
#include "itkComposeImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkTestingComparisonImageFilter.h"

using namespace std;
using namespace itk;

int
itkVectorInverseFFTImageFilterTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage " << std::endl;
    return EXIT_FAILURE;
  }
  const string inputImage = argv[1];

  const unsigned int                       dimension = 3;
  typedef float                            PixelType;
  typedef itk::Image<PixelType, dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;
  ReaderType::Pointer                      reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();
  reader->UpdateLargestPossibleRegion();

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTForwardFilterType;
  FFTForwardFilterType::Pointer                 fftForwardFilter = FFTForwardFilterType::New();
  fftForwardFilter->SetInput(reader->GetOutput());
  fftForwardFilter->Update();
  typedef FFTForwardFilterType::OutputImageType ComplexImageType;

  // Perform inverse FFT on forwardFFT image.
  typedef itk::InverseFFTImageFilter<ComplexImageType> FFTInverseFilterType;
  FFTInverseFilterType::Pointer                        fftInverseFilter = FFTInverseFilterType::New();
  fftInverseFilter->SetInput(fftForwardFilter->GetOutput());
  fftInverseFilter->Update();

  /***** Create VectorImages *****/
  unsigned int num_components = 2;
  // Create vector from forwardFFT.
  typedef itk::ComposeImageFilter<ComplexImageType> ComposeComplexFilterType;
  ComposeComplexFilterType::Pointer                 composeComplexFilter = ComposeComplexFilterType::New();
  for (unsigned int c = 0; c < num_components; c++)
  {
    composeComplexFilter->SetInput(c, fftForwardFilter->GetOutput());
  }
  composeComplexFilter->Update();

  // Do the inverse of the composeComplexFilter using the class to test.
  typedef itk::VectorInverseFFTImageFilter<ComposeComplexFilterType::OutputImageType> VectorInverseFFTType;
  VectorInverseFFTType::Pointer vecInverseFFT = VectorInverseFFTType::New();
  vecInverseFFT->SetInput(composeComplexFilter->GetOutput());
  vecInverseFFT->Update();

  typedef itk::VectorIndexSelectionCastImageFilter<VectorInverseFFTType::OutputImageType,
                                                   FFTInverseFilterType::OutputImageType>
                                VectorCastFilterType;
  VectorCastFilterType::Pointer vectorCastFilter = VectorCastFilterType::New();
  vectorCastFilter->SetInput(vecInverseFFT->GetOutput());

  typedef itk::Testing::ComparisonImageFilter<FFTInverseFilterType::OutputImageType,
                                              FFTInverseFilterType::OutputImageType>
                                DifferenceFilterType;
  DifferenceFilterType::Pointer differenceFilter = DifferenceFilterType::New();
  differenceFilter->SetToleranceRadius(0);
  differenceFilter->SetDifferenceThreshold(0);
  for (unsigned int c = 0; c < num_components; c++)
  {
    vectorCastFilter->SetIndex(c);
    vectorCastFilter->Update();
    differenceFilter->SetValidInput(fftInverseFilter->GetOutput());
    differenceFilter->SetTestInput(vectorCastFilter->GetOutput());
    differenceFilter->Update();
    unsigned int wrong_pixels = differenceFilter->GetNumberOfPixelsWithDifferences();
    if (wrong_pixels > 0)
    {
      std::cout << "The images are not equal, wrong_pixels= " << wrong_pixels << std::endl;
      return EXIT_FAILURE;
    }
  }
  return EXIT_SUCCESS;
}
