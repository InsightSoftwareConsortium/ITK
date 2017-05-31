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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"
#include "itkFFTPadPositiveIndexImageFilter.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkPeriodicBoundaryCondition.h"
#include "itkConstantBoundaryCondition.h"
#include "itkTestingMacros.h"

int
itkFFTPadPositiveIndexImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Usage: " << argv[0] << " inputImageFile"
              << " outputImageFile"
              << " sizeGreatestPrimeFactor"
              << " boundarycondition (ZEROFLUXNEUMANN; CONSTANT; PERIODIC)" << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                       Dimension = 2;
  typedef unsigned char                    PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;

  // Readers, writers and filter typdefs
  typedef itk::ImageFileReader<ImageType>                ReaderType;
  typedef itk::ImageFileWriter<ImageType>                WriterType;
  typedef itk::FFTPadPositiveIndexImageFilter<ImageType> FFTPadType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Create the filter
  FFTPadType::Pointer fftpad = FFTPadType::New();

  EXERCISE_BASIC_OBJECT_METHODS(fftpad, FFTPadPositiveIndexImageFilter, ImageToImageFilter);


  FFTPadType::SizeType::SizeValueType sizeGreatestPrimeFactor =
    static_cast<FFTPadType::SizeType::SizeValueType>(atoi(argv[3]));
  fftpad->SetSizeGreatestPrimeFactor(sizeGreatestPrimeFactor);
  TEST_SET_GET_VALUE(sizeGreatestPrimeFactor, fftpad->GetSizeGreatestPrimeFactor());

  itk::ConstantBoundaryCondition<ImageType>        constantBoundaryCondition;
  itk::PeriodicBoundaryCondition<ImageType>        periodicBoundaryCondition;
  itk::ZeroFluxNeumannBoundaryCondition<ImageType> zeroFluxNeumannBoundaryCondition;

  std::string boundaryCondition(argv[4]);
  if (boundaryCondition == "CONSTANT")
  {
    fftpad->SetBoundaryCondition(&constantBoundaryCondition);
    TEST_SET_GET_VALUE(&constantBoundaryCondition, fftpad->GetBoundaryCondition());
  }
  else if (boundaryCondition == "PERIODIC")
  {
    fftpad->SetBoundaryCondition(&periodicBoundaryCondition);
    TEST_SET_GET_VALUE(&periodicBoundaryCondition, fftpad->GetBoundaryCondition());
  }
  else if (boundaryCondition == "ZEROFLUXNEUMANN")
  {
    fftpad->SetBoundaryCondition(&zeroFluxNeumannBoundaryCondition);
    TEST_SET_GET_VALUE(&zeroFluxNeumannBoundaryCondition, fftpad->GetBoundaryCondition());
  }
  else
  {
    std::cerr << "Invalid BoundaryCondition '" << boundaryCondition << "'" << std::endl;
    std::cerr << "Valid values are CONSTANT, PERIODIC or ZEROFLUXNEUMANN." << std::endl;
    return EXIT_FAILURE;
  }


  fftpad->SetInput(reader->GetOutput());

  TRY_EXPECT_NO_EXCEPTION(fftpad->Update());

  ImageType::IndexType outIndex = fftpad->GetOutput()->GetLargestPossibleRegion().GetIndex();
  ImageType::SizeType  outSize = fftpad->GetOutput()->GetLargestPossibleRegion().GetSize();
  std::cout << "Index: " << outIndex << " Size: " << outSize << std::endl;
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    if (outIndex[i] < 0)
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Negative index found." << std::endl;
      return EXIT_FAILURE;
    }
  }

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(fftpad->GetOutput());
  writer->SetFileName(argv[2]);

  TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
