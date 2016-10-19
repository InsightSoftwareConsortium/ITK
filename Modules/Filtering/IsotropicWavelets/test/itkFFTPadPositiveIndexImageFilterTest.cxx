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

int
itkFFTPadPositiveIndexImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile outputImageFile" << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                       Dimension = 2;
  typedef unsigned char                    PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;

  // readers/writers
  typedef itk::ImageFileReader<ImageType>                ReaderType;
  typedef itk::ImageFileWriter<ImageType>                WriterType;
  typedef itk::FFTPadPositiveIndexImageFilter<ImageType> FFTPadType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  itk::ZeroFluxNeumannBoundaryCondition<ImageType> zfnCond;
  itk::ConstantBoundaryCondition<ImageType>        zeroCond;
  itk::PeriodicBoundaryCondition<ImageType>        wrapCond;

  // Create the filters
  FFTPadType::Pointer fftpad = FFTPadType::New();
  fftpad->SetInput(reader->GetOutput());

  fftpad->Update();
  ImageType::IndexType outIndex = fftpad->GetOutput()->GetLargestPossibleRegion().GetIndex();
  ImageType::SizeType  outSize = fftpad->GetOutput()->GetLargestPossibleRegion().GetSize();
  std::cout << "Index: " << outIndex << " Size: " << outSize << std::endl;
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    if (outIndex[i] < 0)
    {
      std::cerr << "Negative index " << std::endl;
      return EXIT_FAILURE;
    }
  }
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(fftpad->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  return EXIT_SUCCESS;
}
