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

#include "itkUnsharpMaskingImageFilter.h"
#include "itkTestingMacros.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


namespace
{
template<typename OutPixelType>
int RunTest(int argc, char* argv[])
{
  const unsigned int myDimension = 2;
  typedef itk::Image<unsigned char, myDimension> InImageType;
  typedef itk::ImageFileReader<InImageType>      ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[2]);
  std::cout << "Reading input file: " << argv[2] << std::endl;
  TRY_EXPECT_NO_EXCEPTION(reader->Update());

  typedef itk::Image<OutPixelType, myDimension> OutImageType;
  typedef itk::ImageFileWriter<OutImageType>    WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  TRY_EXPECT_NO_EXCEPTION(writer->SetFileName(argv[3]));

  typedef itk::UnsharpMaskingImageFilter< InImageType, OutImageType >  myFilterType;
  typename myFilterType::Pointer filter = myFilterType::New();

  //this does not work from within a templated method (GCC gives an error)
  //EXERCISE_BASIC_OBJECT_METHODS(filter, UnsharpMaskingImageFilter, ImageToImageFilter);

  filter->SetInput(reader->GetOutput());

  if (argc > 4)
    {
    filter->SetAmount(atof(argv[4]));
    }
  if (argc > 5)
    {
    filter->SetSigma(atof(argv[5]));
    }
  if (argc > 6)
    {
    filter->SetThreshold(atoi(argv[6]));
    }

  TRY_EXPECT_NO_EXCEPTION(filter->Update());
  writer->SetInput(filter->GetOutput());
  std::cout << "Writing output file: " << argv[2] << std::endl;
  TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << std::endl << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
}

int itkUnsharpMaskingImageFilterTest(int argc, char* argv[])
{
  if (argc<4)
    {
    std::cerr << "Usage:\n itkUnsharpMaskingImageFilterTest";
    std::cerr << " float | char in.png out.nrrd [amount [sigma [threshold]]]" << std::endl;
    return EXIT_FAILURE;
    }

  if (!strcmp(argv[1], "float"))
    {
    return RunTest<float>(argc, argv);
    }
  else
    {
    return RunTest<unsigned char>(argc, argv);
    }
}
