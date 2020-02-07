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

#include "itkMirrorPadImageFilter.h"
#include "itkTestingMacros.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


namespace
{
template <typename OutPixelType>
int
RunTest(int argc, char * argv[])
{
  constexpr unsigned int Dimension = 2;
  using InputImagePixelType = unsigned char;
  using InImageType = itk::Image<InputImagePixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<InImageType>;

  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[2]);
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  using OutImageType = itk::Image<OutPixelType, Dimension>;
  using WriterType = itk::ImageFileWriter<OutImageType>;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[3]);

  using Mirror = itk::MirrorPadImageFilter<InImageType, OutImageType>;
  typename Mirror::Pointer filter = Mirror::New();

  if (argc > 4)
  {
    double decayFactor = std::stod(argv[4]);
    filter->SetDecayBase(decayFactor);
  }

  typename OutImageType::SizeType pad;
  if (argc > 5)
  {
    pad.Fill(std::stoi(argv[5]));
    filter->SetPadLowerBound(pad);
  }
  if (argc > 6)
  {
    pad.Fill(std::stoi(argv[6]));
    filter->SetPadUpperBound(pad);
  }

  filter->SetInput(reader->GetOutput());
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());
  writer->SetInput(filter->GetOutput());
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << std::endl << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
} // namespace

int
itkMirrorPadImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage:\n itkMirrorPadImageFilterTest";
    std::cerr << " float | uchar in.png out.nrrd [decayFactor [lowerPad [upperPad]]]" << std::endl;
    return EXIT_FAILURE;
  }

  int testStatus = EXIT_SUCCESS;

  if (!strcmp(argv[1], "float"))
  {
    testStatus = RunTest<float>(argc, argv);
  }
  else if (!strcmp(argv[1], "uchar"))
  {
    testStatus = RunTest<unsigned char>(argc, argv);
  }
  else
  {
    std::cerr << "Error" << std::endl;
    std::cerr << "Unable to run test with " << argv[1];
    std::cerr << " pixel type entered as input argument" << std::endl;
    std::cerr << "Expected float or uchar" << std::endl;
    std::cerr << "TEST FAILED!" << std::endl;
    testStatus = EXIT_FAILURE;
  }

  return testStatus;
}
