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

#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkDirectedHausdorffDistanceImageFilter.h"
#include "itkTestingMacros.h"
#include "itkPrintHelper.h"

using namespace itk::print_helper;


int
itkDirectedHausdorffDistanceImageFilterTest2(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName outputFileName" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader1 = ReaderType::New();
  auto reader2 = ReaderType::New();
  reader1->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader1->Update());

  reader2->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader2->Update());


  using FilterType = itk::DirectedHausdorffDistanceImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, DirectedHausdorffDistanceImageFilter, ImageToImageFilter);


  typename ImageType::Pointer image1 = reader1->GetOutput();
  filter->SetInput1(image1);
  ITK_TEST_SET_GET_VALUE(image1, filter->GetInput1());

  typename ImageType::Pointer image2 = reader2->GetOutput();
  filter->SetInput2(image2);
  ITK_TEST_SET_GET_VALUE(image2, filter->GetInput2());

  ITK_TEST_SET_GET_BOOLEAN(filter, UseImageSpacing, true);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  double expecteDirectedHausdorffDistance = 0;
  ITK_TEST_EXPECT_EQUAL(expecteDirectedHausdorffDistance, filter->GetDirectedHausdorffDistance());

  double expecteAverageHausdorffDistance = 0;
  ITK_TEST_EXPECT_EQUAL(expecteAverageHausdorffDistance, filter->GetAverageHausdorffDistance());


  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
