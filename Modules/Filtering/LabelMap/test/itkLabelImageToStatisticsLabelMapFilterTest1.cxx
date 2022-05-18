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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelImageToStatisticsLabelMapFilter.h"

#include "itkTestingMacros.h"

int
itkLabelImageToStatisticsLabelMapFilterTest1(int argc, char * argv[])
{

  if (argc != 9)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputLabelImage inputGrayscaleImage outputStatisticsLabelMap";
    std::cerr << " backgroundValue";
    std::cerr << " feretDiameter, perimeter, histogram, numberOfBins";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 2;

  using ImageType = itk::Image<unsigned char, dim>;

  // reading image to file
  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  auto reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  // converting Label image to Statistics label map
  // don't set the output type to test the default value of the template parameter
  using I2LType = itk::LabelImageToStatisticsLabelMapFilter<ImageType, ImageType>;
  auto                     i2l = I2LType::New();
  itk::SimpleFilterWatcher watcher1(i2l);

  i2l->SetInput(reader->GetOutput());

  // test all the possible ways to set the feature image. Be sure they can work with const images.
  ImageType::ConstPointer constOutput = reader2->GetOutput();
  i2l->SetInput2(constOutput);
  i2l->SetFeatureImage(constOutput);
  i2l->SetInput(1, constOutput);

  // testing get/set BackgroundValue macro
  int BackgroundValue = (std::stoi(argv[4]));
  i2l->SetBackgroundValue(BackgroundValue);
  ITK_TEST_SET_GET_VALUE(BackgroundValue, i2l->GetBackgroundValue());

  // testing get/set ComputeFeretDiameter macro
  bool computeFeretDiameter = (std::stoi(argv[5]));
  i2l->SetComputeFeretDiameter(computeFeretDiameter);
  ITK_TEST_SET_GET_VALUE(computeFeretDiameter, i2l->GetComputeFeretDiameter());

  // testing boolean ComputeFeretDiameter macro
  i2l->ComputeFeretDiameterOff();
  ITK_TEST_SET_GET_VALUE(false, i2l->GetComputeFeretDiameter());

  i2l->ComputeFeretDiameterOn();
  ITK_TEST_SET_GET_VALUE(true, i2l->GetComputeFeretDiameter());

  // testing get/set ComputePerimeter macro
  bool computePerimeter = std::stoi(argv[6]);
  i2l->SetComputePerimeter(computePerimeter);
  ITK_TEST_SET_GET_VALUE(computePerimeter, i2l->GetComputePerimeter());

  // testing boolean ComputePerimeter macro
  i2l->ComputePerimeterOff();
  ITK_TEST_SET_GET_VALUE(false, i2l->GetComputePerimeter());

  i2l->ComputePerimeterOn();
  ITK_TEST_SET_GET_VALUE(true, i2l->GetComputePerimeter());

  // testing get/set ComputeHistogram macro
  bool computeHistogram = (std::stoi(argv[7]));
  i2l->SetComputeHistogram(computeHistogram);
  ITK_TEST_SET_GET_VALUE(computeHistogram, i2l->GetComputeHistogram());

  // testing boolean ComputeHistogram macro
  i2l->ComputeHistogramOff();
  ITK_TEST_SET_GET_VALUE(false, i2l->GetComputeHistogram());

  i2l->ComputeHistogramOn();
  ITK_TEST_SET_GET_VALUE(true, i2l->GetComputeHistogram());

  // testing get/set NumberOfBins macro
  unsigned int numberOfBins = (std::stoi(argv[8]));
  i2l->SetNumberOfBins(numberOfBins);
  ITK_TEST_SET_GET_VALUE(numberOfBins, i2l->GetNumberOfBins());

  using L2IType = itk::LabelMapToLabelImageFilter<I2LType::OutputImageType, ImageType>;
  auto                     l2i = L2IType::New();
  itk::SimpleFilterWatcher watcher2(l2i);

  l2i->SetInput(i2l->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[3]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
