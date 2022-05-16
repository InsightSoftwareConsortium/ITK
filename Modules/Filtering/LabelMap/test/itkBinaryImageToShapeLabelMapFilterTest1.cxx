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

#include "itkBinaryImageToShapeLabelMapFilter.h"

#include "itkTestingMacros.h"

int
itkBinaryImageToShapeLabelMapFilterTest1(int argc, char * argv[])
{

  if (argc != 9)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputBinaryImage outputShapeLabelMap";
    std::cerr << " fullyConnected(0/1) foregroundValue backgroundValue";
    std::cerr << " feretDiameter perimeter computeOrientedBoundingBox";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 2;

  using ImageType = itk::Image<unsigned char, dim>;

  using LabelObjectType = itk::ShapeLabelObject<unsigned char, dim>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  // reading image to file
  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // converting binary image to shape label map
  using I2LType = itk::BinaryImageToShapeLabelMapFilter<ImageType, LabelMapType>;
  auto i2l = I2LType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(i2l, BinaryImageToShapeLabelMapFilter, ImageToImageFilter);


  itk::SimpleFilterWatcher watcher1(i2l);

  i2l->SetInput(reader->GetOutput());

  // testing get/set FullyConnected macro
  bool fullyConnected = std::stoi(argv[3]);
  i2l->SetFullyConnected(fullyConnected);
  ITK_TEST_SET_GET_VALUE(fullyConnected, i2l->GetFullyConnected());

  // testing boolean FullyConnected macro
  i2l->FullyConnectedOff();
  ITK_TEST_SET_GET_VALUE(false, i2l->GetFullyConnected());

  i2l->FullyConnectedOn();
  ITK_TEST_SET_GET_VALUE(true, i2l->GetFullyConnected());

  // testing get/set InputForegroundValue macro
  int inputForegroundValue = (std::stoi(argv[4]));
  i2l->SetInputForegroundValue(inputForegroundValue);
  ITK_TEST_SET_GET_VALUE(inputForegroundValue, i2l->GetInputForegroundValue());

  // testing get/set OutputBackgroundValue macro
  int outputBackgroundValue = (std::stoi(argv[5]));
  i2l->SetOutputBackgroundValue(outputBackgroundValue);
  ITK_TEST_SET_GET_VALUE(outputBackgroundValue, i2l->GetOutputBackgroundValue());

  bool computeFeretDiameter = (std::stoi(argv[6]));
  ITK_TEST_SET_GET_BOOLEAN(i2l, ComputeFeretDiameter, computeFeretDiameter);

  bool computePerimeter = std::stoi(argv[7]);
  ITK_TEST_SET_GET_BOOLEAN(i2l, ComputePerimeter, computePerimeter);

  bool computeOrientedBoundingBox = std::stoi(argv[8]);
  ITK_TEST_SET_GET_BOOLEAN(i2l, ComputeOrientedBoundingBox, computeOrientedBoundingBox);


  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  auto                     l2i = L2IType::New();
  itk::SimpleFilterWatcher watcher2(l2i);

  l2i->SetInput(i2l->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
