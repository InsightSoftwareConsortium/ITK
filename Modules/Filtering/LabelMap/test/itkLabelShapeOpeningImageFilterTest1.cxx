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

#include "itkLabelShapeOpeningImageFilter.h"

#include "itkTestingMacros.h"

int
itkLabelShapeOpeningImageFilterTest1(int argc, char * argv[])
{

  if (argc != 7)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input output";
    std::cerr << " background lambda";
    std::cerr << "reverseOrdering attribute" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 2;

  using IType = itk::Image<unsigned char, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using LabelOpeningType = itk::LabelShapeOpeningImageFilter<IType>;
  auto opening = LabelOpeningType::New();

  opening->SetInput(reader->GetOutput());

  int BackgroundValue = (std::stoi(argv[3]));
  opening->SetBackgroundValue(BackgroundValue);
  ITK_TEST_SET_GET_VALUE(BackgroundValue, opening->GetBackgroundValue());

  double lambda = std::stod(argv[4]);
  opening->SetLambda(lambda);
  ITK_TEST_SET_GET_VALUE(lambda, opening->GetLambda());

  bool reverseOrdering = std::stoi(argv[5]);
  ITK_TEST_SET_GET_BOOLEAN(opening, ReverseOrdering, reverseOrdering);

  LabelOpeningType::AttributeType attribute = std::stoi(argv[6]);
  opening->SetAttribute(attribute);
  ITK_TEST_SET_GET_VALUE(attribute, opening->GetAttribute());

  itk::SimpleFilterWatcher watcher(opening, "filter");

  using WriterType = itk::ImageFileWriter<IType>;
  auto writer = WriterType::New();
  writer->SetInput(opening->GetOutput());
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "Test Complete!" << std::endl;

  return EXIT_SUCCESS;
}
