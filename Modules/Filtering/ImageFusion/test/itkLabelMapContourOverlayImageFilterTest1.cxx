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

#include "itkLabelMapContourOverlayImageFilter.h"
#include "itkTestingMacros.h"


int
itkLabelMapContourOverlayImageFilterTest1(int argc, char * argv[])
{
  if (argc != 10)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input input output opacity type thickness dilation priority sliceDim" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int dim = 2;

  using IType = itk::Image<unsigned char, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using ConverterType = itk::LabelImageToLabelMapFilter<IType>;
  auto converter = ConverterType::New();
  converter->SetInput(reader->GetOutput());

  auto reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  using ColorizerType = itk::LabelMapContourOverlayImageFilter<ConverterType::OutputImageType, IType>;
  auto colorizer = ColorizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(colorizer, LabelMapContourOverlayImageFilter, LabelMapFilter);


  colorizer->SetInput(converter->GetOutput());

  colorizer->SetFeatureImage(reader2->GetOutput());
  ITK_TEST_SET_GET_VALUE(reader2->GetOutput(), colorizer->GetFeatureImage());

  auto opacity = std::stod(argv[4]);
  colorizer->SetOpacity(opacity);
  ITK_TEST_SET_GET_VALUE(opacity, colorizer->GetOpacity());

  auto type = std::stoi(argv[5]);
  colorizer->SetType(type);
  ITK_TEST_SET_GET_VALUE(type, colorizer->GetType());

  ColorizerType::SizeType r;
  r.Fill(std::stoi(argv[6]));
  colorizer->SetContourThickness(r);
  ITK_TEST_SET_GET_VALUE(r, colorizer->GetContourThickness());

  r.Fill(std::stoi(argv[7]));
  colorizer->SetDilationRadius(r);
  ITK_TEST_SET_GET_VALUE(r, colorizer->GetDilationRadius());

  auto priority = std::stoi(argv[8]);
  colorizer->SetPriority(priority);
  ITK_TEST_SET_GET_VALUE(priority, colorizer->GetPriority());

  auto sliceDimension = std::stoi(argv[9]);
  colorizer->SetSliceDimension(sliceDimension);
  ITK_TEST_SET_GET_VALUE(sliceDimension, colorizer->GetSliceDimension());

  itk::SimpleFilterWatcher watcher(colorizer, "filter");

  using WriterType = itk::ImageFileWriter<ColorizerType::OutputImageType>;
  auto writer = WriterType::New();
  writer->SetInput(colorizer->GetOutput());
  writer->SetFileName(argv[3]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
