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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkTestingMacros.h"

int
itkLabelImageToShapeLabelMapFilterTest1(int argc, char * argv[])
{
  if (argc != 6)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputBinaryImage outputShapeLabelMap";
    std::cerr << " backgroundValue computeFeretDiameter computePerimeter";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 2;

  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, dim>;

  using LabelObjectType = itk::ShapeLabelObject<PixelType, dim>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using L2SType = itk::LabelImageToShapeLabelMapFilter<ImageType, LabelMapType>;
  L2SType::Pointer l2s = L2SType::New();
  l2s->SetInput(reader->GetOutput());

  const PixelType backgroundValue = std::stoi(argv[3]);
  l2s->SetBackgroundValue(backgroundValue);
  ITK_TEST_SET_GET_VALUE(backgroundValue, l2s->GetBackgroundValue());

  const bool computeFeretDiameter = std::stoi(argv[4]);
  l2s->SetComputeFeretDiameter(computeFeretDiameter);
  ITK_TEST_SET_GET_VALUE(computeFeretDiameter, l2s->GetComputeFeretDiameter());

  const bool computePerimeter = std::stoi(argv[5]);
  l2s->SetComputePerimeter(computePerimeter);
  ITK_TEST_SET_GET_VALUE(computePerimeter, l2s->GetComputeFeretDiameter());


  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput(l2s->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
