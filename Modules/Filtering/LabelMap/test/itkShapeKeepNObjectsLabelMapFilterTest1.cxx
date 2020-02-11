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
#include "itkSimpleFilterWatcher.h"


#include "itkShapeKeepNObjectsLabelMapFilter.h"
#include "itkLabelImageToShapeLabelMapFilter.h"

#include "itkTestingMacros.h"

int
itkShapeKeepNObjectsLabelMapFilterTest1(int argc, char * argv[])
{
  if (argc != 6)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input output";
    std::cerr << " reverseOrdering attribute numberOfObjectsToKeep";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 3;

  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, dim>;

  using ShapeLabelObjectType = itk::ShapeLabelObject<PixelType, dim>;
  using LabelMapType = itk::LabelMap<ShapeLabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using I2LType = itk::LabelImageToShapeLabelMapFilter<ImageType, LabelMapType>;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());

  using LabelOpeningType = itk::ShapeKeepNObjectsLabelMapFilter<LabelMapType>;
  LabelOpeningType::Pointer opening = LabelOpeningType::New();

  // testing boolean macro for ReverseOrdering
  opening->ReverseOrderingOn();
  ITK_TEST_SET_GET_VALUE(true, opening->GetReverseOrdering());

  opening->ReverseOrderingOff();
  ITK_TEST_SET_GET_VALUE(false, opening->GetReverseOrdering());

  // testing get and set macros or ReverseOrdering
  bool reverseOrdering = std::stoi(argv[3]);
  opening->SetReverseOrdering(reverseOrdering);
  ITK_TEST_SET_GET_VALUE(reverseOrdering, opening->GetReverseOrdering());

  // testing get and set macros for Attribute
  LabelOpeningType::AttributeType attribute = std::stoi(argv[4]);
  ITK_TRY_EXPECT_NO_EXCEPTION(opening->SetAttribute(attribute));
  try
  {
    ITK_TEST_SET_GET_VALUE(attribute, opening->GetAttribute());
  }
  catch (const itk::ExceptionObject & exc)
  {
    std::cerr << "Unexpected exception detected: " << exc;
    return EXIT_FAILURE;
  }
  opening->SetNumberOfObjects(std::stoi(argv[5]));
  opening->SetInput(i2l->GetOutput());

  itk::SimpleFilterWatcher watcher(opening, "filter");

  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput(opening->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
