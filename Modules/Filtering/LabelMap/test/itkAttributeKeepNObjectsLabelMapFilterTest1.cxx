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


#include "itkLabelImageToLabelMapFilter.h"
#include "itkAttributeKeepNObjectsLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int
itkAttributeKeepNObjectsLabelMapFilterTest1(int argc, char * argv[])
{
  if (argc != 5)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input output";
    std::cerr << " nbOfObjects reverseOrdering(0/1)";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 3;

  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, dim>;

  using LabelObjectType = itk::AttributeLabelObject<PixelType, dim, int>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using I2LType = itk::LabelImageToLabelMapFilter<ImageType, LabelMapType>;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());


  // The next step is made outside the pipeline model, so we call Update() now.
  i2l->Update();

  // Now we will valuate the attributes. The attribute will be the object position
  // in the label map
  LabelMapType::Pointer labelMap = i2l->GetOutput();

  int pos = 0;
  for (LabelMapType::Iterator it(labelMap); !it.IsAtEnd(); ++it)
  {
    LabelObjectType * labelObject = it.GetLabelObject();
    labelObject->SetAttribute(pos++);
  }


  using LabelKeepNObjectsType = itk::AttributeKeepNObjectsLabelMapFilter<LabelMapType>;
  LabelKeepNObjectsType::Pointer opening = LabelKeepNObjectsType::New();

  // testing get and set macros for NumberOfObjects
  unsigned long nbOfObjects = std::stoi(argv[3]);
  opening->SetNumberOfObjects(nbOfObjects);
  ITK_TEST_SET_GET_VALUE(nbOfObjects, opening->GetNumberOfObjects());

  // testing get and set macros for ReverseOrdering
  // testing boolean macro for ReverseOrdering
  opening->ReverseOrderingOn();
  ITK_TEST_SET_GET_VALUE(true, opening->GetReverseOrdering());

  opening->ReverseOrderingOff();
  ITK_TEST_SET_GET_VALUE(false, opening->GetReverseOrdering());

  bool reverseOrdering = std::stoi(argv[4]);
  opening->SetReverseOrdering(reverseOrdering);
  ITK_TEST_SET_GET_VALUE(reverseOrdering, opening->GetReverseOrdering());

  opening->SetInput(labelMap);

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
