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

#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkAttributePositionLabelMapFilter.h"

/**
 *\class TestLabelObjectAccessor
 * \brief a test accessor - to make kwstyle happy
 */
template <typename TLabelObject>
class TestLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = typename LabelObjectType::RegionType::IndexType;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetBoundingBox().GetIndex();
  }
};


int
itkAttributePositionLabelMapFilterTest1(int argc, char * argv[])
{

  if (argc != 3)
  {
    std::cerr << "usage: " << argv[0] << " input output" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
  }

  // declare the dimension used, and the type of the input image
  constexpr int dim = 3;
  using PType = unsigned char;
  using IType = itk::Image<PType, dim>;

  // We read the input image.
  using ReaderType = itk::ImageFileReader<IType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // And convert it to a LabelMap, with the shape attribute computed.
  // We use the default label object type.
  using I2LType = itk::LabelImageToShapeLabelMapFilter<IType>;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());

  using OpeningType =
    itk::AttributePositionLabelMapFilter<I2LType::OutputImageType,
                                         TestLabelObjectAccessor<I2LType::OutputImageType::LabelObjectType>,
                                         true>;
  OpeningType::Pointer opening = OpeningType::New();
  opening->SetInput(i2l->GetOutput());
  itk::SimpleFilterWatcher watcher(opening, "filter");

  // the label map is then converted back to an label image.
  using L2IType = itk::LabelMapToLabelImageFilter<I2LType::OutputImageType, IType>;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput(opening->GetOutput());

  // write the result
  using WriterType = itk::ImageFileWriter<IType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  return 0;
}
