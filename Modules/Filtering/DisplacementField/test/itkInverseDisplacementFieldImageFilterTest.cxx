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

#include "itkInverseDisplacementFieldImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkThinPlateSplineKernelTransform.h"
#include "itkTestingMacros.h"

int
itkInverseDisplacementFieldImageFilterTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using VectorComponentType = float;

  using VectorType = itk::Vector<VectorComponentType, Dimension>;

  using DisplacementFieldType = itk::Image<VectorType, Dimension>;

  using FilterType = itk::InverseDisplacementFieldImageFilter<DisplacementFieldType, DisplacementFieldType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, InverseDisplacementFieldImageFilter, ImageToImageFilter);


  itk::SimpleFilterWatcher watcher(filter);


  auto kernelTransform = itk::ThinPlateSplineKernelTransform<double, FilterType::ImageDimension>::New();
  filter->SetKernelTransform(kernelTransform);
  ITK_TEST_SET_GET_VALUE(kernelTransform, filter->GetKernelTransform());

  // Creating an input displacement field
  auto field = DisplacementFieldType::New();

  DisplacementFieldType::SpacingType spacing;
  spacing.Fill(1.0);

  DisplacementFieldType::PointType origin;
  origin.Fill(0.0);

  DisplacementFieldType::RegionType region;
  DisplacementFieldType::SizeType   size;
  DisplacementFieldType::IndexType  start;

  size[0] = 128;
  size[1] = 128;

  start[0] = 0;
  start[1] = 0;

  region.SetSize(size);
  region.SetIndex(start);

  field->SetOrigin(origin);
  field->SetSpacing(spacing);
  field->SetRegions(region);
  field->Allocate();

  VectorType pixelValue;

  itk::ImageRegionIteratorWithIndex<DisplacementFieldType> it(field, region);

  // Fill the field with some vectors
  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    DisplacementFieldType::IndexType index = it.GetIndex();
    pixelValue[0] = index[0] * 2.0 - index[0];
    pixelValue[1] = index[1] * 2.0 - index[1];
    it.Set(pixelValue);
    ++it;
  }

  // Since the tested transform is upsampling by a factor of two, the
  // size of the inverse field should be twice the size of the input
  // field. All other geometry parameters are the same.
  filter->SetOutputSpacing(spacing);
  ITK_TEST_SET_GET_VALUE(spacing, filter->GetOutputSpacing());

  // Keep the origin
  filter->SetOutputOrigin(origin);
  ITK_TEST_SET_GET_VALUE(origin, filter->GetOutputOrigin());

  // Set the size
  DisplacementFieldType::SizeType invFieldSize;
  invFieldSize[0] = size[0] * 2;
  invFieldSize[1] = size[1] * 2;

  filter->SetSize(invFieldSize);
  ITK_TEST_SET_GET_VALUE(invFieldSize, filter->GetSize());

  filter->SetInput(field);

  unsigned int subsamplingFactor = 16;
  filter->SetSubsamplingFactor(subsamplingFactor);
  ITK_TEST_SET_GET_VALUE(subsamplingFactor, filter->GetSubsamplingFactor());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->UpdateLargestPossibleRegion());


  // Write an image for regression testing
  using WriterType = itk::ImageFileWriter<DisplacementFieldType>;
  auto writer = WriterType::New();

  writer->SetFileName(argv[1]);
  writer->SetInput(filter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // Now, test for loop invariant (acts as filter validation)
  // f^-1(f(p1) + p1 ) - f(p1)  = 0
  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    DisplacementFieldType::PointType p1;
    field->TransformIndexToPhysicalPoint(it.GetIndex(), p1);

    DisplacementFieldType::PixelType fp1 = it.Get();

    DisplacementFieldType::PointType p2;
    p2[0] = p1[0] + fp1[0];
    p2[1] = p1[1] + fp1[1];

    DisplacementFieldType::IndexType id2 = filter->GetOutput()->TransformPhysicalPointToIndex(p2);
    DisplacementFieldType::PixelType fp2 = filter->GetOutput()->GetPixel(id2);

    if (itk::Math::abs(fp2[0] + fp1[0]) > 0.001 || itk::Math::abs(fp2[1] + fp1[1]) > 0.001)
    {
      std::cerr << "Loop invariant not satisfied for index " << it.GetIndex() << " : f^-1(f(p1) + p1 ) + f(p1)  = 0"
                << std::endl;
      std::cerr << "f(p1) = " << fp1 << std::endl;
      std::cerr << "f^-1(f(p1) + p1 ) = " << fp2 << std::endl;
      std::cerr << "diff: " << fp1[0] + fp2[0] << ", " << fp1[1] + fp2[1] << std::endl;
      return EXIT_FAILURE;
    }
    ++it;
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
