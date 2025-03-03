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

#include "itkWatershedImageFilter.h"
#include "itkWatershedEquivalenceRelabeler.h"
#include "itkWatershedBoundaryResolver.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


int
itkWatershedImageFilterTest(int, char *[])
{

  constexpr unsigned int Dimension = 2;

  using PixelType = float;
  using ImageType2D = itk::Image<PixelType, Dimension>;
  using LongImageType2D = itk::Image<itk::IdentifierType, Dimension>;


  itk::ImageRegion<Dimension> region;

  itk::Size<Dimension> size;
  size[0] = 314;
  size[1] = 314;

  itk::Index<Dimension> origin;
  origin[0] = 0;
  origin[1] = 0;

  region.SetSize(size);
  region.SetIndex(origin);

  auto image2D = ImageType2D::New();
  image2D->SetRegions(region);
  image2D->Allocate();

  auto longimage2D = LongImageType2D::New();
  longimage2D->SetRegions(region);
  longimage2D->AllocateInitialized();

  itk::ImageRegionIterator<ImageType2D> it2D(image2D, image2D->GetRequestedRegion());

  for (float q = 0.00f; !it2D.IsAtEnd(); ++it2D)
  {
    it2D.Value() = std::sin(q);
    q += 0.10f;
  }

  // Test various objects associated to itk::WatershedImageFilter
  //

  // Test EquivalenceRelabeler
  const itk::EquivalencyTable::Pointer table = itk::EquivalencyTable::New();

  const itk::watershed::EquivalenceRelabeler<LongImageType2D::PixelType, Dimension>::Pointer eq =
    itk::watershed::EquivalenceRelabeler<LongImageType2D::PixelType, Dimension>::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(eq, EquivalenceRelabeler, ProcessObject);


  eq->SetInputImage(longimage2D);

  eq->SetEquivalencyTable(table);
  ITK_TEST_SET_GET_VALUE(table, eq->GetEquivalencyTable());


  ITK_TRY_EXPECT_NO_EXCEPTION(eq->Update());

  // Test WatershedMiniPipelineProgressCommand
  // Forcing the execution of the const Execute method which is not normally called.
  const itk::WatershedMiniPipelineProgressCommand::Pointer wmppc = itk::WatershedMiniPipelineProgressCommand::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(wmppc, WatershedMiniPipelineProgressCommand, Command);

  constexpr double count = 2.0;
  wmppc->SetCount(count);
  ITK_TEST_SET_GET_VALUE(count, wmppc->GetCount());

  constexpr unsigned int numberOfFilters = 2;
  wmppc->SetNumberOfFilters(numberOfFilters);
  ITK_TEST_SET_GET_VALUE(numberOfFilters, wmppc->GetNumberOfFilters());

  wmppc->SetFilter(eq);
  ITK_TEST_SET_GET_VALUE(eq, wmppc->GetFilter());

  const itk::ProcessObject * constp = eq.GetPointer();
  wmppc->Execute(constp, itk::ProgressEvent());
  wmppc->Execute(eq, itk::ProgressEvent());

  // Test watershed::BoundaryResolver
  const itk::watershed::BoundaryResolver<PixelType, Dimension>::Pointer br =
    itk::watershed::BoundaryResolver<PixelType, Dimension>::New();
  if (br.IsNull())
  {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "Null itk::watershed::BoundaryResolver." << std::endl;
    return EXIT_FAILURE;
  }

  ITK_EXERCISE_BASIC_OBJECT_METHODS(br, BoundaryResolver, ProcessObject);

  const itk::watershed::Boundary<PixelType, 1>::Pointer boundaryA = itk::watershed::Boundary<PixelType, 1>::New();
  if (boundaryA.IsNull())
  {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "Null itk::watershed::Boundary." << std::endl;
    return EXIT_FAILURE;
  }

  ITK_EXERCISE_BASIC_OBJECT_METHODS(boundaryA, Boundary, DataObject);

  const itk::watershed::Boundary<PixelType, 1>::Pointer boundaryB = itk::watershed::Boundary<PixelType, 1>::New();
  if (boundaryB.IsNull())
  {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "Null itk::watershed::Boundary." << std::endl;
    return EXIT_FAILURE;
  }


  const itk::WatershedImageFilter<ImageType2D>::Pointer watershedFilter = itk::WatershedImageFilter<ImageType2D>::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(watershedFilter, WatershedImageFilter, ImageToImageFilter);

  const itk::SimpleFilterWatcher watchIt(watershedFilter, "WatershedImageFilter");

  constexpr double threshold = .05;
  watershedFilter->SetThreshold(threshold);
  ITK_TEST_SET_GET_VALUE(threshold, watershedFilter->GetThreshold());

  constexpr double level = 1.0;
  watershedFilter->SetLevel(level);
  ITK_TEST_SET_GET_VALUE(level, watershedFilter->GetLevel());

  unsigned int inputId = 1;
  ITK_TRY_EXPECT_EXCEPTION(watershedFilter->SetInput(inputId, image2D));

  watershedFilter->SetInput(image2D);
  const ImageType2D * input = watershedFilter->GetInput();

  inputId = 0;
  watershedFilter->SetInput(inputId, image2D);
  ITK_TEST_SET_GET_VALUE(input, watershedFilter->GetInput());


  ITK_TRY_EXPECT_NO_EXCEPTION(watershedFilter->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
