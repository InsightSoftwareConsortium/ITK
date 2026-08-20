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

#include <iostream>

#include "itkMINCImageIOFactory.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


int
itkMINCImageIOTest_4D(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputfile outputfile" << std::endl;
    return EXIT_FAILURE;
  }

  itk::MINCImageIOFactory::RegisterOneFactory();

  // A MINC volume with x/y/z spatial dimensions plus a time dimension is read as
  // a genuine 4D itk::Image, with the time step/start mapped to the 4th
  // dimension spacing/origin.
  using ImageType = itk::Image<float, 4>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);
  writer->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  const ImageType::ConstPointer image = reader->GetOutput();
  image->Print(std::cout);

  // Round-trip a synthetic 4D volume with a non-trivial time step/start and
  // verify the time geometry survives, together with the voxel values.
  const std::string synthetic = std::string(argv[2]) + "_synthetic.mnc";

  auto            synthImage = ImageType::New();
  constexpr float timeStep = 2.5F;
  constexpr float timeStart = 10.0F;

  ImageType::RegionType region;
  region.SetSize({ { 3, 4, 5, 2 } });
  synthImage->SetRegions(region);

  ImageType::SpacingType spacing;
  spacing[0] = 1.0;
  spacing[1] = 1.0;
  spacing[2] = 1.0;
  spacing[3] = timeStep;
  synthImage->SetSpacing(spacing);

  ImageType::PointType origin;
  origin[0] = 0.0;
  origin[1] = 0.0;
  origin[2] = 0.0;
  origin[3] = timeStart;
  synthImage->SetOrigin(origin);

  synthImage->Allocate();

  {
    float                               v = 0.0F;
    itk::ImageRegionIterator<ImageType> it(synthImage, synthImage->GetLargestPossibleRegion());
    for (; !it.IsAtEnd(); ++it, ++v)
    {
      it.Set(v);
    }
  }

  auto synthWriter = WriterType::New();
  synthWriter->SetFileName(synthetic);
  synthWriter->SetInput(synthImage);
  ITK_TRY_EXPECT_NO_EXCEPTION(synthWriter->Update());

  auto synthReader = ReaderType::New();
  synthReader->SetFileName(synthetic);
  ITK_TRY_EXPECT_NO_EXCEPTION(synthReader->Update());

  const ImageType::ConstPointer synthBack = synthReader->GetOutput();

  ITK_TEST_EXPECT_EQUAL(synthBack->GetLargestPossibleRegion().GetSize()[3], 2);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(static_cast<float>(synthBack->GetSpacing()[3]), timeStep));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(static_cast<float>(synthBack->GetOrigin()[3]), timeStart));

  itk::ImageRegionConstIterator<ImageType> inIt(synthImage, synthImage->GetLargestPossibleRegion());
  itk::ImageRegionConstIterator<ImageType> outIt(synthBack, synthBack->GetLargestPossibleRegion());
  for (; !inIt.IsAtEnd(); ++inIt, ++outIt)
  {
    if (itk::Math::NotAlmostEquals(inIt.Get(), outIt.Get()))
    {
      std::cerr << "Voxel mismatch after 4D round-trip at " << inIt.ComputeIndex() << ": " << inIt.Get()
                << " != " << outIt.Get() << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
