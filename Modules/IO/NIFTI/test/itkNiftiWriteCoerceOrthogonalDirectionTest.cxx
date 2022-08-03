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
#include "itkIOTestHelper.h"
#include "itkTestingMacros.h"
#include "itkNiftiImageIO.h"

int
itkNiftiWriteCoerceOrthogonalDirectionTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << "testOutputDir" << std::endl;
    std::cerr << "1 argument required, received " << argc << std::endl;
    for (int i = 0; i < argc; ++i)
    {
      std::cerr << "\t" << i << " : " << argv[i] << std::endl;
    }
    return EXIT_FAILURE;
  }

  const unsigned int dim = 2;
  using ImageType = itk::Image<unsigned char, dim>;

  ImageType::IndexType  startIndex = { { 0, 0 } };
  ImageType::SizeType   imageSize = { { 2, 2 } };
  ImageType::RegionType region;
  region.SetSize(imageSize);
  region.SetIndex(startIndex);
  auto image1 = ImageType::New();
  image1->SetRegions(region);
  image1->Allocate(true);

  ImageType::DirectionType mat1;
  mat1.SetIdentity();
  mat1[0][0] = 0.5; // make matrix non-orthonormal
  image1->SetDirection(mat1);

  const std::string outputDir = argv[1];
  const std::string outputFilename = outputDir + "/coercedDirection.nii.gz";
  itk::IOTestHelper::WriteImage<ImageType, itk::NiftiImageIO>(image1, outputFilename);
  auto image2 = itk::IOTestHelper::ReadImage<ImageType>(outputFilename);
  // Nifti image writing coerces the direction matrix to be
  // orthonormal, so the matrices are expected to be different
  if (image1->GetDirection() == image2->GetDirection())
  {
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
