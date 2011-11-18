/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkNiftiImageIOTest.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkNiftiImageIOTest11(int ac, char *av[])
{
  std::string testfilename;
  if(ac > 1)
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }
  if(ac > 2)
    {
    testfilename = *++av;
    }
  else
    {
    return EXIT_FAILURE;
    }
  typedef  itk::Image<char,3> ImageType;
  ImageType::RegionType imageRegion;
  ImageType::SizeType size;
  ImageType::IndexType index;
  ImageType::SpacingType spacing;
  ImageType::PointType origin;
  ImageType::DirectionType myDirection;

  size[0] = static_cast<long int>(itk::NumericTraits<short>::max()) * 2;
  size[1] = 1;
  size[2] = 1;

  index.Fill(0);
  spacing.Fill(1.0);
  origin.Fill(0.0);

  imageRegion.SetSize(size);
  imageRegion.SetIndex(index);
  ImageType::Pointer im =
    itk::IOTestHelper::AllocateImageFromRegionAndSpacing<ImageType>(imageRegion,spacing);
  ImageType::DirectionType dir(CORDirCosines<ImageType>());
  std::cout << "itkNiftiImageIOTest11" << std::endl;
  std::cout << "Direction = " << dir << std::endl;
  im->SetDirection(dir);
  try
    {
    itk::IOTestHelper::WriteImage<ImageType,itk::NiftiImageIO>(im,testfilename);
    std::cerr << "FAILED to catch expected exception" << std::endl;
    return EXIT_FAILURE;
    }
  catch (itk::ExceptionObject & e)
    {
    std::cout << "EXPECTED exception in file writer " << std::endl;
    std::cout << e.GetDescription() << std::endl;
    std::cout << e.GetLocation() << std::endl;
    }

  return EXIT_SUCCESS;
}
