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

int itkNiftiImageIOTest6(int ac, char *av[])
{
  if(ac > 1)
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }
  int success(EXIT_SUCCESS);

  typedef itk::VectorImage<double,3> VectorImageType;
  VectorImageType::RegionType imageRegion;
  VectorImageType::SizeType size;
  VectorImageType::IndexType index;
  VectorImageType::SpacingType spacing;
  VectorImageType::VectorLengthType vecLength(4);

  for(unsigned i = 0; i < 3; i++)
    {
    size[i] = 3;
    index[i] = 0;
    spacing[i] = 1.0;
    }
  imageRegion.SetSize(size); imageRegion.SetIndex(index);
  VectorImageType::Pointer vecImage =
    itk::IOTestHelper::AllocateImageFromRegionAndSpacing<VectorImageType>(imageRegion, spacing,vecLength);

  itk::ImageRegionIterator<VectorImageType>
    it(vecImage,vecImage->GetLargestPossibleRegion());
  double val(0.0);
  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    VectorImageType::PixelType p(vecLength);
    for(unsigned i = 0; i < vecLength; i++)
      {
      p[i] = val;
      val++;
      }
    it.Set(p);
    }
  const std::string testfname("vectorImage.nii.gz");
  VectorImageType::Pointer readback;
  try
    {
    itk::IOTestHelper::WriteImage<VectorImageType,itk::NiftiImageIO>(vecImage,testfname);
    readback = itk::IOTestHelper::ReadImage<VectorImageType>(testfname);
    }
  catch(itk::ExceptionObject &err)
    {
    std::cout << "itkNiftiImageIOTest6" << std::endl
              << "Exception Object caught: " << std::endl
              << err << std::endl;
    throw;
    }
  itk::ImageRegionIterator<VectorImageType>
    readbackIt(readback,readback->GetLargestPossibleRegion());
  for(it.GoToBegin(),readbackIt.GoToBegin();
      !it.IsAtEnd() && !readbackIt.IsAtEnd();
      ++it, ++readbackIt)
    {
    VectorImageType::PixelType p(vecLength),
      readbackP(vecLength);
    p = it.Get();
    readbackP = readbackIt.Get();
    if(p != readbackP)
      {
      std::cout << "Pixel mismatch at index "
                << it.GetIndex()
                << " original = "
                << p
                << " read value = "
                << readbackP
                << std::endl;
      success = EXIT_FAILURE;
      break;
      }
    }
  itk::IOTestHelper::Remove(testfname.c_str());
  return success;
}
