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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkNiftiImageIOTest.h"
#include "itkNumericTraits.h"

#if defined(ITK_USE_MODULAR_BUILD)
  #define SPECIFIC_IMAGEIO_MODULE_TEST
#endif

template <class RGBPixelType>
int RGBTest(int ac, char *av[])
{
  if(ac > 2)
    {
    char *testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  else
    {
    return EXIT_FAILURE;
    }
  char * tmpImage = *++av;
  int success(EXIT_SUCCESS);
  typedef typename itk::Image<RGBPixelType,3> RGBImageType;
  typename RGBImageType::RegionType imageRegion;
  typename RGBImageType::SizeType size;
  typename RGBImageType::IndexType index;
  typename RGBImageType::SpacingType spacing;
  typename RGBImageType::PointType origin;
  typename RGBImageType::DirectionType myDirection;
  for(unsigned i = 0; i < 3; i++)
    {
    size[i] = 5;
    index[i] = 0;
    spacing[i] = 1.0;
    origin[i] = 0;
    }
  imageRegion.SetSize(size);
  imageRegion.SetIndex(index);
  typename RGBImageType::Pointer im;
  AllocateImageFromRegionAndSpacing(RGBImageType,im,imageRegion,spacing);
  vnl_random randgen(12345678);
  itk::ImageRegionIterator<RGBImageType> it(im,im->GetLargestPossibleRegion());
  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    RGBPixelType pix;
    for(unsigned int i = 0; i < RGBPixelType::Dimension; i++)
      {
      pix[i] = randgen.lrand32(255);
      }
    it.Set(pix);
    }
  typename RGBImageType::Pointer im2;
  try
    {
    WriteImage<RGBImageType>(im,std::string(tmpImage));
    im2 = ReadImage<RGBImageType>(std::string(tmpImage));
    }
  catch(itk::ExceptionObject &err)
    {
    std::cout << "itkNiftiImageIOTest9" << std::endl
              << "Exception Object caught: " << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }
  itk::ImageRegionIterator<RGBImageType> it2(im2,im2->GetLargestPossibleRegion());
  for(it.GoToBegin(),it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it,++it2)
    {
    if(it.Value() != it2.Value())
      {
      std::cout << "Original Pixel (" << it.Value()
                << ") doesn't match read-in Pixel ("
                << it2.Value() << std::endl;
      success = EXIT_FAILURE;
      break;
      }
    }
  Remove(tmpImage);
  return success;
}

int itkNiftiImageIOTest9(int ac, char *av[])
{
  return RGBTest<itk::RGBPixel<unsigned char> >(ac,av);
}

int itkNiftiImageIOTest10(int ac, char *av[])
{
  return RGBTest<itk::RGBAPixel<unsigned char> >(ac,av);
}

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
  ImageType::Pointer im;
  AllocateImageFromRegionAndSpacing(ImageType,im,imageRegion,spacing);
  ImageType::DirectionType dir(CORDirCosines<ImageType>());
  std::cout << "itkNiftiImageIOTest11" << std::endl;
  std::cout << "Direction = " << dir << std::endl;
  im->SetDirection(dir);
  try
    {
    WriteImage<ImageType>(im,testfilename);
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
