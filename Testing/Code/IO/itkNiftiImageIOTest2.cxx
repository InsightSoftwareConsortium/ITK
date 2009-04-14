/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkNiftiImageIOTest2.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkNiftiImageIOTest.h"


template <class RGBPixelType>
int RGBTest(int ac, char *av[])
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
    WriteImage<RGBImageType>(im,std::string("RGBImage.nii.gz"));
    im2 = ReadImage<RGBImageType>(std::string("RGBImage.nii.gz"));
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
  Remove("RGBImage.nii.gz");
  return success;
}

int itkNiftiImageIOTest9(int ac, char *av[])
{
  return RGBTest<itk::RGBPixel<unsigned char> >(ac,av);
}

int itkNiftiImageIOTest10(int ac, char *av[])
{
#ifndef __BORLANDC__
  return RGBTest<itk::RGBAPixel<unsigned char> >(ac,av);
#else
  if (ac == 2)
    {
    std::cout <<"Dummy test for Borland platforms: " << av[0] << " " << av[1] << std::endl;
    }
  return EXIT_SUCCESS;
#endif
}
