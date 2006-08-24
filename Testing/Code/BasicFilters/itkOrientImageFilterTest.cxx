/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrientImageFilterTest.cxx
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

#include "itkMultiThreader.h"
#include "itkAnalyzeImageIO.h"
#include "itkMetaDataObject.h"
#include "itkImage.h"
#include "itkSpatialOrientation.h"
#include "itkOrientImageFilter.h"
#include "itkIOCommon.h"
#include <vnl/vnl_sample.h>

typedef itk::Image<unsigned int,3> ImageType;

ImageType::Pointer CreateRandomImage()
{
  const ImageType::SizeType imageSize = {{4,4,4}};
  const ImageType::IndexType imageIndex = {{0,0,0}};
  ImageType::RegionType region;
  region.SetSize(imageSize);
  region.SetIndex(imageIndex);
  ImageType::Pointer img = ImageType::New();
  img->SetLargestPossibleRegion(region);
  img->SetBufferedRegion(region);
  img->SetRequestedRegion(region);
  img->Allocate();
  itk::ImageRegionIterator<ImageType> ri(img,region);
  while(!ri.IsAtEnd())
    {
    ri.Set( (unsigned int) vnl_sample_uniform(0, 32767) );
    ++ri;
    }
  return img;
}

static void PrintImg(ImageType::Pointer img)
{
  //  std::cerr << img << std::endl;
  // std::cerr << std::endl << "-------------------" << std::endl;
  ImageType::IndexType Index;  
  for(Index[2] = 0;Index[2] < 4; Index[2]++)
    {
    for(Index[1] = 0; Index[1] < 4; Index[1]++)
      {
      for(Index[0] = 0; Index[0] < 4; Index[0]++)
        {
        std::cerr << img->GetPixel(Index) << " ";
        }
      std::cerr << std::endl;
      }
    std::cerr << std::endl;
    }
}

int itkOrientImageFilterTest(int,char *[])
{
  itk::MultiThreader::SetGlobalMaximumNumberOfThreads(1);
  ImageType::Pointer randimage = CreateRandomImage();
  std::cerr << "Original" << std::endl;
  PrintImg(randimage);

  // act like we're in RIP.
#if 0
  itk::EncapsulateMetaData<itk::SpatialOrientation::ValidCoordinateOrientationFlags>
    (randimage->GetMetaDataDictionary(),
     itk::ITK_CoordinateOrientation,
     itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP);
#endif

  itk::OrientImageFilter<ImageType,ImageType>::Pointer orienter =
    itk::OrientImageFilter<ImageType,ImageType>::New();

  orienter->SetGivenCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP);
  orienter->SetInput(randimage);

  // try permuting axes
  orienter->SetDesiredCoordinateOrientation
    (itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP);
  orienter->Update();
  ImageType::Pointer IRP  = orienter->GetOutput();
  std::cerr << "IRP" << std::endl;
  PrintImg(IRP);

  ImageType::RegionType::SizeType originalSize = 
    randimage->GetLargestPossibleRegion().GetSize();
  ImageType::RegionType::SizeType transformedSize = 
    IRP->GetLargestPossibleRegion().GetSize();
  ImageType::IndexType originalIndex, transformedIndex;

  for(originalIndex[2] = transformedIndex[2] = 0;
      originalIndex[2] < static_cast<ImageType::IndexType::IndexValueType>(originalSize[2]); originalIndex[2]++,transformedIndex[2]++)
    {
    for(originalIndex[1] = transformedIndex[0] = 0; 
        originalIndex[1] < static_cast<ImageType::IndexType::IndexValueType>(originalSize[1]); originalIndex[1]++,transformedIndex[0]++)
      {
      for(originalIndex[0] = transformedIndex[1] = 0; 
          originalIndex[0] < static_cast<ImageType::IndexType::IndexValueType>(originalSize[0]); originalIndex[0]++,transformedIndex[1]++)
        {
        ImageType::PixelType orig = randimage->GetPixel(originalIndex);
        ImageType::PixelType xfrm = IRP->GetPixel(transformedIndex);
        if(orig != xfrm)
          return -1;
        }
      }
    }

  // go to LIP, to check flipping an axis.
  orienter = itk::OrientImageFilter<ImageType,ImageType>::New();
  orienter->SetInput(randimage);
  orienter->SetGivenCoordinateOrientation(itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP);
  orienter->SetDesiredCoordinateOrientation
    (itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP);
  orienter->Update();
  ImageType::Pointer LIP  = orienter->GetOutput();
  std::cerr << "LIP" << std::endl;
  PrintImg(LIP);
  transformedSize = LIP->GetLargestPossibleRegion().GetSize();
  
  for(originalIndex[2] = transformedIndex[2] = 0; 
      originalIndex[2] < static_cast<ImageType::IndexType::IndexValueType>(originalSize[2]); originalIndex[2]++,transformedIndex[2]++)
    {
    for(originalIndex[1] = transformedIndex[1] = 0; 
        originalIndex[1] < static_cast<ImageType::IndexType::IndexValueType>(originalSize[1]); originalIndex[1]++,transformedIndex[1]++)
      {
      for(originalIndex[0] = 0, 
            transformedIndex[0] = transformedSize[0] - 1; 
          originalIndex[0] < static_cast<ImageType::IndexType::IndexValueType>(originalSize[0]); originalIndex[0]++,transformedIndex[0]--)
        {
        ImageType::PixelType orig = randimage->GetPixel(originalIndex);
        ImageType::PixelType xfrm = LIP->GetPixel(transformedIndex);
        if(orig != xfrm)
          return -1;
        }
      }
    }

  return 0;
}
