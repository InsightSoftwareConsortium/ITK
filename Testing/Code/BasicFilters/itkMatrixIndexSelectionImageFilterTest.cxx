/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatrixIndexSelectionImageFilterTest.cxx
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
#include <fstream>
#include "itkMatrixIndexSelectionImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkMatrix.h"

int itkMatrixIndexSelectionImageFilterTest(int argc, char* argv[] )
{
  if( argc < 1 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " outputImage" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;
  typedef itk::Matrix<unsigned short,Dimension,Dimension> PixelType;
  typedef unsigned char OutputPixelType;

  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef itk::Image<OutputPixelType, Dimension> OutputImageType;
  
  // create a matrix image
  ImageType::Pointer image = ImageType::New();
  ImageType::RegionType region;
  ImageType::SizeType size; size.Fill(100);
  ImageType::IndexType index; index.Fill(0);

  region.SetSize(size);
  region.SetIndex(index);
  image->SetRegions( region );
  image->Allocate();

  size = region.GetSize();
  index = region.GetIndex();
  unsigned int width = size[0];
  unsigned int height = size[1];

  // populate upper half of image
  size[0] = width;
  size[1] = height / 2;
  index[0] = 0;
  index[1] = 0;
  region.SetSize(size);
  region.SetIndex(index);
  {
  PixelType pixel;
  pixel[0][0] = 128;  pixel[0][1] = 192;
  pixel[1][0] =   0;  pixel[1][1] =  64;
  std::cout << "pixel: " << pixel << std::endl;
  itk::ImageRegionIterator<ImageType> it(image, region);
  std::cout << region;
  it.GoToBegin();
  while (!it.IsAtEnd())
    {
    it.Set(pixel);
    ++it;
    }  
  }

  // populate lower half of image
  index[0] = 0;
  index[1] = height / 2;
  region.SetSize(size);
  region.SetIndex(index);
  {
  PixelType pixel;
  pixel[0][0] =  64; pixel[0][1] =  16;
  pixel[1][0] = 255; pixel[1][1] = 192;
  std::cout << "pixel: " << pixel << std::endl;
  std::cout << region;
  itk::ImageRegionIterator<ImageType> it(image, region);
  it.GoToBegin();
  while (!it.IsAtEnd())    {
    it.Set(pixel);
    ++it;
    }  
  }

  typedef itk::MatrixIndexSelectionImageFilter<ImageType,OutputImageType> SelectionFilterType;

  SelectionFilterType::Pointer filter = SelectionFilterType::New();
  filter->SetInput(image);
  filter->SetIndices(0,1);

  typedef itk::ImageFileWriter<OutputImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();

  try
    {
    writer->SetInput(filter->GetOutput());
    writer->SetFileName(argv[1]);
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;   
    }

  return EXIT_SUCCESS;   
}
