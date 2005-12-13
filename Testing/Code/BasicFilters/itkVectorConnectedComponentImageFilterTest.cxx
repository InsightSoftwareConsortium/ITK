/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorConnectedComponentImageFilterTest.cxx
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
#include "itkVectorConnectedComponentImageFilter.h"
#include "itkRelabelComponentImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkVector.h"
#include "itkTextOutput.h"

int itkVectorConnectedComponentImageFilterTest(int argc, char* argv[] )
{
  if( argc < 1 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " outputImage" << std::endl;
    return EXIT_FAILURE;
    }

  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  const unsigned int Dimension = 2;
  typedef itk::Vector<float,Dimension> PixelType;
  typedef unsigned long OutputPixelType;
  typedef unsigned char LabelPixelType;

  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef itk::Image<OutputPixelType, Dimension> OutputImageType;
  typedef itk::Image<LabelPixelType, Dimension> LabelImageType;
  
  // create an image of vectors
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
  size[0] = width / 2;
  size[1] = width / 2;

  index[0] = 0;
  index[1] = 0;
  region.SetSize(size);
  region.SetIndex(index);
  {
  PixelType pixel;
  pixel[0] = 1; pixel[1] = 0;
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

  index[0] = width/2;
  index[1] = 0;
  region.SetSize(size);
  region.SetIndex(index);
  {
  PixelType pixel;
  pixel[0] = 0; pixel[1] = -1;
  std::cout << "pixel: " << pixel << std::endl;
  std::cout << region;
  itk::ImageRegionIterator<ImageType> it(image, region);
  it.GoToBegin();
  while (!it.IsAtEnd())
    {
    it.Set(pixel);
    ++it;
    }  
  }

  index[0] = 0;
  index[1] = width/2;
  region.SetSize(size);
  region.SetIndex(index);
  {
  PixelType pixel;
  pixel[0] = -1; pixel[1] = 0;
  std::cout << "pixel: " << pixel << std::endl;
  std::cout << region;
  itk::ImageRegionIterator<ImageType> it(image, region);
  it.GoToBegin();
  while (!it.IsAtEnd())
    {
    it.Set(pixel);
    ++it;
    }  
  }

  index[0] = width/2;
  index[1] = width/2;
  region.SetSize(size);
  region.SetIndex(index);
  {
  PixelType pixel;
  pixel[0] = 0; pixel[1] = 1;
  std::cout << "pixel: " << pixel << std::endl;
  std::cout << region;
  itk::ImageRegionIterator<ImageType> it(image, region);
  it.GoToBegin();
  while (!it.IsAtEnd())
    {
    it.Set(pixel);
    ++it;
    }  
  }

  index[0] = width/4;
  index[1] = width/4;
  size[0] = width/2;
  size[1] = width/2;
  region.SetSize(size);
  region.SetIndex(index);
  {
  PixelType pixel;
  pixel[0] = 1; pixel[1] = 1;
  pixel.Normalize();
  std::cout << "pixel: " << pixel << std::endl;
  std::cout << region;
  itk::ImageRegionIterator<ImageType> it(image, region);
  it.GoToBegin();
  while (!it.IsAtEnd())
    {
    it.Set(pixel);
    ++it;
    }  
  }

  typedef itk::VectorConnectedComponentImageFilter<
                            ImageType, 
                            OutputImageType >  VectorFilterType;

  VectorFilterType::Pointer filter = VectorFilterType::New();
  filter->SetInput( image );
  filter->SetDistanceThreshold(.01);
  typedef itk::RelabelComponentImageFilter< OutputImageType, LabelImageType > RelabelComponentType;
  RelabelComponentType::Pointer relabel = RelabelComponentType::New();
  relabel->SetInput( filter->GetOutput() );

  typedef itk::ImageFileWriter<  LabelImageType  > WriterType;
  WriterType::Pointer writer = WriterType::New();

  try
    {
    writer->SetInput (relabel->GetOutput());
    writer->SetFileName( argv[1] );
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
