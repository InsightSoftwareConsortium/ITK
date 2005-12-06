/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaskNeighborhoodOperatorImageFilterTest.cxx
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
#include "itkMaskNeighborhoodOperatorImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSobelOperator.h"
#include "itkImageRegionIterator.h"

int itkMaskNeighborhoodOperatorImageFilterTest(int ac, char* av[] )
{
  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage\n";
    return -1;
    }

  const unsigned int Dimension = 2;
  typedef float PixelType;
  typedef unsigned char OutputPixelType;

  typedef itk::Image<PixelType, Dimension> InputImageType;
  typedef itk::Image<OutputPixelType, Dimension> OutputImageType;
  
  itk::ImageFileReader<InputImageType>::Pointer input 
    = itk::ImageFileReader<InputImageType>::New();
  input->SetFileName(av[1]);
  input->Update();
  
  // create a mask the size of the input file
  typedef itk::Image<unsigned char, Dimension> MaskImageType;
  MaskImageType::Pointer mask1 = MaskImageType::New();
  MaskImageType::Pointer mask2 = MaskImageType::New();
  MaskImageType::RegionType region;
  MaskImageType::SizeType size;
  MaskImageType::IndexType index;

  region = input->GetOutput()->GetBufferedRegion();
  mask1->SetRegions( region );
  mask1->Allocate();
  mask1->FillBuffer(0);

  mask2->SetRegions( region );
  mask2->Allocate();
  mask2->FillBuffer(0);


  size = region.GetSize();
  index = region.GetIndex();
  unsigned int width = size[0];
  size[0] = width / 2 - static_cast<unsigned int>(.25 * static_cast<float>(width));
  index[0] = size[0] + static_cast<unsigned int>(.25 * static_cast<float>(width));
  region.SetSize(size);
  region.SetIndex(index);
  {
  itk::ImageRegionIterator<MaskImageType> it(mask1, region);
  it.GoToBegin();
  while (!it.IsAtEnd())
    {
    it.Set(1);
    ++it;
    }  
  }
  index[0] = 0;
  region.SetIndex(index);
  {
  itk::ImageRegionIterator<MaskImageType> it(mask2, region);
  it.GoToBegin();
  while (!it.IsAtEnd())
    {
    it.Set(1);
    ++it;
    }  
  }

  // Create a filter
  typedef itk::MaskNeighborhoodOperatorImageFilter<InputImageType, MaskImageType, InputImageType> FilterType;

  itk::SobelOperator<float, 2> sobelHorizontal;
  sobelHorizontal.SetDirection(0);
  sobelHorizontal.CreateDirectional();

  itk::SobelOperator<float, 2> sobelVertical;
  sobelVertical.SetDirection(1);
  sobelVertical.CreateDirectional();

  FilterType::Pointer filter1 = FilterType::New();

  filter1->SetInput(input->GetOutput());
  filter1->SetMaskImage( mask1.GetPointer() );
  filter1->SetOperator( sobelHorizontal );
  filter1->UseDefaultValueOff();

  FilterType::Pointer filter2 = FilterType::New();
  filter2->SetInput(filter1->GetOutput());
  filter2->SetMaskImage( mask2.GetPointer() );
  filter2->SetOperator( sobelVertical );
  filter2->UseDefaultValueOff();
  
  typedef itk::RescaleIntensityImageFilter< 
               InputImageType, OutputImageType > RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput( filter2->GetOutput() );

  // Generate test image
  itk::ImageFileWriter<OutputImageType>::Pointer writer =
    itk::ImageFileWriter<OutputImageType>::New();
  writer->SetInput( rescaler->GetOutput() );
  writer->SetFileName( av[2] );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return -1;
    }
  catch (...)
    {
    std::cerr << "Some other exception occurred" << std::endl;
    return -2;
    }

  return 0;
}
