/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChangeInformationImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>
#include "itkChangeInformationImageFilter.h"
#include "itkImage.h"

const unsigned int ImageDimension = 3;
typedef itk::Image<float, ImageDimension>  ImageType;
typedef ImageType::Pointer ImagePointer;

void PrintInformation(ImagePointer image1, ImagePointer image2)
{
  unsigned int i;
  std::cout << "Input  " << "      Output" << std::endl;
  std::cout << "Origin"  << "      Origin"  << std::endl;
  for (i = 0; i < ImageDimension; i++)
    {
    std::cout << "  " << image1->GetOrigin()[i] << "       " << image2->GetOrigin()[i] << std::endl;
    }
  std::cout << "Spacing"  << "      Spacing"  << std::endl;
  for (i = 0; i < ImageDimension; i++)
    {
    std::cout << "    " << image1->GetSpacing()[i] << "        " << image2->GetSpacing()[i] << std::endl;
    }
}

void PrintInformation3(ImagePointer image1, ImagePointer image2, ImagePointer image3)
{
  unsigned int i;
  std::cout << "Input  " << "      Output" << "      Reference" << std::endl;
  std::cout << "Origin"  << "      Origin"  << "      Origin"  << std::endl;
  for (i = 0; i < ImageDimension; i++)
    {
    std::cout << "  " << image1->GetOrigin()[i] << "       " << image2->GetOrigin()[i] << "       " << image3->GetOrigin()[i] << std::endl;
    }
  std::cout << "Spacing"  << "      Spacing"  << "      Spacing"  << std::endl;
  for (i = 0; i < ImageDimension; i++)
    {
    std::cout << "    " << image1->GetSpacing()[i] << "        " << image2->GetSpacing()[i] << "        " << image3->GetSpacing()[i] << std::endl;
    }
}

int itkChangeInformationImageFilterTest(int, char* [] )
{


  typedef itk::Image<float, ImageDimension>  ImageType;
  typedef itk::ChangeInformationImageFilter<ImageType> FilterType;
  
  ImageType::Pointer inputImage  = ImageType::New();
  ImageType::Pointer referenceImage  = ImageType::New();
  FilterType::Pointer filter = FilterType::New();
  
  double spacing[ImageDimension] = {1, 2, 3};
  double origin[ImageDimension] = {-100, -200, -300};
  
  typedef itk::ImageRegion<ImageDimension> RegionType;
  typedef itk::Size<ImageDimension> SizeType;
  typedef itk::ImageRegion<ImageDimension> RegionType;
  
  SizeType size; size.Fill(20);

  inputImage->SetRegions(size);
  inputImage->Allocate();

  double referenceOrigin[ImageDimension] = {-1000, -2000, -3000};
  double referenceSpacing[ImageDimension] = {1000, 2000, 3000};
  referenceImage->SetOrigin(referenceOrigin);
  referenceImage->SetSpacing(referenceSpacing);  

  referenceImage->SetRegions(size);
  referenceImage->Allocate();

  inputImage->SetSpacing (spacing);
  inputImage->SetOrigin (origin);

  double newOrigin[ImageDimension] = {1000, 2000, 3000};
  double newSpacing[ImageDimension] = {10, 20, 30};
  long newOffset[ImageDimension] = {10, 20, 30};
  
  filter->SetInput (inputImage);
  filter->SetOutputSpacing (newSpacing);
  filter->SetOutputOrigin (newOrigin);
  filter->SetOutputOffset (newOffset);
  filter->SetReferenceImage (referenceImage);
  
  // Catch any exceptions
  try
    {
    std::cout << "-----------filter: " << filter << std::endl;
    filter->Update();
    std::cout << "-----------Default behavior: "<< std::endl;;
    PrintInformation (inputImage, filter->GetOutput());
  
    filter->ChangeAll();
    filter->ChangeRegionOff();
    filter->Update();
    std::cout << "-----------ChangeAll(), ChangeRegionOff(): " << std::endl;;
    PrintInformation (inputImage, filter->GetOutput());
  
    filter->CenterImageOn();
    filter->Update();
    std::cout << "-----------CenterImageOn(): " << std::endl;;
    PrintInformation (inputImage, filter->GetOutput());
  
    filter->CenterImageOn();
    filter->ChangeSpacingOff();
    filter->Update();
    std::cout << "-----------CenterImageOn(), ChangeSpacingOff(): " << std::endl;;
    PrintInformation (inputImage, filter->GetOutput());
  
    filter->CenterImageOn();
    filter->ChangeSpacingOn();
    filter->ChangeOriginOff();
    filter->Update();
    std::cout << "-----------CenterImageOn(), ChangeOriginOff(): " << std::endl;;
    PrintInformation (inputImage, filter->GetOutput());
    
    filter->CenterImageOff();
    filter->ChangeNone();
    filter->Update();
    std::cout << "-----------ChangeNone(): " << std::endl;;
    PrintInformation (inputImage, filter->GetOutput());

    filter->CenterImageOff();
    filter->UseReferenceImageOn();
    filter->Update();
    std::cout << "-----------ChangeNone(), UseReferenceOn(): " << std::endl;;
    PrintInformation3 (inputImage, filter->GetOutput(), referenceImage);

    filter->ChangeOriginOn();
    filter->Update();
    std::cout << "-----------ChangeOriginOn(), UseReferenceOn(): " << std::endl;;
    PrintInformation3 (inputImage, filter->GetOutput(), referenceImage);

    filter->ChangeOriginOff();
    filter->ChangeSpacingOn();
    filter->Update();
    std::cout << "-----------ChangeSpacingOn(), UseReferenceOn(): " << std::endl;;
    PrintInformation3 (inputImage, filter->GetOutput(), referenceImage);

    filter->ChangeAll();
    filter->UpdateLargestPossibleRegion();
    std::cout << "-----------ChangeAll(), UseReferenceOn(): " << std::endl;;
    PrintInformation3 (inputImage, filter->GetOutput(), referenceImage);

    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e;
    return -1;
    }
  return 0;
}

