/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnisotropicFourthOrderLevelSetImageFilterTest.cxx
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

#include "itkAnisotropicFourthOrderLevelSetImageFilter.h"
#include "itkImage.h"
#include <iostream>

int itkAnisotropicFourthOrderLevelSetImageFilterTest(int, char* [] )
{
  typedef itk::Image<float, 2> ImageType;
  typedef ImageType::IndexType IndexType;
  
  ImageType::Pointer im_init = ImageType::New();
  
  ImageType::RegionType r;
  ImageType::SizeType   sz = {{128, 128}};
  ImageType::IndexType  idx = {{0,0}};
  r.SetSize(sz);
  r.SetIndex(idx);

  im_init->SetLargestPossibleRegion(r);
  im_init->SetBufferedRegion(r);
  im_init->SetRequestedRegion(r);
  im_init->Allocate();

  IndexType index;

  for ( index[0]=0; index[0] < 128; index[0]++ )
    for ( index[1]=0; index[1] < 128; index[1]++ )
      {
      if ( (index[0]>=32) && (index[0]<=96) &&
           (index[1]>=32) && (index[1]<=96) )
        {
        im_init->SetPixel (index, static_cast<float>(-1));
        
        }
      else
        {
        im_init->SetPixel (index, static_cast<float>(1));
        }
      }
  
  typedef itk::AnisotropicFourthOrderLevelSetImageFilter<ImageType,
    ImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetMaxFilterIteration (2);
  filter->SetMaxNormalIteration(5);
  filter->SetNormalProcessConductance(0.5);
                                      
  filter->SetInput(im_init);
  filter->SetRMSChangeNormalProcessTrigger(0.1);
  std::cout<<"max iteration = "<<(filter->GetMaxFilterIteration())<<"\n";
  std::cout<<"Starting processing.\n";
  filter->Update();
  filter->Print(std::cout);
  std::cout<<"Passed.\n";
  return EXIT_SUCCESS;
}
