/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPeriodicBoundaryConditionTest.cxx
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

#include "itkPeriodicBoundaryCondition.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkImage.h"

void PBCTest_PrintNeighborhood( itk::ConstNeighborhoodIterator<itk::Image<int, 2> > &p )
{
  std::cout << std::endl;
  unsigned x, y, i=0;
  for (y = 0; y < p.GetSize()[1]; ++y)
    {
      for (x = 0; x < p.GetSize()[2]; ++x, ++i)
        {
          std::cout << p.GetPixel(i) << " ";
        }
      std::cout << std::endl;
    }
  
}

int itkPeriodicBoundaryConditionTest(int, char**)
{
  typedef itk::Image<int, 2> ImageType;
  typedef itk::ConstNeighborhoodIterator<ImageType> IteratorType;
  typedef IteratorType::RadiusType RadiusType;
  unsigned int i;
  
  ImageType::Pointer img = ImageType::New();
  ImageType::RegionType reg;
  ImageType::RegionType::SizeType sz = {{ 5, 5 }};
  ImageType::RegionType::IndexType idx = {{ 0, 0 }};
  reg.SetSize(sz);
  reg.SetIndex(idx);
  img->SetRegions(reg);
  img->Allocate();

  ImageType::IndexType pos;
  for (pos[1] = 0; pos[1] < 5; ++pos[1])
    {
      for (pos[0] = 0; pos[0] < 5; ++pos[0])
        {
          img->SetPixel(pos, pos[0] * 10 + pos[1]);
          std::cout << img->GetPixel(pos) << " ";
        }
      std::cout << std::endl;
    };

  RadiusType rad;
  RadiusType radTwo;
  rad[0] = rad[1] = 1;
  IteratorType it(rad, img, img->GetRequestedRegion());

  itk::PeriodicBoundaryCondition<ImageType> bc;

  it.OverrideBoundaryCondition(&bc);

  pos[0] = pos[1] = 0;
  it.SetLocation(pos);
  PBCTest_PrintNeighborhood(it);
  

  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
      PBCTest_PrintNeighborhood(it);
    }

  //  RadiusType radTwo;
  radTwo[0] = radTwo[1] = 2;
  IteratorType it2(radTwo, img, img->GetRequestedRegion());

  it2.OverrideBoundaryCondition(&bc);

  pos[0] = pos[1] = 0;
  it2.SetLocation(pos);
  PBCTest_PrintNeighborhood(it2);
  
  for (it2.GoToBegin(); !it2.IsAtEnd(); ++it2)
    {
      PBCTest_PrintNeighborhood(it2);
    }
  
  return EXIT_SUCCESS;
}
