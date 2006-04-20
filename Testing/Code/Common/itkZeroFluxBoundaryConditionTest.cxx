/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroFluxBoundaryConditionTest.cxx
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
#include <iostream>

#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkImage.h"

void ZFBCTest_PrintNeighborhood( itk::ConstNeighborhoodIterator<itk::Image<int, 2> > &p )
{
  std::cout << std::endl;
  unsigned x, y, i=0;
  for (y = 0; y < p.GetSize()[1]; ++y)
    {
      for (x = 0; x < p.GetSize()[0]; ++x, ++i)
        {
          std::cout << p.GetPixel(i) << " ";
        }
      std::cout << std::endl;
    }
  
}

int itkZeroFluxBoundaryConditionTest(int, char* [] )
{
  typedef itk::Image<int, 2> ImageType;
  typedef itk::ConstNeighborhoodIterator<ImageType> IteratorType;
  typedef IteratorType::RadiusType RadiusType;
  
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
    }
  
  RadiusType rad;
  RadiusType radTwo;
  rad[0] = rad[1] = 1;
  IteratorType it(rad, img, img->GetRequestedRegion());

  itk::ZeroFluxNeumannBoundaryCondition<ImageType> bc;

  it.OverrideBoundaryCondition(&bc);

  pos[0] = pos[1] = 0;
  it.SetLocation(pos);

  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    ZFBCTest_PrintNeighborhood(it);
    }

  //  RadiusType radTwo;
  radTwo[0] = radTwo[1] = 2;
  IteratorType it2(radTwo, img, img->GetRequestedRegion());

  it2.OverrideBoundaryCondition(&bc);

  pos[0] = pos[1] = 0;
  it2.SetLocation(pos);
  
  for (it2.GoToBegin(); !it2.IsAtEnd(); ++it2)
    {
    ZFBCTest_PrintNeighborhood(it2);
    }
  
  return EXIT_SUCCESS;
}
