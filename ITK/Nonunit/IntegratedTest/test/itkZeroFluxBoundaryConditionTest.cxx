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
#include <iostream>

#include "itkConstShapedNeighborhoodIterator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkImage.h"

void ZFBCTest_PrintNeighborhood( itk::ConstShapedNeighborhoodIterator<itk::Image<int, 2> > &p )
{
  itk::ConstShapedNeighborhoodIterator<itk::Image<int, 2> >::ConstIterator
    ci = p.Begin();
  while (!ci.IsAtEnd())
    {
    std::cout << "    "
              << ci.GetNeighborhoodIndex() << " -> "
              << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
    ci++;
    }
}

int itkZeroFluxBoundaryConditionTest(int, char* [] )
{
  typedef itk::Image<int, 2> ImageType;
  typedef itk::ConstShapedNeighborhoodIterator<ImageType> IteratorType;
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

  itk::ConstShapedNeighborhoodIterator<ImageType>::OffsetType off;
  off[0] = -1; off[1] = -1;
  it.ActivateOffset(off);
  off[0] = 0; off[1] = 0;
  it.ActivateOffset(off);
  off[0] = 1; off[1] = 1;
  it.ActivateOffset(off);

  pos[0] = pos[1] = 0;
  it.SetLocation(pos);

  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    std::cout << "Index: " << it.GetIndex() << std::endl;
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
