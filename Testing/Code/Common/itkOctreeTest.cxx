/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOctreeTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkOctree.h"
#include "itkImage.h"
#include "itkExceptionObject.h"
#include "itkNumericTraits.h"
#include "itkImageRegionIterator.h"
#include "stdlib.h"
//#include "cstdio"
#include <time.h>
#include <math.h>

template <class TPixel,unsigned int TableSize>
class IdentityMap
{
public:
unsigned int Evaluate(const TPixel *pixel) 
{ 
  unsigned int pixval = static_cast<unsigned int>(*pixel); 
  return pixval < TableSize ? pixval : TableSize - 1;
}
};

int itkOctreeTest(int, char *[])
{
   typedef itk::Image<unsigned int,3> ImageType;
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
  std::srand( (unsigned)time( NULL) );
  itk::ImageRegionIterator<ImageType> ri(img,region);
  try
    {
    unsigned int counter = 0;
    while(!ri.IsAtEnd())
      {
      unsigned int val = std::rand() % 16384;
      if(counter & counter % 8 == 0)
        std::cerr << val << std::endl;
      else
        std::cerr << val << " ";
      counter++;
      ri.Set(val);
      ++ri;
      }
    }
  catch(itk::ExceptionObject & ex)
    {
    ex.Print(std::cerr);
    return -1;
    }
  
  typedef itk::Octree<unsigned int,16384,IdentityMap<unsigned int,16384> > OctreeType;
  OctreeType::Pointer octree = OctreeType::New();
  octree->BuildFromImage(img);
  ImageType::Pointer output = octree->GetImage();
  itk::ImageRegionIterator<ImageType> ri2(output,region);
  ri.GoToBegin();
  IdentityMap<unsigned int,16384> id;
  try
    {
    while(!ri.IsAtEnd() && !ri2.IsAtEnd())
      {
      unsigned int x = ri.Get();
      unsigned int y = ri2.Get();
      unsigned int mapped = id.Evaluate(&x);
      std::cerr << "x = " << 
        x << " mapped(x) " << mapped << " y = " << y << std::endl;
      if(mapped != y)
        {
        std::cerr << "Error comparing Input and Output of Octree" << std::endl;
        return -1;
        }
      ++ri;
      ++ri2;
      }
    if(!ri.IsAtEnd() || !ri2.IsAtEnd())
      {
      std::cerr << "Error, inconsistent image sizes in Octree" << std::endl;
      return -1;
      }
    }
  catch(itk::ExceptionObject & ex)
    {
    ex.Print(std::cerr);
    return -1;
    }
  return 0;

}
