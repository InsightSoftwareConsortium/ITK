/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodIteratorTestCommon.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNeighborhoodIteratorTestCommon_txx
#define __itkNeighborhoodIteratorTestCommon_txx

#include "itkImage.h"
#include "itkOffset.h"
#include "itkIndex.h"
#include "itkImageRegionIterator.h"
#include <iostream>

typedef itk::Image<itk::Index<4>, 4> TestImageType;
typedef itk::Offset<4> OffsetType;

namespace{
inline void println(char *s) { std::cout << s << std::endl; }
}
template<class TIteratorType>
void printnb( const TIteratorType &nb, bool full)
{
  unsigned long count = 1;
  const unsigned long sz = nb.GetRadius()[0] *2 +1;
  typename TIteratorType::ConstIterator it;
  if (full)
    {
    it = nb.Begin();

    while (it != nb.End() )
      {
      std::cout << **it << " ";
      if ( (count % sz) == 0 ) std::cout << std::endl;
      ++it;
      count ++;
      }
    }
  else
    {
    std::cout << nb.GetCenterPixel() << std::endl;
    }
}

template<unsigned int N>
void FillImage(itk::Image<itk::Index<N>,N> *img)
{
  typedef itk::Index<N> IndexType;
  typedef itk::Image<IndexType, N> ImageType;
  const itk::Size<N> size = img->GetRequestedRegion().GetSize();

  unsigned int i;
  IndexType loop;
  loop.Fill(0);
  itk::ImageRegionIterator<ImageType> it(img, img->GetRequestedRegion());
  
  while (! it.IsAtEnd() )
    {
    it.Value() = loop;
    for (i = 0; i <N; ++i)
      {
      loop[i]++;
      if ( (unsigned int)(loop[i]) == size[i] )
        {
        loop[i]= 0;
        }
      else break;
      }
    ++it;
    }
}

namespace
{
TestImageType::Pointer GetTestImage(int , int , int , int )
{
  itk::Size<4>  sizeND;
   sizeND[0] = 10;
   sizeND[1] = 10;
   sizeND[2] = 5;
   sizeND[3] = 3;
  
  itk::Index<4> origND;
   origND.Fill(0);
   
  itk::ImageRegion<4> RegionND;
   RegionND.SetSize(sizeND);
   RegionND.SetIndex(origND);

  TestImageType::Pointer imageND = TestImageType::New();
   imageND->SetLargestPossibleRegion(RegionND);
   imageND->SetBufferedRegion(RegionND);
   imageND->SetRequestedRegion(RegionND);
   imageND->Allocate();

  FillImage<4>(imageND);
   
  return  imageND;
}

}

#endif
