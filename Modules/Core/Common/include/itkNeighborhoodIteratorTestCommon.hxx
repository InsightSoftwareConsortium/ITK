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
#ifndef itkNeighborhoodIteratorTestCommon_hxx
#define itkNeighborhoodIteratorTestCommon_hxx

#include "itkImage.h"
#include "itkIndex.h"
#include "itkImageRegionIterator.h"
#include <iostream>

typedef itk::Image<itk::Index<4>, 4> TestImageType;
typedef itk::Offset<4>               OffsetType;

extern void println(const char *s);
extern TestImageType::Pointer GetTestImage(int , int , int , int );


template<typename TIteratorType>
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
  typedef itk::Index<N>            IndexType;
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


#endif
