/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodIteratorTestCommon.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkNeighborhoodIteratorTestCommon_txx
#define __itkNeighborhoodIteratorTestCommon_txx

#include "itkImage.h"
#include "itkOffset.h"
#include "itkIndex.h"
#include "itkPixelTraits.h"
#include "itkImageRegionIterator.h"
#include <iostream>

typedef itk::Image<itk::Index<4>, 4> TestImageType;
typedef itk::Offset<4> OffsetType;


namespace itk {
  
template <>
class ScalarTraits< Index<4> >
{
public:
  // This useless class is necessary because itk expects all image pixels
  // to have ScalarTraits defined.  The default definition is not compatible
  // with itk::Index.
  typedef Index<4> ValueType;
  typedef Index<4> ScalarValueType;
};
  
template <>
class VectorTraits< Index<4> >
{
public:
  // This useless class is necessary because itk expects all image pixels
  // to have VectorTraits defined.  The default definition is not compatible
  // with itk::Index.
  typedef Index<4> ValueType;
  typedef Index<4> ScalarValueType;
};
  
}  // end namespace itk

inline void println(char *s) { std::cout << s << std::endl; }

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
  
  it.Begin();
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



TestImageType::Pointer GetTestImage(int x, int y, int z, int a)
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



#endif
