/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkRandomAccessNeighborhoodIteratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImage.h"
#include "itkOffset.h"
#include "itkIndex.h"
#include "itkPixelTraits.h"
#include "itkRandomAccessNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include <iostream>

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

template<unsigned int N>
void printnb( const
    itk::RandomAccessNeighborhoodIterator< itk::Image<itk::Index<N>, N> > &nb)
{
  typedef itk::RandomAccessNeighborhoodIterator< itk::Image<itk::Index<N>, N> > 
    IteratorType;

  typename IteratorType::ConstIterator it;
  it = nb.begin();

  //  while (it != nb.end() )
  //    {
  //      std::cout << **it << std::endl;
  //      ++it;
  //    }
  
    std::cout << nb.Center() << std::endl;
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
          if ( loop[i] == size[i] )
            {
              loop[i]= 0;
            }
          else break;
        }
      ++it;
    }
}

int main()
{
  typedef itk::Image<itk::Index<4>, 4> ImageType;
  typedef itk::Offset<4> OffsetType;

  println("Creating image of indicies");
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

  ImageType::Pointer imageND = ImageType::New();
   imageND->SetLargestPossibleRegion(RegionND);
   imageND->SetBufferedRegion(RegionND);
   imageND->SetRequestedRegion(RegionND);
   imageND->Allocate();

  FillImage<4>(imageND);

  println("Looking at first few image values...");
  std::cout << "SIZE = " << imageND->GetRequestedRegion().GetSize()<< std::endl;
  itk::ImageRegionIterator<ImageType> it(imageND, imageND->GetRequestedRegion());
  it.Begin();
  for (int slc = 0; slc <20; ++slc, ++it)
    {    std::cout << it.Value() << std::endl;    }

  println("Setting up a random access neighborhood iterator");
  itk::Size<4> szN;
   szN[0] = 0;
   szN[1] = 0;
   szN[2] = 0;
   szN[3] = 1;

  itk::RandomAccessNeighborhoodIterator<ImageType> rniND(szN, imageND,
                                         imageND->GetRequestedRegion());

  rniND.Print(std::cout);

  println("Testing random access");
  rniND.Begin();
  printnb<4>(rniND);

  println("Adding [1, 1, 1, 1]");
  OffsetType a_off;
  a_off.Fill(1);
  rniND += a_off;
  printnb<4>(rniND);

  println("Subtracting [1, 1, 1, 1]");
  rniND -= a_off;
  printnb<4>(rniND);

  println("Adding [0 0 0 2]");
  a_off.Fill(0);
  a_off[3] = 2;
  rniND += a_off;
  printnb<4>(rniND);

  println("Adding [0 8 0 0]");
  a_off.Fill(0);
  a_off[1] = 8;
  rniND += a_off;
  printnb<4>(rniND);

  println("Adding [5 -3 2 -1]");
  a_off[0] = 5;
  a_off[1] = -3;
  a_off[2] = 2;
  a_off[3] = -1;
  rniND += a_off;
  printnb<4>(rniND);

  return 0;
}
