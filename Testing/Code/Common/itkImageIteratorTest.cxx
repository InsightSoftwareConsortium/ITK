/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkImageIteratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>

#include "itkImage.h"
#include "itkScalar.h"
#include "itkVector.h"
#include "itkImageRegionIterator.h"
#include "itkImageBufferIterator.h"


// This is a recursive function that iterates over a region using the
// "BasisIndex" API to move an iterator. Note the type of iterator passed
// to this routine is converted to an ImageBufferIterator.
template <typename T, unsigned int VImageDimension>
int IterateOverRegion( itk::ImageBufferIterator<T, VImageDimension> it,
                       unsigned int dim = VImageDimension-1)
{
  T value;
  int i, j;
  
  if (dim > 0)
    {
    itk::ImageBufferIterator<T, VImageDimension>::IndexType basisIndex;

    try
      {
      basisIndex = it.GetIndex().GetBasisIndex(dim);
      }
    catch (const int error)
      {
      if (error == itk::InvalidDimension)
        {
        itkGenericOutputMacro(<< "Exception: Invalid dimension");
        }
      }

    // set "it" to the beginning of the dim projection
    for (i=0; i < it.GetRegion().GetSize()[dim]; i++)
      {
      itkGenericOutputMacro(<< "Looping over " << dim);
      
      IterateOverRegion(it, dim-1);
      // increment the iterator
      try
        {
        it.Increment(basisIndex);
        }
      catch (const int error)
        {
        if (error == itk::BoundsError)
          {
          itkGenericOutputMacro(<< "Exception: Exceeding image bounds.");
          }
        }
      }
    }
  else
    {
    // final dimension... do something
    for (j=0; j < it.GetRegion().GetSize()[dim]; j++)
      {
      std::cout << "IterateOverImage(): ";
      itk::ImageIterator<T, VImageDimension>::IndexType index=it.GetIndex();
      for (int ii=0; ii < index.GetIndexDimension(); ii++)
        {
        std::cout << index[ii] << " ";
        }
      std::cout << std::endl;
      
      // set the pixel using iterator notation
      *it = value;

      // Increment the iterator
      try
        {
	// Can't use ++it since operator++ will wrap around the region.
	// Therefore, we have to use a different increment operator
        it.Increment(1);
        }
      catch (const int error)
        {
        if (error == itk::BoundsError)
          {
          itkGenericOutputMacro("Exception: Exceeding image bounds.");
          }
        }
      }
    }
  
  return 1;
}

// This is a recursive function that iterates over a region using the
// "Increment" API to move an iterator. Note the type of iterator passed
// to this routine is converted to an ImageBufferIterator.
template <typename T, unsigned int VImageDimension>
int IterateOverRegion2( itk::ImageBufferIterator<T, VImageDimension> it,
                       unsigned int dim = VImageDimension-1)
{
  T value;
  int i, j;
  
  if (dim > 0)
    {
    // set "it" to the beginning of the dim projection
    for (i=0; i < it.GetRegion().GetSize()[dim]; i++)
      {
      IterateOverRegion2(it, dim-1);
      // increment the iterator
      it.Increment(dim, 1);
      }
    }
  else
    {
    // final dimension... do something
    for (j=0; j < it.GetRegion().GetSize()[dim]; j++)
      {
      std::cout << "IterateOverRegion2(): ";
      itk::ImageIterator<T, VImageDimension>::IndexType index=it.GetIndex();
      for (int ii=0; ii < index.GetIndexDimension(); ii++)
        {
        std::cout << index[ii] << " ";
        }
      std::cout << std::endl;
      
      // set the pixel using iterator notation
      *it = value;

      // Increment the iterator.
      // Can't use ++it since operator++ will wrap around the region.
      // Therefore, we have to use a different increment operator
      it.Increment(1);
      }
    }
  
  return 1;
}


// This routine is used to make sure that we call the "const" version
// of GetPixel() (via the operator[])
template <class T, unsigned int VImageDimension>
void TestConstPixelAccess(const itk::Image<T, VImageDimension> &in,
                          itk::Image<T, VImageDimension> &out)
{
  itk::Image<T, VImageDimension>::IndexType regionStartIndex3D = {5, 10, 15};
  itk::Image<T, VImageDimension>::IndexType regionEndIndex3D = {8, 15, 17};

  T vec;
  unsigned short uvec[5] = {5, 4, 3, 2, 1};
  vec.GetVector().copy_in( uvec );
  out[regionStartIndex3D] = vec;
  out[regionEndIndex3D] = in[regionStartIndex3D];
}


int main()
{
  std::cout << "Creating an image" << std::endl;
  itk::Image<itk::Vector<unsigned short, 5>, 3>::Pointer
    o3 = itk::Image<itk::Vector<unsigned short, 5>, 3>::New();

  float origin3D[3] = { 5, 2.1, 8.1};
  float spacing3D[3] = { 1.5, 2.1, 1};

  itk::Image<itk::Vector<unsigned short, 5>, 3>::SizeType imageSize3D = { 20, 40, 60 };
  itk::Image<itk::Vector<unsigned short, 5>, 3>::SizeType bufferSize3D = { 8, 20, 14 };
  itk::Image<itk::Vector<unsigned short, 5>, 3>::SizeType regionSize3D = { 4,  6,  6 };

  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType startIndex3D = {-5, 4, -1};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType bufferStartIndex3D = {2, 3, 5};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType regionStartIndex3D = {5, 10, 12};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType regionEndIndex3D = {8, 15, 17};


  itk::Image<itk::Vector<unsigned short, 5>, 3>::RegionType region;
  region.SetSize(imageSize3D);
  region.SetIndex(startIndex3D);
  o3->SetLargestPossibleRegion( region );
  region.SetSize(bufferSize3D);
  region.SetIndex(bufferStartIndex3D);
  o3->SetBufferedRegion( region );
  region.SetSize(regionSize3D);
  region.SetIndex(regionStartIndex3D);
  o3->SetRequestedRegion( region );
  
  o3->SetOrigin(origin3D);
  o3->SetSpacing(spacing3D);

  o3->Allocate();

  std::cout << "Setting/Getting a pixel" << std::endl;
  itk::Vector<unsigned short, 5> vec;
  unsigned short uvec[5] = {5, 4, 3, 2, 1};
  vec.GetVector().copy_in(uvec);
  
  (*o3)[regionStartIndex3D] = vec;
  (*o3)[regionEndIndex3D] = (*o3)[regionStartIndex3D];
  TestConstPixelAccess(*o3, *o3);

  
  itk::ImageIterator<itk::Vector<unsigned short, 5>, 3> standardIt(o3, region);

  itk::ImageBufferIterator<itk::Vector<unsigned short, 5>, 3> bufferIt = standardIt;
  itk::ImageBufferIterator<itk::Vector<unsigned short, 5>, 3> bufferIt2(standardIt);

  // Call a recursive routine that will loop over an N-dimensional region.
  // We are forcing a conversion from a ImageIterator to a ImageBufferIterator.
  IterateOverRegion<itk::Vector<unsigned short, 5>, 3>( standardIt );

  // Call a recursive routine that will loop over an N-dimensional region.
  // We are forcing a conversion from a ImageIterator to a ImageBufferIterator.
  IterateOverRegion2<itk::Vector<unsigned short, 5>, 3>( standardIt );
  
  // Iterate over a region using a simple for loop
  itk::ImageRegionIterator<itk::Vector<unsigned short, 5>, 3> it(o3, region);

  for ( ; !it.IsAtEnd(); ++it)
    {
    itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType index = it.GetIndex();
    std::cout << "Simple iterator loop: ";
    for (int i=0; i < index.GetIndexDimension(); i++)
      {
      std::cout << index[i] << " ";
      }
    std::cout << std::endl;
    }

  return 0;
}



