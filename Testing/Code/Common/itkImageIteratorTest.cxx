/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkImageIteratorTest.cxx
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
#include <iostream>

#include "itkPhysicalImage.h"
#include "itkScalar.h"
#include "itkVector.h"
#include "itkImageRegionIterator.h"
#include "itkImageBufferIterator.h"


// This is a recursive function that iterates over a region using the
// "BasisIndex" API to move an iterator. Note the type of iterator passed
// to this routine is converted to an ImageBufferIterator.
template <typename T, unsigned int VImageDimension>
int IterateOverRegion( itk::ImageBufferIterator<itk::PhysicalImage<T, VImageDimension> > it,
                       unsigned int dim = VImageDimension-1)
{
  T value;
  unsigned int i, j;
  
  if (dim > 0)
    {
    itk::ImageBufferIterator<itk::PhysicalImage<T, VImageDimension> >::IndexType basisIndex;

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
      itk::ImageIterator<itk::PhysicalImage<T, VImageDimension> >::IndexType index=it.GetIndex();
      for (unsigned int ii=0; ii < index.GetIndexDimension(); ii++)
        {
        std::cout << index[ii] << " ";
        }
      std::cout << std::endl;
      
      // set the pixel using iterator notation
      it.Set( value );

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
int IterateOverRegion2( itk::ImageBufferIterator<itk::PhysicalImage<T, VImageDimension> > it,
                       unsigned int dim = VImageDimension-1)
{
  T value;
  unsigned int i, j;
  
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
      itk::ImageIterator<itk::PhysicalImage<T, VImageDimension> >::IndexType index=it.GetIndex();
      for (unsigned int ii=0; ii < index.GetIndexDimension(); ii++)
        {
        std::cout << index[ii] << " ";
        }
      std::cout << std::endl;
      
      // set the pixel using iterator notation
      it.Set( value );

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
  itk::Image<T, VImageDimension>::IndexType regionStartIndex3D = {{5, 10, 15}};
  itk::Image<T, VImageDimension>::IndexType regionEndIndex3D = {{8, 15, 17}};

  T vec;
  // requires type T to support comma-separated-list assignments.
  vec = 5,4,3,2,1;
  out[regionStartIndex3D] = vec;
  out[regionEndIndex3D] = in[regionStartIndex3D];
}


int main()
{
  std::cout << "Creating an image" << std::endl;
  itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3>::Pointer
    o3 = itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3>::New();

  float origin3D[3] = { 5, 2.1, 8.1};
  float spacing3D[3] = { 1.5, 2.1, 1};

  itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3>::SizeType imageSize3D = {{ 20, 40, 60 }};
  itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3>::SizeType bufferSize3D = {{ 8, 20, 14 }};
  itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3>::SizeType regionSize3D = {{ 4,  6,  6 }};

  itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3>::IndexType startIndex3D = {{5, 4, 1}};
  itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3>::IndexType bufferStartIndex3D = {{2, 3, 5}};
  itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3>::IndexType regionStartIndex3D = {{5, 10, 12}};
  itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3>::IndexType regionEndIndex3D = {{8, 15, 17}};


  itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3>::RegionType region;
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
  vec = 5,4,3,2,1;
  
  (*o3)[regionStartIndex3D] = vec;
  (*o3)[regionEndIndex3D] = (*o3)[regionStartIndex3D];
  TestConstPixelAccess(*o3, *o3);

  
  itk::ImageIterator<itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3> > standardIt(o3, region);

  itk::ImageBufferIterator<itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3> > bufferIt = standardIt;
  itk::ImageBufferIterator<itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3> > bufferIt2(standardIt);

  // Call a recursive routine that will loop over an N-dimensional region.
  // We are forcing a conversion from a ImageIterator to a ImageBufferIterator.
  IterateOverRegion<itk::Vector<unsigned short, 5>, 3>( standardIt );

  // Call a recursive routine that will loop over an N-dimensional region.
  // We are forcing a conversion from a ImageIterator to a ImageBufferIterator.
  IterateOverRegion2<itk::Vector<unsigned short, 5>, 3>( standardIt );
  
  // Iterate over a region using a simple for loop
  itk::ImageRegionIterator<itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3> > it(o3, region);

  for ( ; !it.IsAtEnd(); ++it)
    {
    itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3>::IndexType index = it.GetIndex();
    std::cout << "Simple iterator loop: ";
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      std::cout << index[i] << " ";
      }
    std::cout << std::endl;
    }

  // Iterator over the region backwards using a simple for loop
  itk::ImageRegionIterator<itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3> > backIt(o3, region);
 
  backIt = backIt.End(); // one pixel past the end of the region
  do
    {
    --backIt;

    itk::PhysicalImage<itk::Vector<unsigned short, 5>, 3>::IndexType index = backIt.GetIndex();
    std::cout << "Simple iterator backwards loop: ";
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      std::cout << index[i] << " ";
      }
    std::cout << std::endl;    
    }
  while (!backIt.IsAtBegin()); // stop when we reach the beginning
  

  return 0;
}



