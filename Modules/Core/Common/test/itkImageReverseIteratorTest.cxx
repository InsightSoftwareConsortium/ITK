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

#include <iostream>

#include "itkImageRegionReverseIterator.h"
#include "itkImageRegionReverseConstIterator.h"


// This routine is used to make sure that we call the "const" version
// of GetPixel() (via the operator[])
template <typename T, unsigned int VImageDimension>
void TestConstPixelAccess(const itk::Image<T, VImageDimension> &in,
                          itk::Image<T, VImageDimension> &out)
{
  typename itk::Image<T, VImageDimension>::IndexType regionStartIndex3D = {{5, 10, 15}};
  typename itk::Image<T, VImageDimension>::IndexType regionEndIndex3D = {{8, 15, 17}};

  T vec;

  vec[0] = 5;
  vec[1] = 4;
  vec[2] = 3;
  vec[3] = 2;
  vec[4] = 1;

  out[regionStartIndex3D] = vec;
  out[regionEndIndex3D] = in[regionStartIndex3D];
}


int itkImageReverseIteratorTest(int, char* [] )
{

  typedef itk::Vector< unsigned short, 5 >   PixelType;
  const unsigned int Dimension = 3;

  typedef itk::Image< PixelType, Dimension > ImageType;

  std::cout << "Creating an image" << std::endl;
  ImageType::Pointer o3 = ImageType::New();

  float origin3D[3] = { 5.0f, 2.1f, 8.1f};
  float spacing3D[3] = { 1.5f, 2.1f, 1.0f};

  ImageType::SizeType imageSize3D = {{ 20, 40, 60 }};
  ImageType::SizeType bufferSize3D = {{ 8, 20, 14 }};
  ImageType::SizeType regionSize3D = {{ 4,  6,  6 }};

  ImageType::IndexType startIndex3D = {{5, 4, 1}};
  ImageType::IndexType bufferStartIndex3D = {{2, 3, 5}};
  ImageType::IndexType regionStartIndex3D = {{5, 10, 12}};
  ImageType::IndexType regionEndIndex3D = {{8, 15, 17}};


  ImageType::RegionType region;
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
  PixelType vec;

  vec[0] = 5;
  vec[1] = 4;
  vec[2] = 3;
  vec[3] = 2;
  vec[4] = 1;

  (*o3)[regionStartIndex3D] = vec;
  (*o3)[regionEndIndex3D] = (*o3)[regionStartIndex3D];
  TestConstPixelAccess(*o3, *o3);


  itk::ImageIterator< ImageType > standardIt(o3, region);

  // Iterate over a region using a simple for loop
  itk::ImageRegionIterator< ImageType > it(o3, region);

  for (; !it.IsAtEnd(); ++it)
    {
    ImageType::IndexType index = it.GetIndex();
    std::cout << "Simple iterator loop: ";
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      std::cout << index[i] << " ";
      }
    std::cout << std::endl;
    }

  // Iterator over the region backwards using a simple for loop
  itk::ImageRegionIterator< ImageType > backIt(o3, region);
  backIt.GoToEnd(); // one pixel past the end of the region
  do
    {
    --backIt;

    ImageType::IndexType index = backIt.GetIndex();
    std::cout << "Simple iterator backwards loop: ";
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      std::cout << index[i] << " ";
      }
    std::cout << std::endl;
    }
  while (!backIt.IsAtBegin()); // stop when we reach the beginning


  // Iterate over a region backwards using a reverse iterator
  itk::ImageRegionReverseIterator< ImageType > reverseIt(o3, region);

  for (; !reverseIt.IsAtEnd(); ++reverseIt)
    {
    ImageType::IndexType index = reverseIt.GetIndex();
    std::cout << "Reverse iterator: ";
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      std::cout << index[i] << " ";
      }
    std::cout << std::endl;
    }

  // Iterator over the region forwards using a reverse iterator
  itk::ImageRegionReverseIterator< ImageType > backReverseIt(o3, region);
  backReverseIt.GoToEnd(); // one pixel before the region
  do
    {
    --backReverseIt;

    // vanal exercise of Value(), Set() and Get();

    PixelType pixelValue = backReverseIt.Get();
    backReverseIt.Set( pixelValue );
    pixelValue = backReverseIt.Value();

    ImageType::IndexType index = backReverseIt.GetIndex();
    std::cout << "Reverse iterator backwards loop: ";
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      std::cout << index[i] << " ";
      }
    std::cout << std::endl;
    }
  while (!backReverseIt.IsAtBegin()); // stop when we reach the beginning

  // Iterate over a region backwards using a reverse const iterator
  itk::ImageRegionReverseConstIterator< ImageType > reverseConstIt(o3, region);
  for (reverseConstIt.GoToBegin(); !reverseConstIt.IsAtEnd(); ++reverseConstIt)
    {
    ImageType::IndexType index = reverseConstIt.GetIndex();
    std::cout << "Reverse const iterator: ";
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      std::cout << index[i] << " ";
      }
    std::cout << std::endl;
    }

  // Finally, create a ReverseIterator from an Iterator and walk each appropriately so that they match
  it.GoToBegin();
  itk::ImageRegionReverseIterator< ImageType > castBackReverseIt(it);
  castBackReverseIt.GoToEnd();
  int status = 0;
  std::cout << "It and Reverse check: ";
  for (; !it.IsAtEnd(); ++it)
    {
    --castBackReverseIt;
    itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType index = it.GetIndex();
    itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType rindex = castBackReverseIt.GetIndex();
    for (unsigned int i=0; i < index.GetIndexDimension(); i++)
      {
      if (index[i] != rindex[i])
        {
        std::cout << index[i] << " != " << rindex[i] << std::endl;
        status = EXIT_FAILURE;
        }
      }
    }
  if (status == EXIT_SUCCESS)
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    }
  return status;
}
