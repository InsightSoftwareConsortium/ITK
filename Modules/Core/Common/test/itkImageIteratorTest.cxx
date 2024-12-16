/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>

#include "itkImageIterator.h"


// This routine is used to make sure that we call the "const" version
// of GetPixel() (via the operator[])
template <typename T, unsigned int VImageDimension>
void
TestConstPixelAccess(const itk::Image<T, VImageDimension> & in, itk::Image<T, VImageDimension> & out)
{
  const typename itk::Image<T, VImageDimension>::IndexType regionStartIndex3D = { { 5, 10, 15 } };
  const typename itk::Image<T, VImageDimension>::IndexType regionEndIndex3D = { { 8, 15, 17 } };

  T vec;

  vec[0] = 5;
  vec[1] = 4;
  vec[2] = 3;
  vec[3] = 2;
  vec[4] = 1;

  out[regionStartIndex3D] = vec;
  out[regionEndIndex3D] = in[regionStartIndex3D];
}


int
itkImageIteratorTest(int, char *[])
{
  constexpr unsigned int ImageDimension = 3;

  std::cout << "Creating an image" << std::endl;
  const itk::Image<itk::Vector<unsigned short, 5>, ImageDimension>::Pointer o3 =
    itk::Image<itk::Vector<unsigned short, 5>, ImageDimension>::New();

  float origin3D[ImageDimension] = { 5.0f, 2.1f, 8.1f };
  float spacing3D[ImageDimension] = { 1.5f, 2.1f, 1.0f };

  constexpr itk::Image<itk::Vector<unsigned short, 5>, ImageDimension>::SizeType imageSize3D = { { 20, 40, 60 } };

  constexpr itk::Image<itk::Vector<unsigned short, 5>, ImageDimension>::IndexType startIndex3D = { { 5, 4, 1 } };
  constexpr itk::Image<itk::Vector<unsigned short, 5>, ImageDimension>::IndexType regionStartIndex3D = {
    { 5, 10, 12 }
  };
  constexpr itk::Image<itk::Vector<unsigned short, 5>, ImageDimension>::IndexType regionEndIndex3D = { { 8, 15, 17 } };


  const itk::Image<itk::Vector<unsigned short, 5>, ImageDimension>::RegionType region{ startIndex3D, imageSize3D };
  o3->SetRegions(region);
  o3->SetOrigin(origin3D);
  o3->SetSpacing(spacing3D);

  o3->Allocate();
  auto fillValue = itk::MakeFilled<itk::Vector<unsigned short, 5>>(itk::NumericTraits<unsigned short>::max());
  o3->FillBuffer(fillValue);

  std::cout << "Setting/Getting a pixel" << std::endl;
  itk::Vector<unsigned short, 5> vec;

  vec[0] = 5;
  vec[1] = 4;
  vec[2] = 3;
  vec[3] = 2;
  vec[4] = 1;

  (*o3)[regionStartIndex3D] = vec;
  (*o3)[regionEndIndex3D] = (*o3)[regionStartIndex3D];
  TestConstPixelAccess(*o3, *o3);

  using VectorPixelType = itk::Vector<unsigned short, 5>;
  using VectorImageType = itk::Image<VectorPixelType, ImageDimension>;

  using VectorImageIterator = itk::ImageIterator<VectorImageType>;
  using VectorImageConstIterator = itk::ImageConstIterator<VectorImageType>;

  VectorImageIterator            itr1(o3, region);
  const VectorImageConstIterator itr2(o3, region);

  // Exercise copy constructor
  const VectorImageIterator itr3(itr1);

  // Exercise assignment operator
  VectorImageIterator itr4;
  itr4 = itr1;

  // Exercise operator!=
  if (itr4 != itr1)
  {
    std::cerr << "Error in operator= or operator!=" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise operator==
  if (!(itr4 == itr1))
  {
    std::cerr << "Error in operator= or operator==" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise operator<=
  if (!(itr4 <= itr1))
  {
    std::cerr << "Error in operator= or operator<=" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise operator<
  if (itr4 < itr1)
  {
    std::cerr << "Error in operator= or operator<" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise operator>=
  if (!(itr4 >= itr1))
  {
    std::cerr << "Error in operator= or operator>=" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise operator>
  if (itr4 > itr1)
  {
    std::cerr << "Error in operator= or operator>" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise GetImageIteratorDimension()
  if (itr1.GetImageIteratorDimension() != ImageDimension)
  {
    std::cerr << "Error in GetImageIteratorDimension" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise GetIndex()
  const VectorImageType::IndexType index1 = itr1.GetIndex();
  if (index1 != startIndex3D)
  {
    std::cerr << "Error in GetIndex()" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise SetIndex()
  VectorImageType::IndexType index2 = index1;
  index2[0]++;
  VectorImageIterator itr5 = itr1;
  itr5.SetIndex(index2);
  if (itr5.GetIndex() != index2)
  {
    std::cerr << "Error in GetIndex() and/or SetIndex()" << std::endl;
    return EXIT_FAILURE;
  }

  if (itr5.GetIndex() == itr1.GetIndex())
  {
    std::cerr << "Error in GetIndex() and/or SetIndex()" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise GetRegion()
  const VectorImageType::RegionType region1 = itr1.GetRegion();
  if (region1 != region)
  {
    std::cerr << "Error in GetRegion()" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise GetImage() non-const version
  VectorImageType * image1 = itr1.GetImage();
  if (image1 != o3.GetPointer())
  {
    std::cerr << "Error in GetImage()" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise GetImage() const version
  const VectorImageType * image2 = itr2.GetImage();
  if (image2 != o3.GetPointer())
  {
    std::cerr << "Error in GetImage()" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise Get() non-const and const version
  {
    VectorPixelType vp1 = itr1.Get();
    VectorPixelType vp2 = itr2.Get();
    std::cout << "vp1: " << vp1 << std::endl;
    std::cout << "vp2: " << vp2 << std::endl;
    if (vp1 != vp2)
    {
      std::cerr << "Error in Get()" << std::endl;
      return EXIT_FAILURE;
    }
    // verify that the value can be modified
    vp1[0]++;
    itr1.Set(vp1);
    vp2 = itr2.Get();
    if (vp1 != vp2)
    {
      std::cerr << "Error in Get() and/or Set()" << std::endl;
      return EXIT_FAILURE;
    }
  }


  // Exercise Value() const and non-const methods
  {
    VectorPixelType vp1 = itr1.Value();
    VectorPixelType vp2 = itr2.Value();
    if (vp1 != vp2)
    {
      std::cerr << "Error in Value()" << std::endl;
      return EXIT_FAILURE;
    }
    // verify that the value can be modified
    vp1[0]++;
    itr1.Value() = vp1;
    vp2 = itr2.Value();
    if (vp1 != vp2)
    {
      std::cerr << "Error in Get() and/or Set()" << std::endl;
      return EXIT_FAILURE;
    }
  }

  // Exercise Begin(), GoToBegin(), IsAtBegin() and IsAtEnd()
  {
    itr1.GoToBegin();
    if (!itr1.IsAtBegin())
    {
      std::cerr << "Error in Begin() and/or IsAtBegin()" << std::endl;
      return EXIT_FAILURE;
    }
    if (itr1.IsAtEnd())
    {
      std::cerr << "Error in Begin() and/or IsAtEnd()" << std::endl;
      return EXIT_FAILURE;
    }
  }


  // Exercise End(), GoToEnd(), IsAtBegin() and IsAtEnd()
  {
    itr1.GoToEnd();
    if (!itr1.IsAtEnd())
    {
      std::cerr << "Error in End() and/or IsAtEnd()" << std::endl;
      return EXIT_FAILURE;
    }
    if (itr1.IsAtBegin())
    {
      std::cerr << "Error in End() and/or IsAtBegin()" << std::endl;
      return EXIT_FAILURE;
    }
  }


  return EXIT_SUCCESS;
}
