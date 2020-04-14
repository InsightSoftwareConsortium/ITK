/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkRLEImage.h"
#include <iostream>


// This routine is used to make sure that we call the "const" version
// of GetPixel() (via the operator[])
template <typename T>
void
TestConstPixelAccess(const itk::RLEImage<T> & in, itk::RLEImage<T> & out)
{
  typename itk::RLEImage<T>::IndexType regionStartIndex3D = { { 5, 10, 15 } };
  typename itk::RLEImage<T>::IndexType regionEndIndex3D = { { 8, 15, 17 } };

  T vec;

  vec[0] = 5;
  vec[1] = 4;
  vec[2] = 3;
  vec[3] = 2;
  vec[4] = 1;

  out.SetPixel(regionStartIndex3D, vec);
  out.SetPixel(regionEndIndex3D, in[regionStartIndex3D]);
}


int
itkRLEImageIteratorTest(int, char *[])
{
  constexpr unsigned int ImageDimension = 3;

  std::cout << "Creating an image" << std::endl;
  itk::RLEImage<itk::Vector<unsigned short, 5>>::Pointer o3 = itk::RLEImage<itk::Vector<unsigned short, 5>>::New();

  float origin3D[ImageDimension] = { 5.f, 2.1f, 8.1f };
  float spacing3D[ImageDimension] = { 1.5f, 2.1f, 1.f };

  itk::RLEImage<itk::Vector<unsigned short, 5>>::SizeType imageSize3D = { { 20, 40, 60 } };

  itk::RLEImage<itk::Vector<unsigned short, 5>>::IndexType startIndex3D = { { 5, 4, 1 } };
  itk::RLEImage<itk::Vector<unsigned short, 5>>::IndexType regionStartIndex3D = { { 6, 10, 12 } };
  itk::RLEImage<itk::Vector<unsigned short, 5>>::IndexType regionEndIndex3D = { { 8, 15, 17 } };


  itk::RLEImage<itk::Vector<unsigned short, 5>>::RegionType region;
  region.SetSize(imageSize3D);
  region.SetIndex(startIndex3D);
  o3->SetRegions(region);
  o3->SetOrigin(origin3D);
  o3->SetSpacing(spacing3D);

  o3->Allocate();
  itk::Vector<unsigned short, 5> fillValue;
  fillValue.Fill(itk::NumericTraits<unsigned short>::max());
  o3->FillBuffer(fillValue);

  std::cout << "Setting/Getting a pixel" << std::endl;
  itk::Vector<unsigned short, 5> vec;

  vec[0] = 5;
  vec[1] = 4;
  vec[2] = 3;
  vec[3] = 2;
  vec[4] = 1;

  (*o3).SetPixel(regionStartIndex3D, vec);
  (*o3).SetPixel(regionEndIndex3D, (*o3)[regionStartIndex3D]);
  TestConstPixelAccess(*o3, *o3);

  using VectorPixelType = itk::Vector<unsigned short, 5>;
  using VectorImageType = itk::RLEImage<VectorPixelType>;

  using VectorImageIterator = itk::ImageIterator<VectorImageType>;
  using VectorImageConstIterator = itk::ImageConstIterator<VectorImageType>;

  VectorImageIterator      itr1(o3, region);
  VectorImageConstIterator itr2(o3, region);

  // Exercise copy constructor
  VectorImageIterator itr3(itr1);

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
  VectorImageType::IndexType index1 = itr1.GetIndex();
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
  VectorImageType::RegionType region1 = itr1.GetRegion();
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
    itr2 = itr1; // we need to do this because Set invalidates other itarators
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
    itr1.Set(vp1);
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
