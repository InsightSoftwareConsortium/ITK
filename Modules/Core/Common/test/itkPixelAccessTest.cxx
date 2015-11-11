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

#include "itkImage.h"


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


int itkPixelAccessTest(int, char* [] )
{
  std::cout << "Creating an image" << std::endl;
  itk::Image<itk::Vector<unsigned short, 5>, 3>::Pointer
    o3 = itk::Image<itk::Vector<unsigned short, 5>, 3>::New();

  float origin3D[3] = { 5.0f, 2.1f, 8.1f};
  float spacing3D[3] = { 1.5f, 2.1f, 1.0f};

  itk::Image<itk::Vector<unsigned short, 5>, 3>::SizeType imageSize3D = {{ 20, 40, 60 }};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::SizeType bufferSize3D = {{ 8, 20, 14 }};

  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType startIndex3D = {{5, 4, 1}};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType bufferStartIndex3D = {{2, 3, 5}};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType regionStartIndex3D = {{5, 10, 12}};
  itk::Image<itk::Vector<unsigned short, 5>, 3>::IndexType regionEndIndex3D = {{8, 15, 17}};

  itk::Image<itk::Vector<unsigned short, 5>, 3>::RegionType region;
  region.SetSize(imageSize3D);
  region.SetIndex(startIndex3D);
  o3->SetLargestPossibleRegion( region );
  region.SetSize(bufferSize3D);
  region.SetIndex(bufferStartIndex3D);
  o3->SetBufferedRegion( region );

  o3->SetOrigin(origin3D);
  o3->SetSpacing(spacing3D);

  o3->Allocate();

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

  return EXIT_SUCCESS;
}
