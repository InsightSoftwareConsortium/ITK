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

#include "itkNeighborhoodIteratorTestCommon.hxx"
#include "itkShapedNeighborhoodIterator.h"
#include "itkFlatStructuringElement.h"

int
itkShapedNeighborhoodIteratorTest(int, char *[])
{
  const TestImageType::Pointer                              img = GetTestImage(10, 10, 5, 3);
  itk::ShapedNeighborhoodIterator<TestImageType>::IndexType loc;
  loc[0] = 4;
  loc[1] = 4;
  loc[2] = 2;
  loc[3] = 1;

  // radius of the iterator
  itk::ShapedNeighborhoodIterator<TestImageType>::RadiusType radius;
  radius[0] = radius[1] = radius[2] = radius[3] = 1;

  // region over which the iterator is defined
  constexpr itk::ShapedNeighborhoodIterator<TestImageType>::SizeType  sz{ 10, 10, 5, 1 };
  constexpr itk::ShapedNeighborhoodIterator<TestImageType>::IndexType idx{ 0, 0, 0, 1 };
  itk::ShapedNeighborhoodIterator<TestImageType>::RegionType          reg = { idx, sz };

  // initialize an iterator
  std::cout << "Creating ShapedNeighborhoodIterator" << std::endl;
  itk::ShapedNeighborhoodIterator<TestImageType> it(radius, img, reg);
  it.Print(std::cout);

  std::cout << "Moving iterator using SetLocation()" << std::endl;
  it.SetLocation(loc);
  it.Print(std::cout);

  std::cout << "Initializing ShapedNeighborhoodIterator" << std::endl;

  std::cout << "Activating some offsets" << std::endl;
  std::cout << "...turn on [0,0,0,0], the center pixel" << std::endl;
  itk::ShapedNeighborhoodIterator<TestImageType>::OffsetType off;
  off[0] = 0;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  std::cout << "...turn on [1,0,0,0]" << std::endl;
  off[0] = 1;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  std::cout << "...turn on [1,0,0,0] again" << std::endl;
  off[0] = 1;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  std::cout << "...turn on [-1,0,0,0]" << std::endl;
  off[0] = -1;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  std::cout << "...turn on [0,-1,0,0]" << std::endl;
  off[0] = 0;
  off[1] = -1;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  std::cout << "...turn on [0,1,0,0]" << std::endl;
  off[0] = 0;
  off[1] = 1;
  off[2] = 0;
  off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  std::cout << "Testing iteration through the neighborhood." << std::endl;
  {
    itk::ShapedNeighborhoodIterator<TestImageType>::Iterator ci = it.Begin();
    std::cout << "Testing using IsAtEnd()" << std::endl;
    while (!ci.IsAtEnd())
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> " << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
      ++ci;
    }
  }

  std::cout << "Testing using != it.End()" << std::endl;
  for (auto ci = it.Begin(); ci != it.End(); ++ci)
  {
    std::cout << ci.GetNeighborhoodIndex() << " -> " << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
  }

  {
    std::cout << "Testing reverse iteration using != it.Begin()" << std::endl;
    auto ci = it.End();
    --ci;
    while (ci != it.Begin())
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> " << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
      --ci;
    }
    std::cout << ci.GetNeighborhoodIndex() << " -> " << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
  }

  std::cout << "Testing read through GetPixel(itk::Offset(0,0,0,0))" << std::endl;
  TestImageType::IndexType voff;
  voff[0] = 1;
  voff[1] = 1;
  voff[2] = 1;
  voff[3] = 1;
  off[0] = 0;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  std::cout << it.GetPixel(off) << std::endl;

  std::cout << "Testing read through GetPixel(unsigned int)" << std::endl;
  std::cout << it.GetPixel(it.GetNeighborhoodIndex(off)) << std::endl;

  std::cout << "Testing write through iterator dereference" << std::endl;
  for (auto ci = it.Begin(); ci != it.End(); ++ci)
  {
    ci.Set(voff);
  }
  for (auto ci = it.Begin(); ci != it.End(); ++ci)
  {
    std::cout << ci.GetNeighborhoodIndex() << " -> " << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
  }


  std::cout << "Testing write through SetPixel(itk::Offset(0,0,0,0))" << std::endl;
  voff[0] = 45000;
  it.SetPixel(off, voff);
  for (auto ci = it.Begin(); ci != it.End(); ++ci)
  {
    std::cout << ci.GetNeighborhoodIndex() << " -> " << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
  }

  std::cout << "Testing iteration through the image" << std::endl;
  off[0] = 0;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    std::cout << it.GetPixel(off) << std::endl;
  }

  std::cout << "Testing reverse iteration through the image" << std::endl;
  off[0] = 0;
  off[1] = 0;
  off[2] = 0;
  off[3] = 0;
  for (it.GoToEnd(), --it; !it.IsAtBegin(); --it)
  {
    std::cout << it.GetPixel(off) << std::endl;
  }
  std::cout << it.GetPixel(off) << std::endl;

  std::cout << "Testing interoperability with FlatStructuringElement" << std::endl;
  using StructuringElementType = itk::FlatStructuringElement<4>;
  constexpr StructuringElementType::RadiusType   radius2{ StructuringElementType::RadiusType::Filled(2) };
  const StructuringElementType                   kernel = StructuringElementType::Cross(radius2);
  itk::ShapedNeighborhoodIterator<TestImageType> iterator(radius2, img, img->GetBufferedRegion());
  iterator.CreateActiveListFromNeighborhood(kernel);
  iterator.SetLocation(loc);
  for (auto ci = iterator.Begin(); ci != iterator.End(); ++ci)
  {
    std::cout << ci.GetNeighborhoodIndex() << " -> " << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
  }

  std::cout << "testing operator=" << std::endl;
  itk::ShapedNeighborhoodIterator<TestImageType> oeIt;
  oeIt = it;

  it.Print(std::cout);
  oeIt.Print(std::cout);

  return EXIT_SUCCESS;
}
