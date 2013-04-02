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

#include "itkNeighborhoodIteratorTestCommon.hxx"
#include "itkShapedNeighborhoodIterator.h"

int itkShapedNeighborhoodIteratorTest(int, char* [] )
{

  TestImageType::Pointer img = GetTestImage(10, 10, 5, 3);
  itk::ShapedNeighborhoodIterator<TestImageType>::IndexType loc;
  loc[0] = 4; loc[1] = 4; loc[2] = 2; loc[3] = 1;

  // radius of the iterator
  itk::ShapedNeighborhoodIterator<TestImageType>::RadiusType radius;
  radius[0] = radius[1] = radius[2] = radius[3] = 1;

  // region over which the iterator is defined
  itk::ShapedNeighborhoodIterator<TestImageType>::RegionType reg;
  itk::ShapedNeighborhoodIterator<TestImageType>::SizeType sz;
  itk::ShapedNeighborhoodIterator<TestImageType>::IndexType idx;
  idx[0] = idx[1] = idx[2] = 0;  idx[3] = 1;
  sz[0] = sz[1] = 10; sz[2] = 5; sz[3] = 1;
  reg.SetIndex(idx); reg.SetSize(sz);

  // initialize an iterator
  println("Creating ShapedNeighborhoodIterator");
  itk::ShapedNeighborhoodIterator<TestImageType>
    it(radius, img, reg);
  it.Print(std::cout);

  println("Moving iterator using SetLocation()");
  it.SetLocation(loc);
  it.Print(std::cout);

  println("Initializing ShapedNeighborhoodIterator");

  println("Activating some offsets");
  println("...turn on [0,0,0,0], the center pixel");
  itk::ShapedNeighborhoodIterator<TestImageType>::OffsetType off;
  off[0] = 0; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  println("...turn on [1,0,0,0]");
  off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  println("...turn on [1,0,0,0] again");
  off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  println("...turn on [-1,0,0,0]");
  off[0] = -1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  println("...turn on [0,-1,0,0]");
  off[0] = 0; off[1] = -1; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  println("...turn on [0,1,0,0]");
  off[0] = 0; off[1] = 1; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  println("Testing iteration through the neighborhood.");
  itk::ShapedNeighborhoodIterator<TestImageType>::Iterator
    ci = it.Begin();

  println("Testing using IsAtEnd()");
  while (! ci.IsAtEnd())
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> "
                << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
      ci++;
    }


  println("Testing using != it.End()");
  for (ci = it.Begin(); ci != it.End(); ++ci)
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> "
                << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
    }


  println("Testing reverse iteration using != it.Begin()");
  ci = it.End();
  --ci;
  while (ci != it.Begin())
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> "
                << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
      ci--;
    }
  std::cout << ci.GetNeighborhoodIndex() << " -> "
            << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;


  println("Testing read through GetPixel(itk::Offset(0,0,0,0))");
  TestImageType::IndexType voff;
  voff[0] = 1; voff[1] = 1; voff[2] = 1; voff[3] = 1;
  off[0] = 0; off[1] = 0; off[2] = 0; off[3] = 0;
  std::cout << it.GetPixel(off) << std::endl;

  println("Testing read through GetPixel(unsigned int)");
  std::cout << it.GetPixel(it.GetNeighborhoodIndex(off)) << std::endl;

  println("Testing write through iterator dereference");
  for (ci = it.Begin(); ci != it.End(); ++ci)
    {
      ci.Set(voff);
    }
  for (ci = it.Begin(); ci != it.End(); ++ci)
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> "
                << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
    }


  println("Testing write through SetPixel(itk::Offset(0,0,0,0))");
  voff[0] = 45000;
  it.SetPixel(off, voff);
   for (ci = it.Begin(); ci != it.End(); ++ci)
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> "
                << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
    }

   println("Testing iteration through the image");
   off[0] = 0; off[1] =0; off[2] = 0; off[3] = 0;
   for (it.GoToBegin(); !it.IsAtEnd(); ++it)
     {
       std::cout << it.GetPixel(off) << std::endl;
     }

   println("Testing reverse iteration through the image");
   off[0] = 0; off[1] =0; off[2] = 0; off[3] = 0;
   for (it.GoToEnd(), --it; !it.IsAtBegin(); --it)
     {
       std::cout << it.GetPixel(off) << std::endl;
     }
   std::cout << it.GetPixel(off) << std::endl;

   println("testing operator=");
   itk::ShapedNeighborhoodIterator<TestImageType> oeIt;
   oeIt = it;

   it.Print(std::cout);
   oeIt.Print(std::cout);

  return EXIT_SUCCESS;
}
