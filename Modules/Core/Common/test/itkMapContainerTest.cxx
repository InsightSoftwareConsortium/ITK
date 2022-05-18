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

#include "itkTestingMacros.h"
#include "itkMapContainer.h"
#include "itkPoint.h"

#include <iostream>

/**
 * Some type alias to make things easier.
 */
using PointType = itk::Point<float, 3>;
using VectorType = itk::Vector<float, 3>;

using ContainerType = itk::MapContainer<unsigned long, PointType>;
using ContainerPointer = ContainerType::Pointer;

int
itkMapContainerTest(int, char *[])
{

  /**
   * Create the Container
   */
  ContainerPointer container = ContainerType::New();

  PointType pointA;
  PointType pointB;
  PointType pointC;
  PointType pointD;

  VectorType displacement;

  displacement[0] = 2;
  displacement[1] = 5;
  displacement[2] = 9;

  pointA.Fill(0.0);
  pointB = pointA + displacement;
  pointC = pointB + displacement;
  pointD = pointC + displacement;

  container->SetElement(0, pointA);
  container->SetElement(1, pointB);
  container->SetElement(2, pointC);
  container->SetElement(3, pointD);

  // Iterator
  {
    ContainerType::Iterator p_null;
    ContainerType::Iterator p = container->Begin();
    ContainerType::Iterator p_copy(p);
    ContainerType::Iterator p_assign = p;

    while (p != container->End())
    {
      ITK_TEST_EXPECT_EQUAL(p.Value(), p_copy.Value());
      ITK_TEST_EXPECT_EQUAL(p.Value(), p_assign.Value());

      ++p;
      ++p_copy;
      ++p_assign;
    }
  }

  // ConstIterator
  {
    ContainerType::ConstIterator p_null;
    ContainerType::ConstIterator p = container->Begin();
    ContainerType::ConstIterator p_copy(p);
    ContainerType::ConstIterator p_assign = p;

    while (p != container->End())
    {
      ITK_TEST_EXPECT_EQUAL(p.Value(), p_copy.Value());
      ITK_TEST_EXPECT_EQUAL(p.Value(), p_assign.Value());

      ++p;
      ++p_copy;
      ++p_assign;
    }
  }

  container->Initialize();
  if (container->Size() != 0)
  {
    std::cerr << "Initialize() didn't get rid of elements" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
