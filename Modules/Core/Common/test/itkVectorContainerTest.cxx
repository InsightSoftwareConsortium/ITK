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

#include "itkVectorContainer.h"
#include "itkTestingMacros.h"

using ContainerType = itk::VectorContainer<size_t, double>;

int
itkVectorContainerTest(int, char *[])
{

  auto container = ContainerType::New();
  container->InsertElement(0, 9.2);
  container->InsertElement(1, 4.7);
  container->InsertElement(2, 1.1);
  container->InsertElement(3, 4.9);

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

  return EXIT_SUCCESS;
}
