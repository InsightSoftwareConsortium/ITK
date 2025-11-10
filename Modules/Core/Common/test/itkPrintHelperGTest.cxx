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

#include "itkPrintHelper.h"
#include "itkOffset.h"
#include "gtest/gtest.h"
#include <sstream>
#include <vector>
#include <list>

TEST(PrintHelper, Vector)
{
  using namespace itk::print_helper;
  std::vector<int>   v{ 1, 2, 3, 4, 5 };
  std::ostringstream oss;
  oss << v;
  EXPECT_EQ(oss.str(), "[1, 2, 3, 4, 5]");
}

TEST(PrintHelper, EmptyVector)
{
  using namespace itk::print_helper;
  std::vector<int>   v;
  std::ostringstream oss;
  oss << v;
  EXPECT_EQ(oss.str(), "[]");
}

TEST(PrintHelper, List)
{
  using namespace itk::print_helper;
  std::list<int>     l{ 1, 2, 3, 4, 5 };
  std::ostringstream oss;
  oss << l;
  EXPECT_EQ(oss.str(), "[1, 2, 3, 4, 5]");
}

TEST(PrintHelper, EmptyList)
{
  using namespace itk::print_helper;
  std::list<int>     l;
  std::ostringstream oss;
  oss << l;
  EXPECT_EQ(oss.str(), "[]");
}

TEST(PrintHelper, CStyleArray)
{
  using namespace itk::print_helper;
  int                arr[5] = { 1, 2, 3, 4, 5 };
  std::ostringstream oss;
  oss << arr;
  EXPECT_EQ(oss.str(), "(1, 2, 3, 4, 5)");
}

TEST(PrintHelper, CStyleArraySingleElement)
{
  using namespace itk::print_helper;
  int                arr[1] = { 42 };
  std::ostringstream oss;
  oss << arr;
  EXPECT_EQ(oss.str(), "(42)");
}

TEST(PrintHelper, VectorOfOffsets)
{
  using namespace itk::print_helper;
  std::vector<itk::Offset<2>> v;
  itk::Offset<2>              o1{ { 1, 2 } };
  itk::Offset<2>              o2{ { 3, 4 } };
  v.push_back(o1);
  v.push_back(o2);
  std::ostringstream oss;
  oss << v;
  EXPECT_EQ(oss.str(), "[[1, 2], [3, 4]]");
}
