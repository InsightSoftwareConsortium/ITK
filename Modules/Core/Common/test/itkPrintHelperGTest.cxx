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
#include <array>
#include <complex>
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

// Recursive case: vector<vector<T>>.  Verifies the inner operator<< is
// reachable from the outer template body via in-namespace lookup, which
// requires the manual loop to replace std::ostream_iterator (whose
// insertion happens inside namespace std and bypasses ADL into print_helper).
TEST(PrintHelper, VectorOfVector)
{
  using namespace itk::print_helper;
  std::vector<std::vector<int>> v{ { 1, 2, 3 }, { 4, 5, 6 } };
  std::ostringstream            oss;
  oss << v;
  EXPECT_EQ(oss.str(), "[[1, 2, 3], [4, 5, 6]]");
}

TEST(PrintHelper, VectorOfEmptyVector)
{
  using namespace itk::print_helper;
  std::vector<std::vector<int>> v{ {}, { 7 } };
  std::ostringstream            oss;
  oss << v;
  EXPECT_EQ(oss.str(), "[[], [7]]");
}

TEST(PrintHelper, VectorOfList)
{
  using namespace itk::print_helper;
  std::vector<std::list<int>> v{ { 1, 2 }, { 3, 4, 5 } };
  std::ostringstream          oss;
  oss << v;
  EXPECT_EQ(oss.str(), "[[1, 2], [3, 4, 5]]");
}

TEST(PrintHelper, ArrayOfVector)
{
  using namespace itk::print_helper;
  std::array<std::vector<int>, 2> a{ std::vector<int>{ 1, 2 }, std::vector<int>{ 3 } };
  std::ostringstream              oss;
  oss << a;
  EXPECT_EQ(oss.str(), "([1, 2], [3])");
}

TEST(PrintHelper, PrintNumericTraitDouble)
{
  std::ostringstream oss;
  itk::print_helper::PrintNumericTrait(oss, itk::Indent{}, "Value", 3.5);
  EXPECT_EQ(oss.str(), "Value: 3.5\n");
}

TEST(PrintHelper, PrintNumericTraitIntIsIdentityCast)
{
  // PrintType<int> == int, so the constexpr branch skips static_cast and
  // streams the value directly.
  std::ostringstream oss;
  itk::print_helper::PrintNumericTrait(oss, itk::Indent{}, "Count", 42);
  EXPECT_EQ(oss.str(), "Count: 42\n");
}

TEST(PrintHelper, PrintNumericTraitCharRendersNumerically)
{
  // PrintType<unsigned char> == int.  Without the cast the value would be
  // emitted as the ASCII character; the helper must produce the integer.
  std::ostringstream  oss;
  const unsigned char ch = 65;
  itk::print_helper::PrintNumericTrait(oss, itk::Indent{}, "Byte", ch);
  EXPECT_EQ(oss.str(), "Byte: 65\n");
}

TEST(PrintHelper, PrintNumericTraitSignedCharRendersNumerically)
{
  std::ostringstream oss;
  const signed char  sc = -7;
  itk::print_helper::PrintNumericTrait(oss, itk::Indent{}, "Offset", sc);
  EXPECT_EQ(oss.str(), "Offset: -7\n");
}

TEST(PrintHelper, PrintNumericTraitComplexIsIdentityCast)
{
  // NumericTraits<std::complex<T>>::PrintType is Self, so PrintNumericTrait
  // forwards to the value's own ostream insertion overload unchanged.
  std::ostringstream   oss;
  std::complex<double> z{ 1.0, -2.5 };
  itk::print_helper::PrintNumericTrait(oss, itk::Indent{}, "Z", z);
  EXPECT_EQ(oss.str(), "Z: (1,-2.5)\n");
}

TEST(PrintHelper, PrintNumericTraitIndentation)
{
  std::ostringstream oss;
  itk::Indent        indent{ 4 };
  itk::print_helper::PrintNumericTrait(oss, indent, "Threshold", 0.125);
  EXPECT_EQ(oss.str(), "    Threshold: 0.125\n");
}
