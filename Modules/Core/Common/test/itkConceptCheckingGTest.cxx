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

// First include the header file to be tested:
#include "itkConceptChecking.h"
#include "itkCovariantVector.h"
#include "itkDiffusionTensor3D.h"
#include "itkIndex.h"
#include "itkNumericTraitsCovariantVectorPixel.h"
#include "itkNumericTraitsRGBPixel.h"
#include "itkNumericTraitsTensorPixel.h"
#include "itkNumericTraitsVariableLengthVectorPixel.h"
#include "itkNumericTraitsVectorPixel.h"
#include "itkOffset.h"
#include "itkRGBPixel.h"
#include "itkVariableLengthVector.h"
#include "itkVector.h"
#include <gtest/gtest.h>
#include <complex>
#include <string>
#include <vector>

namespace
{
struct Opaque
{};

template <typename TConcept>
struct MacroUser
{
  itkConceptMacro(Check, (TConcept));
};
} // namespace

using namespace itk::Concept;


TEST(ConceptChecking, ConceptMacroAcceptsSatisfiedConcept)
{
  [[maybe_unused]] constexpr MacroUser<HasNumericTraits<float>> user{};
}


TEST(ConceptChecking, HasNumericTraits)
{
  static_assert(HasNumericTraits<float>::value);
  static_assert(HasNumericTraits<bool>::value);
  static_assert(HasNumericTraits<long double>::value);
  static_assert(HasNumericTraits<std::complex<double>>::value);
  static_assert(HasNumericTraits<itk::Vector<float, 3>>::value);
  static_assert(HasNumericTraits<itk::RGBPixel<unsigned char>>::value);
  static_assert(HasNumericTraits<itk::VariableLengthVector<double>>::value);

  // Only the primary NumericTraits template applies to these.
  static_assert(!HasNumericTraits<Opaque>::value);
  static_assert(!HasNumericTraits<std::string>::value);
  static_assert(!HasNumericTraits<std::vector<float>>::value);
  // These specializations lack IsPositive and related members.
  static_assert(!HasNumericTraits<itk::CovariantVector<double, 2>>::value);
  static_assert(!HasNumericTraits<itk::DiffusionTensor3D<float>>::value);
}


TEST(ConceptChecking, Convertible)
{
  static_assert(Convertible<int, double>::value);
  static_assert(Convertible<double, std::complex<double>>::value);
  static_assert(!Convertible<itk::Vector<float, 3>, double>::value);
  static_assert(!Convertible<std::string, int>::value);
}


TEST(ConceptChecking, Comparisons)
{
  static_assert(Comparable<double, int>::value);
  static_assert(LessThanComparable<itk::Offset<2>>::value);
  static_assert(EqualityComparable<itk::Index<3>>::value);
  static_assert(!Comparable<std::complex<float>>::value);
  static_assert(!EqualityComparable<Opaque>::value);
  static_assert(!GreaterThanComparable<Opaque>::value);
}


TEST(ConceptChecking, Operators)
{
  static_assert(AdditiveOperators<double, int, float>::value);
  static_assert(!AdditiveOperators<Opaque>::value);
  static_assert(AdditiveAndAssignOperators<itk::Vector<double, 2>>::value);
  static_assert(!AdditiveAndAssignOperators<Opaque>::value);
  static_assert(MultiplyOperator<itk::Vector<float, 3>, float, itk::Vector<float, 3>>::value);
  static_assert(!MultiplyOperator<Opaque>::value);
  static_assert(MultiplyAndAssignOperator<double, itk::Vector<double, 3>>::value);
  static_assert(DivisionOperators<double>::value && DivisionAndAssignOperators<double>::value);
  static_assert(BitwiseOperators<unsigned char>::value && !BitwiseOperators<float>::value);
  static_assert(BracketOperator<itk::Vector<float, 3>, unsigned int, float>::value);
  static_assert(!BracketOperator<float, unsigned int, float>::value);
  static_assert(NotOperator<bool>::value && !NotOperator<Opaque>::value);
  static_assert(IncrementDecrementOperators<int>::value && !IncrementDecrementOperators<Opaque>::value);
  static_assert(OStreamWritable<itk::Vector<float, 3>>::value && !OStreamWritable<Opaque>::value);
}


TEST(ConceptChecking, NumericCategories)
{
  static_assert(Signed<int>::value && !Signed<unsigned short>::value);
  static_assert(IsUnsignedInteger<unsigned int>::value && !IsUnsignedInteger<int>::value);
  static_assert(IsInteger<short>::value && !IsInteger<float>::value);
  static_assert(IsNonInteger<double>::value && !IsNonInteger<char>::value);
  static_assert(IsFloatingPoint<double>::value && IsFloatingPoint<itk::Vector<float, 2>>::value);
  static_assert(IsFloatingPoint<itk::VariableLengthVector<float>>::value && !IsFloatingPoint<int>::value);
  static_assert(IsFixedPoint<itk::RGBPixel<unsigned char>>::value && !IsFixedPoint<float>::value);
}


TEST(ConceptChecking, TypeProperties)
{
  static_assert(HasPixelTraits<float>::value && HasPixelTraits<itk::RGBPixel<float>>::value);
  static_assert(!HasPixelTraits<std::complex<double>>::value && !HasPixelTraits<Opaque>::value);
  static_assert(HasValueType<itk::Vector<float, 3>>::value && !HasValueType<float>::value);
  static_assert(HasJoinTraits<char, float>::value);
  static_assert(SameType<int, int>::value && !SameType<int, long>::value);
  static_assert(SameDimension<3, 3>::value && !SameDimension<2, 3>::value);
  static_assert(SameDimensionOrMinusOne<3, 2>::value && !SameDimensionOrMinusOne<3, 1>::value);
  static_assert(SameDimensionOrMinusOneOrTwo<3, 1>::value && !SameDimensionOrMinusOneOrTwo<4, 1>::value);
}
