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
#include <cstddef>

// Better name demanging for gcc
#if __GNUC__ > 3 || ( __GNUC__ == 3 && __GNUC_MINOR__ > 0 )
#ifndef __EMSCRIPTEN__
#define GCC_USEDEMANGLE
#endif
#endif

#ifdef GCC_USEDEMANGLE
#include <cstdlib>
#include <cxxabi.h>
#endif

#include "itkNumericTraitsArrayPixel.h"
#include "itkNumericTraitsRGBAPixel.h"
#include "itkNumericTraitsRGBPixel.h"
#include "itkNumericTraitsTensorPixel.h"
#include "itkNumericTraitsVariableLengthVectorPixel.h"

namespace
{

struct UnknownTypeTestCase
{
};

struct ForcedFailureTestCase
{
};

}


//test implementation of NumericTraits designed to fail
namespace itk
{

template<>
class NumericTraits< ForcedFailureTestCase > : public std::numeric_limits< ForcedFailureTestCase >
{
  public:
  typedef ForcedFailureTestCase ValueType;
  static ITK_CONSTEXPR_VAR bool IsSigned = true;    //the default (for unknown classes) in std::numeric_limits is false, false.
  static ITK_CONSTEXPR_VAR bool IsInteger = true;   //so this should not match and the test should fail.
};

template<>
class NumericTraits< std::complex< ForcedFailureTestCase > >
{
  public:
  typedef ForcedFailureTestCase ValueType;
  static ITK_CONSTEXPR_VAR bool IsSigned = false;  //Complex values are never integers, and their IsSigned property
  static ITK_CONSTEXPR_VAR bool IsInteger = true;  //should match that of their base type, so this should fail
};

}//end namespace itk

namespace numeric_traits_test {

// Change from anonymous namespace to named namespace to bypass clang with
// XCode 7.3, 8 internal compiler errors
template<typename T> void CheckVariableLengthArrayTraits(const T &t)
{
  std::string name;
#ifdef GCC_USEDEMANGLE
  char const *mangledName = typeid( t ).name();
  int         status;
  char *      unmangled = abi::__cxa_demangle(mangledName, ITK_NULLPTR, ITK_NULLPTR, &status);
  name = unmangled;
  free(unmangled);
#else
  name = typeid( t ).name();
#endif

  // check std::numeric_limits members
  std::cout << "itk::NumericTraits<" << name << ">" << std::endl;
  std::cout << "\tmin(" << name << "): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::min(t)) << std::endl;
  std::cout << "\tNonpositiveMin(" << name << "): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::NonpositiveMin(t)) << std::endl;
  std::cout << "\tmax(" << name << "): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::max(t)) << std::endl;
  std::cout << "\tZeroValue(" << name << "): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::ZeroValue(t)) << std::endl;
  std::cout << "\tOneValue(" << name << "): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::OneValue(t)) << std::endl;
  std::cout << "\tGetLength(" << name << "): " << itk::NumericTraits<T>::GetLength(t) << std::endl;
}

}

namespace
{
void CheckPointer( const void *) {}

using numeric_traits_test::CheckVariableLengthArrayTraits;

template<typename T> void CheckFixedArrayTraits(const T &t)
{

  std::string name;
#ifdef GCC_USEDEMANGLE
  char const *mangledName = typeid( t ).name();
  int         status;
  char *      unmangled = abi::__cxa_demangle(mangledName, ITK_NULLPTR, ITK_NULLPTR, &status);
  name = unmangled;
  free(unmangled);
#else
  name = typeid( t ).name();
#endif

  // check std::numeric_limits members
  std::cout << "itk::NumericTraits<" << name << ">" << std::endl;
  std::cout << "\tZero: " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::Zero) << std::endl;
  std::cout << "\tOne: " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::One) << std::endl;
  std::cout << "\tmin(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::min()) << std::endl;
  std::cout << "\tNonpositiveMin(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::NonpositiveMin()) << std::endl;
  std::cout << "\tmax(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::max()) << std::endl;
  std::cout << "\tZeroValue(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::ZeroValue()) << std::endl;
  std::cout << "\tOneValue(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::OneValue()) << std::endl;
  std::cout << "\tGetLength(): " << itk::NumericTraits<T>::GetLength() << std::endl;

  CheckPointer(&itk::NumericTraits<T>::One);
  CheckPointer(&itk::NumericTraits<T>::Zero);

  CheckVariableLengthArrayTraits(t);
}


template<typename T> void CheckTraits(const char *name, T t)
{
  // check std::numeric_limits members
  std::cout << "itk::NumericTraits<" << name << ">" << std::endl;
  std::cout << "\tis_specialized: " << itk::NumericTraits<T>::digits << std::endl;
  std::cout << "\tdigits: " << itk::NumericTraits<T>::digits << std::endl;
  std::cout << "\tdigits10: " << itk::NumericTraits<T>::digits10 << std::endl;
  std::cout << "\tis_signed: " << itk::NumericTraits<T>::is_signed << std::endl;
  std::cout << "\tround_error(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::round_error()) << std::endl;
  std::cout << "\tdenorm_min(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::denorm_min()) << std::endl;

  // to move to array traits?
  std::cout << "\tepsilon(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::epsilon()) << std::endl;

  // check NumericTraits
  std::cout << "\tIsPositive( One )" << itk::NumericTraits<T>::IsPositive( itk::NumericTraits<T>::OneValue() ) << std::endl;
  std::cout << "\tIsNonpositive( One )" << itk::NumericTraits<T>::IsNonpositive( itk::NumericTraits<T>::OneValue() ) << std::endl;
  std::cout << "\tIsNegative( One )" << itk::NumericTraits<T>::IsNegative( itk::NumericTraits<T>::OneValue() ) << std::endl;
  std::cout << "\tIsNonnegative( One )" << itk::NumericTraits<T>::IsNonnegative( itk::NumericTraits<T>::OneValue() ) << std::endl;

 CheckFixedArrayTraits(t);
}

//The following functions and classes are used to test the new NumericTraits, IsSigned and IsInteger
//check IsSigned and IsInteger to make sure that they match the behaviour of std::numeric_limits
template <typename T>
bool CheckSignedAndIntegerTraitsSameAsSTDNumericLimits( const char * const name )
{
  bool didTestPass = true;
  std::cout << "    " << name << std::endl;
  //test for IsSigned
  if( itk::NumericTraits<T>::IsSigned != std::numeric_limits<T>::is_signed )
    {
    std::cout << "\tERROR:  IsSigned definitions for itk::NumericTraits and std::numeric_limits do not match!! ERROR!!" << std::endl;
    std::cout << "\tFor type: \t" << name << std::endl;
    std::cout <<  "\tITK signed Value for:\t<  " << name << "  >\tis:\t" << ( itk::NumericTraits<T>::IsSigned ?"true":"false" ) << std::endl;
    std::cout <<  "\tstd signed Value for:\t<  " << name << "  >\tis:\t" << ( std::numeric_limits<T>::is_signed ?"true":"false" ) << std::endl;
    didTestPass=false;
    }
  else
    {
    std::cout << "\tSUCCESS:  IsSigned definition for itk::NumericTraits matches std::numeric_limits" << std::endl;
    std::cout << "\tSigned Value for:\t<  " << name << "  >\tis:\t" << ( itk::NumericTraits<T>::IsSigned ?"true":"false" ) << std::endl;
    }

  //test for IsInteger
  if( itk::NumericTraits<T>::IsInteger != std::numeric_limits<T>::is_integer )
    {
    std::cout << "\tERROR:  IsInteger definitions for itk::NumericTraits and std::numeric_limits do not match!! ERROR!!" << std::endl;
    std::cout << "\tFor type: \t" << name << std::endl;
    std::cout <<  "\tITK integer value for:\t<  " << name << "  >\tis:\t" << ( itk::NumericTraits<T>::IsInteger ?"true":"false" ) << std::endl;
    std::cout <<  "\tstd integer value for:\t<  " << name << "  >\tis:\t" << ( std::numeric_limits<T>::is_integer ?"true":"false" ) << std::endl;
    didTestPass=false;
    }
  else
    {
    std::cout << "\tSUCCESS:  IsInteger definition for itk::NumericTraits matches std::numeric_limits" << std::endl;
    std::cout << "\tInteger Value for:\t<  " << name << "  >\tis:\t" << ( itk::NumericTraits<T>::IsInteger ?"true":"false" ) << std::endl;
    }
  std::cout << std::endl;
  return didTestPass;
}

//Complex types should have the same sign as their base types, but none of them should be considered integers
template <typename T>
bool CheckSignedAndIntegerTraitsForComplexTypes( const char * const name )
{
  bool didTestPass = true;
  std::cout << "    " << name << std::endl;
  //complex types should never be integers.
  if( itk::NumericTraits<T>::IsInteger )
    {
    didTestPass = false;
    std::cout << "\tERROR:  NumericTraits< " << name << " >::IsInteger definition is true." << std::endl;
    std::cout << "\tComplex types are not integers" << std::endl;
    }
  else
    {
    didTestPass = true;
    }

  //IsSigned same for complex and basic types??
  if( itk::NumericTraits<T>::IsSigned != itk::NumericTraits< typename itk::NumericTraits<T>::ValueType >::IsSigned )
    {
    std::cout << "\tERROR:  IsSigned definitions for itk::NumericTraits< " << name << " > and" << std::endl;
    std::cout << "\t        itk::NumericTraits< " << name << " >::ValueType" << std::endl;
    std::cout << "\t         do not match!! ERROR!!" << std::endl;
    std::cout << "\tFor type: \t" << name << std::endl;
    std::cout <<  "\tSigned Value for:\t<  " << name << "  >\tis:\t" << ( itk::NumericTraits<T>::IsSigned ?"true":"false" ) << std::endl;
    std::cout <<  "\tSigned Value for:\t<  NumericTraits< " << name << "  >::ValueType \tis:\t";
    std::cout << ( itk::NumericTraits< typename itk::NumericTraits<T>::ValueType >::IsSigned ?"true":"false" ) << std::endl;
    didTestPass=false;
    }
  else
    {
    std::cout << "\tSUCCESS:  IsSigned definition for complex type  matches value of basic type" << std::endl;
    std::cout << "\tSigned Value for:\t<  " << name << "  >\tis:\t" << ( itk::NumericTraits<T>::IsSigned ?"true":"false" ) << std::endl;
    didTestPass = true;
    }
  std::cout << std::endl;
  return didTestPass;
}

// the types that are to be checked for correct IsSigned and IsInteger values are set here
bool CheckAllSignedAndIntegerTraits()
{
  bool didAllTestsPass=true;
  std::cout << "\nTesting IsSigned and IsInteger traits for non-complex types:" << std::endl;
  std::cout << "\tThis first one should fail" << std::endl << std::endl;
  didAllTestsPass &= ! CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<ForcedFailureTestCase>("ForcedFailureTestCase");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<UnknownTypeTestCase>("UnknownTypeTestCase");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<bool>("bool");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<char>("char");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<signed char>("signed char");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<unsigned char>("unsigned char");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<short>("short");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<unsigned short>("unsigned short");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<int>("int");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<unsigned int>("unsigned int");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<long>("long");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<unsigned long>("unsigned long");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<float>("float");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<long double>("long double");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<long long>("long long");
  didAllTestsPass &= CheckSignedAndIntegerTraitsSameAsSTDNumericLimits<unsigned long long>("unsigned long long");

  std::cout << "\nTesting IsSigned and IsInteger traits for non-complex types:" << std::endl;
  std::cout << "\tThis first one should fail" << std::endl << std::endl;
  didAllTestsPass &= ! CheckSignedAndIntegerTraitsForComplexTypes< std::complex< ForcedFailureTestCase> >("std::complex< ForcedFailureTestCase >");
  didAllTestsPass &= CheckSignedAndIntegerTraitsForComplexTypes< std::complex< UnknownTypeTestCase> >("std::complex< UnknownTypeTestCase >");
  didAllTestsPass &= CheckSignedAndIntegerTraitsForComplexTypes< std::complex< char > >(" std::complex< char > ");
  didAllTestsPass &= CheckSignedAndIntegerTraitsForComplexTypes< std::complex< unsigned char > >(" std::complex< unsigned char > ");
  didAllTestsPass &= CheckSignedAndIntegerTraitsForComplexTypes< std::complex< short > >(" std::complex< short > ");
  didAllTestsPass &= CheckSignedAndIntegerTraitsForComplexTypes< std::complex< unsigned short > >(" std::complex< unsigned short > ");
  didAllTestsPass &= CheckSignedAndIntegerTraitsForComplexTypes< std::complex< int > >(" std::complex< int > ");
  didAllTestsPass &= CheckSignedAndIntegerTraitsForComplexTypes< std::complex< unsigned int > >(" std::complex< unsigned int > ");
  didAllTestsPass &= CheckSignedAndIntegerTraitsForComplexTypes< std::complex< long > >(" std::complex< long > ");
  didAllTestsPass &= CheckSignedAndIntegerTraitsForComplexTypes< std::complex< unsigned long > >(" std::complex< unsigned long > ");
  didAllTestsPass &= CheckSignedAndIntegerTraitsForComplexTypes< std::complex< float > >(" std::complex< float > ");
  didAllTestsPass &= CheckSignedAndIntegerTraitsForComplexTypes< std::complex< double > >(" std::complex< double > ");
  didAllTestsPass &= CheckSignedAndIntegerTraitsForComplexTypes< std::complex< long double > >(" std::complex< long double > ");

  if(didAllTestsPass)
    {
    std::cout << "SUCESS!!:  All IsSigned and IsInteger tests Passed!!!" << std::endl;
    }
  else
    {
    std::cout << "FAIL!!:  Not all IsSigned and IsInteger tests Passed !!!" << std::endl;
    }
  std::cout << "End of IsSigned and IsInteger traits testing\n" << std::endl;

  return didAllTestsPass;
}

// Check a few types and make sure that they have the correct value for IsComplex
bool CheckIsComplexTraits()
{
  bool didTestsPass = true;
  std::cout << "Testing non complex types for IsComplex trait" << std::endl;
  if ( itk::NumericTraits< float >::IsComplex
    || itk::NumericTraits< double >::IsComplex
    || itk::NumericTraits< char >::IsComplex
    || itk::NumericTraits< int >::IsComplex
    || itk::NumericTraits< unsigned long >::IsComplex )
    {
    std::cout << "Test FAILED!!\n" << std::endl;
    std::cout << "Not all non complex types have the correct IsComplex trait" << std::endl;
    didTestsPass = false;
    }
  else
    {
    std::cout << "Test Passed\n" << std::endl;
    }

  std::cout << "Testing complex types for IsComplex trait" << std::endl;
  if ( !itk::NumericTraits< std::complex< float > >::IsComplex
    || !itk::NumericTraits< std::complex< double > >::IsComplex
    || !itk::NumericTraits< std::complex< char > >::IsComplex
    || !itk::NumericTraits< std::complex< int > >::IsComplex
    || !itk::NumericTraits< std::complex< unsigned long > >::IsComplex )
    {
    std::cout << "Test FAILED!!\n" << std::endl;
    std::cout << "Not all complex types have the correct IsComplex trait" << std::endl;
    didTestsPass = false;
    }
  else
    {
    std::cout << "Test Passed\n" << std::endl;
    }
  return didTestsPass;
} // End CheckIsComplexTraits()

} // end anonymous namespace

int itkNumericTraitsTest(int, char* [] )
{
  bool testPassedStatus = true;

  CheckTraits("char", static_cast<char>(0));
  CheckTraits("signed char", static_cast<signed char>(0));
  CheckTraits("unsigned char", static_cast<unsigned char>(0));

  CheckTraits("short", static_cast<short>(0));
  CheckTraits("signed short", static_cast<signed short>(0));
  CheckTraits("unsigned short", static_cast<unsigned short>(0));

  CheckTraits("int", static_cast<int>(0));
  CheckTraits("signed int", static_cast<signed int>(0));
  CheckTraits("unsigned int", static_cast<unsigned int>(0));

  CheckTraits("long", static_cast<long>(0));
  CheckTraits("signed long", static_cast<signed long>(0));
  CheckTraits("unsigned long", static_cast<unsigned long>(0));

  CheckTraits("long long", static_cast<long long>(0));
  CheckTraits("signed long long", static_cast<signed long long>(0));
  CheckTraits("unsigned long long", static_cast<unsigned long long>(0));

  CheckTraits("float", static_cast<float>(0));
  CheckTraits("double", static_cast<double>(0));
  CheckTraits("long double", static_cast<long double>(0));

  // Check not fundamental types which we need

  CheckTraits("size_t", static_cast<size_t>(0));
  CheckTraits("std::ptrdiff_t", static_cast<std::ptrdiff_t>(0));
  typedef std::vector<int>::size_type VectorSizeType;
  CheckTraits("std::vector<int>::size_type", static_cast<VectorSizeType>(0));

#ifndef __EMSCRIPTEN__
  // itk::CovariantVector<char, 1>()
  CheckFixedArrayTraits(itk::CovariantVector<char, 1>());
  CheckFixedArrayTraits(itk::CovariantVector<signed char, 1>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned char, 1>());

  CheckFixedArrayTraits(itk::CovariantVector<short, 1>());
  CheckFixedArrayTraits(itk::CovariantVector<signed short, 1>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned short, 1>());

  CheckFixedArrayTraits(itk::CovariantVector<int, 1>());
  CheckFixedArrayTraits(itk::CovariantVector<signed int, 1>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned int, 1>());

  CheckFixedArrayTraits(itk::CovariantVector<long, 1>());
  CheckFixedArrayTraits(itk::CovariantVector<signed long, 1>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned long, 1>());

  CheckFixedArrayTraits(itk::CovariantVector<long long, 1>());
  CheckFixedArrayTraits(itk::CovariantVector<signed long long, 1>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned long long, 1>());

  CheckFixedArrayTraits(itk::CovariantVector<float, 1>());
  CheckFixedArrayTraits(itk::CovariantVector<double, 1>());
  CheckFixedArrayTraits(itk::CovariantVector<long double, 1>());


  // itk::CovariantVector<char, 2>()
  CheckFixedArrayTraits(itk::CovariantVector<char, 2>());
  CheckFixedArrayTraits(itk::CovariantVector<signed char, 2>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned char, 2>());

  CheckFixedArrayTraits(itk::CovariantVector<short, 2>());
  CheckFixedArrayTraits(itk::CovariantVector<signed short, 2>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned short, 2>());

  CheckFixedArrayTraits(itk::CovariantVector<int, 2>());
  CheckFixedArrayTraits(itk::CovariantVector<signed int, 2>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned int, 2>());

  CheckFixedArrayTraits(itk::CovariantVector<long, 2>());
  CheckFixedArrayTraits(itk::CovariantVector<signed long, 2>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned long, 2>());

  CheckFixedArrayTraits(itk::CovariantVector<long long, 2>());
  CheckFixedArrayTraits(itk::CovariantVector<signed long long, 2>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned long long, 2>());

  CheckFixedArrayTraits(itk::CovariantVector<float, 2>());
  CheckFixedArrayTraits(itk::CovariantVector<double, 2>());
  CheckFixedArrayTraits(itk::CovariantVector<long double, 2>());


  // itk::CovariantVector<char, 3>()
  CheckFixedArrayTraits(itk::CovariantVector<char, 3>());
  CheckFixedArrayTraits(itk::CovariantVector<signed char, 3>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned char, 3>());

  CheckFixedArrayTraits(itk::CovariantVector<short, 3>());
  CheckFixedArrayTraits(itk::CovariantVector<signed short, 3>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned short, 3>());

  CheckFixedArrayTraits(itk::CovariantVector<int, 3>());
  CheckFixedArrayTraits(itk::CovariantVector<signed int, 3>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned int, 3>());

  CheckFixedArrayTraits(itk::CovariantVector<long, 3>());
  CheckFixedArrayTraits(itk::CovariantVector<signed long, 3>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned long, 3>());

  CheckFixedArrayTraits(itk::CovariantVector<long long, 3>());
  CheckFixedArrayTraits(itk::CovariantVector<signed long long, 3>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned long long, 3>());

  CheckFixedArrayTraits(itk::CovariantVector<float, 3>());
  CheckFixedArrayTraits(itk::CovariantVector<double, 3>());
  CheckFixedArrayTraits(itk::CovariantVector<long double, 3>());


  // itk::CovariantVector<char, 4>()
  CheckFixedArrayTraits(itk::CovariantVector<char, 4>());
  CheckFixedArrayTraits(itk::CovariantVector<signed char, 4>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned char, 4>());

  CheckFixedArrayTraits(itk::CovariantVector<short, 4>());
  CheckFixedArrayTraits(itk::CovariantVector<signed short, 4>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned short, 4>());

  CheckFixedArrayTraits(itk::CovariantVector<int, 4>());
  CheckFixedArrayTraits(itk::CovariantVector<signed int, 4>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned int, 4>());

  CheckFixedArrayTraits(itk::CovariantVector<long, 4>());
  CheckFixedArrayTraits(itk::CovariantVector<signed long, 4>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned long, 4>());

  CheckFixedArrayTraits(itk::CovariantVector<long long, 4>());
  CheckFixedArrayTraits(itk::CovariantVector<signed long long, 4>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned long long, 4>());

  CheckFixedArrayTraits(itk::CovariantVector<float, 4>());
  CheckFixedArrayTraits(itk::CovariantVector<double, 4>());
  CheckFixedArrayTraits(itk::CovariantVector<long double, 4>());


  // itk::CovariantVector<char, 5>()
  CheckFixedArrayTraits(itk::CovariantVector<char, 5>());
  CheckFixedArrayTraits(itk::CovariantVector<signed char, 5>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned char, 5>());

  CheckFixedArrayTraits(itk::CovariantVector<short, 5>());
  CheckFixedArrayTraits(itk::CovariantVector<signed short, 5>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned short, 5>());

  CheckFixedArrayTraits(itk::CovariantVector<int, 5>());
  CheckFixedArrayTraits(itk::CovariantVector<signed int, 5>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned int, 5>());

  CheckFixedArrayTraits(itk::CovariantVector<long, 5>());
  CheckFixedArrayTraits(itk::CovariantVector<signed long, 5>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned long, 5>());

  CheckFixedArrayTraits(itk::CovariantVector<long long, 5>());
  CheckFixedArrayTraits(itk::CovariantVector<signed long long, 5>());
  CheckFixedArrayTraits(itk::CovariantVector<unsigned long long, 5>());

  CheckFixedArrayTraits(itk::CovariantVector<float, 5>());
  CheckFixedArrayTraits(itk::CovariantVector<double, 5>());
  CheckFixedArrayTraits(itk::CovariantVector<long double, 5>());


  // itk::FixedArray<char, 1>()
  CheckFixedArrayTraits(itk::FixedArray<char, 1>());
  CheckFixedArrayTraits(itk::FixedArray<signed char, 1>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned char, 1>());

  CheckFixedArrayTraits(itk::FixedArray<short, 1>());
  CheckFixedArrayTraits(itk::FixedArray<signed short, 1>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned short, 1>());

  CheckFixedArrayTraits(itk::FixedArray<int, 1>());
  CheckFixedArrayTraits(itk::FixedArray<signed int, 1>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned int, 1>());

  CheckFixedArrayTraits(itk::FixedArray<long, 1>());
  CheckFixedArrayTraits(itk::FixedArray<signed long, 1>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned long, 1>());

  CheckFixedArrayTraits(itk::FixedArray<long long, 1>());
  CheckFixedArrayTraits(itk::FixedArray<signed long long, 1>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned long long, 1>());

  CheckFixedArrayTraits(itk::FixedArray<float, 1>());
  CheckFixedArrayTraits(itk::FixedArray<double, 1>());
  CheckFixedArrayTraits(itk::FixedArray<long double, 1>());


  // itk::FixedArray<char, 2>()
  CheckFixedArrayTraits(itk::FixedArray<char, 2>());
  CheckFixedArrayTraits(itk::FixedArray<signed char, 2>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned char, 2>());

  CheckFixedArrayTraits(itk::FixedArray<short, 2>());
  CheckFixedArrayTraits(itk::FixedArray<signed short, 2>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned short, 2>());

  CheckFixedArrayTraits(itk::FixedArray<int, 2>());
  CheckFixedArrayTraits(itk::FixedArray<signed int, 2>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned int, 2>());

  CheckFixedArrayTraits(itk::FixedArray<long, 2>());
  CheckFixedArrayTraits(itk::FixedArray<signed long, 2>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned long, 2>());

  CheckFixedArrayTraits(itk::FixedArray<long long, 2>());
  CheckFixedArrayTraits(itk::FixedArray<signed long long, 2>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned long long, 2>());

  CheckFixedArrayTraits(itk::FixedArray<float, 2>());
  CheckFixedArrayTraits(itk::FixedArray<double, 2>());
  CheckFixedArrayTraits(itk::FixedArray<long double, 2>());


  // itk::FixedArray<char, 3>()
  CheckFixedArrayTraits(itk::FixedArray<char, 3>());
  CheckFixedArrayTraits(itk::FixedArray<signed char, 3>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned char, 3>());

  CheckFixedArrayTraits(itk::FixedArray<short, 3>());
  CheckFixedArrayTraits(itk::FixedArray<signed short, 3>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned short, 3>());

  CheckFixedArrayTraits(itk::FixedArray<int, 3>());
  CheckFixedArrayTraits(itk::FixedArray<signed int, 3>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned int, 3>());

  CheckFixedArrayTraits(itk::FixedArray<long, 3>());
  CheckFixedArrayTraits(itk::FixedArray<signed long, 3>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned long, 3>());

  CheckFixedArrayTraits(itk::FixedArray<long long, 3>());
  CheckFixedArrayTraits(itk::FixedArray<signed long long, 3>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned long long, 3>());

  CheckFixedArrayTraits(itk::FixedArray<float, 3>());
  CheckFixedArrayTraits(itk::FixedArray<double, 3>());
  CheckFixedArrayTraits(itk::FixedArray<long double, 3>());


  // itk::FixedArray<char, 4>()
  CheckFixedArrayTraits(itk::FixedArray<char, 4>());
  CheckFixedArrayTraits(itk::FixedArray<signed char, 4>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned char, 4>());

  CheckFixedArrayTraits(itk::FixedArray<short, 4>());
  CheckFixedArrayTraits(itk::FixedArray<signed short, 4>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned short, 4>());

  CheckFixedArrayTraits(itk::FixedArray<int, 4>());
  CheckFixedArrayTraits(itk::FixedArray<signed int, 4>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned int, 4>());

  CheckFixedArrayTraits(itk::FixedArray<long, 4>());
  CheckFixedArrayTraits(itk::FixedArray<signed long, 4>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned long, 4>());

  CheckFixedArrayTraits(itk::FixedArray<long long, 4>());
  CheckFixedArrayTraits(itk::FixedArray<signed long long, 4>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned long long, 4>());

  CheckFixedArrayTraits(itk::FixedArray<float, 4>());
  CheckFixedArrayTraits(itk::FixedArray<double, 4>());
  CheckFixedArrayTraits(itk::FixedArray<long double, 4>());


  // itk::FixedArray<char, 5>()
  CheckFixedArrayTraits(itk::FixedArray<char, 5>());
  CheckFixedArrayTraits(itk::FixedArray<signed char, 5>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned char, 5>());

  CheckFixedArrayTraits(itk::FixedArray<short, 5>());
  CheckFixedArrayTraits(itk::FixedArray<signed short, 5>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned short, 5>());

  CheckFixedArrayTraits(itk::FixedArray<int, 5>());
  CheckFixedArrayTraits(itk::FixedArray<signed int, 5>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned int, 5>());

  CheckFixedArrayTraits(itk::FixedArray<long, 5>());
  CheckFixedArrayTraits(itk::FixedArray<signed long, 5>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned long, 5>());

  CheckFixedArrayTraits(itk::FixedArray<long long, 5>());
  CheckFixedArrayTraits(itk::FixedArray<signed long long, 5>());
  CheckFixedArrayTraits(itk::FixedArray<unsigned long long, 5>());

  CheckFixedArrayTraits(itk::FixedArray<float, 5>());
  CheckFixedArrayTraits(itk::FixedArray<double, 5>());
  CheckFixedArrayTraits(itk::FixedArray<long double, 5>());


  // itk::Point<char, 1>()
  CheckFixedArrayTraits(itk::Point<char, 1>());
  CheckFixedArrayTraits(itk::Point<signed char, 1>());
  CheckFixedArrayTraits(itk::Point<unsigned char, 1>());

  CheckFixedArrayTraits(itk::Point<short, 1>());
  CheckFixedArrayTraits(itk::Point<signed short, 1>());
  CheckFixedArrayTraits(itk::Point<unsigned short, 1>());

  CheckFixedArrayTraits(itk::Point<int, 1>());
  CheckFixedArrayTraits(itk::Point<signed int, 1>());
  CheckFixedArrayTraits(itk::Point<unsigned int, 1>());

  CheckFixedArrayTraits(itk::Point<long, 1>());
  CheckFixedArrayTraits(itk::Point<signed long, 1>());
  CheckFixedArrayTraits(itk::Point<unsigned long, 1>());

  CheckFixedArrayTraits(itk::Point<long long, 1>());
  CheckFixedArrayTraits(itk::Point<signed long long, 1>());
  CheckFixedArrayTraits(itk::Point<unsigned long long, 1>());

  CheckFixedArrayTraits(itk::Point<float, 1>());
  CheckFixedArrayTraits(itk::Point<double, 1>());
  CheckFixedArrayTraits(itk::Point<long double, 1>());


  // itk::Point<char, 2>()
  CheckFixedArrayTraits(itk::Point<char, 2>());
  CheckFixedArrayTraits(itk::Point<signed char, 2>());
  CheckFixedArrayTraits(itk::Point<unsigned char, 2>());

  CheckFixedArrayTraits(itk::Point<short, 2>());
  CheckFixedArrayTraits(itk::Point<signed short, 2>());
  CheckFixedArrayTraits(itk::Point<unsigned short, 2>());

  CheckFixedArrayTraits(itk::Point<int, 2>());
  CheckFixedArrayTraits(itk::Point<signed int, 2>());
  CheckFixedArrayTraits(itk::Point<unsigned int, 2>());

  CheckFixedArrayTraits(itk::Point<long, 2>());
  CheckFixedArrayTraits(itk::Point<signed long, 2>());
  CheckFixedArrayTraits(itk::Point<unsigned long, 2>());

  CheckFixedArrayTraits(itk::Point<long long, 2>());
  CheckFixedArrayTraits(itk::Point<signed long long, 2>());
  CheckFixedArrayTraits(itk::Point<unsigned long long, 2>());

  CheckFixedArrayTraits(itk::Point<float, 2>());
  CheckFixedArrayTraits(itk::Point<double, 2>());
  CheckFixedArrayTraits(itk::Point<long double, 2>());


  // itk::Point<char, 3>()
  CheckFixedArrayTraits(itk::Point<char, 3>());
  CheckFixedArrayTraits(itk::Point<signed char, 3>());
  CheckFixedArrayTraits(itk::Point<unsigned char, 3>());

  CheckFixedArrayTraits(itk::Point<short, 3>());
  CheckFixedArrayTraits(itk::Point<signed short, 3>());
  CheckFixedArrayTraits(itk::Point<unsigned short, 3>());

  CheckFixedArrayTraits(itk::Point<int, 3>());
  CheckFixedArrayTraits(itk::Point<signed int, 3>());
  CheckFixedArrayTraits(itk::Point<unsigned int, 3>());

  CheckFixedArrayTraits(itk::Point<long, 3>());
  CheckFixedArrayTraits(itk::Point<signed long, 3>());
  CheckFixedArrayTraits(itk::Point<unsigned long, 3>());

  CheckFixedArrayTraits(itk::Point<long long, 3>());
  CheckFixedArrayTraits(itk::Point<signed long long, 3>());
  CheckFixedArrayTraits(itk::Point<unsigned long long, 3>());

  CheckFixedArrayTraits(itk::Point<float, 3>());
  CheckFixedArrayTraits(itk::Point<double, 3>());
  CheckFixedArrayTraits(itk::Point<long double, 3>());


  // itk::Point<char, 4>()
  CheckFixedArrayTraits(itk::Point<char, 4>());
  CheckFixedArrayTraits(itk::Point<signed char, 4>());
  CheckFixedArrayTraits(itk::Point<unsigned char, 4>());

  CheckFixedArrayTraits(itk::Point<short, 4>());
  CheckFixedArrayTraits(itk::Point<signed short, 4>());
  CheckFixedArrayTraits(itk::Point<unsigned short, 4>());

  CheckFixedArrayTraits(itk::Point<int, 4>());
  CheckFixedArrayTraits(itk::Point<signed int, 4>());
  CheckFixedArrayTraits(itk::Point<unsigned int, 4>());

  CheckFixedArrayTraits(itk::Point<long, 4>());
  CheckFixedArrayTraits(itk::Point<signed long, 4>());
  CheckFixedArrayTraits(itk::Point<unsigned long, 4>());

  CheckFixedArrayTraits(itk::Point<long long, 4>());
  CheckFixedArrayTraits(itk::Point<signed long long, 4>());
  CheckFixedArrayTraits(itk::Point<unsigned long long, 4>());

  CheckFixedArrayTraits(itk::Point<float, 4>());
  CheckFixedArrayTraits(itk::Point<double, 4>());
  CheckFixedArrayTraits(itk::Point<long double, 4>());


  // itk::Point<char, 5>()
  CheckFixedArrayTraits(itk::Point<char, 5>());
  CheckFixedArrayTraits(itk::Point<signed char, 5>());
  CheckFixedArrayTraits(itk::Point<unsigned char, 5>());

  CheckFixedArrayTraits(itk::Point<short, 5>());
  CheckFixedArrayTraits(itk::Point<signed short, 5>());
  CheckFixedArrayTraits(itk::Point<unsigned short, 5>());

  CheckFixedArrayTraits(itk::Point<int, 5>());
  CheckFixedArrayTraits(itk::Point<signed int, 5>());
  CheckFixedArrayTraits(itk::Point<unsigned int, 5>());

  CheckFixedArrayTraits(itk::Point<long, 5>());
  CheckFixedArrayTraits(itk::Point<signed long, 5>());
  CheckFixedArrayTraits(itk::Point<unsigned long, 5>());

  CheckFixedArrayTraits(itk::Point<long long, 5>());
  CheckFixedArrayTraits(itk::Point<signed long long, 5>());
  CheckFixedArrayTraits(itk::Point<unsigned long long, 5>());

  CheckFixedArrayTraits(itk::Point<float, 5>());
  CheckFixedArrayTraits(itk::Point<double, 5>());
  CheckFixedArrayTraits(itk::Point<long double, 5>());


  // itk::RGBPixel<char>()
  CheckFixedArrayTraits(itk::RGBPixel<char>());
  CheckFixedArrayTraits(itk::RGBPixel<signed char>());
  CheckFixedArrayTraits(itk::RGBPixel<unsigned char>());

  CheckFixedArrayTraits(itk::RGBPixel<short>());
  CheckFixedArrayTraits(itk::RGBPixel<signed short>());
  CheckFixedArrayTraits(itk::RGBPixel<unsigned short>());

  CheckFixedArrayTraits(itk::RGBPixel<int>());
  CheckFixedArrayTraits(itk::RGBPixel<signed int>());
  CheckFixedArrayTraits(itk::RGBPixel<unsigned int>());

  CheckFixedArrayTraits(itk::RGBPixel<long>());
  CheckFixedArrayTraits(itk::RGBPixel<signed long>());
  CheckFixedArrayTraits(itk::RGBPixel<unsigned long>());

  CheckFixedArrayTraits(itk::RGBPixel<long long>());
  CheckFixedArrayTraits(itk::RGBPixel<signed long long>());
  CheckFixedArrayTraits(itk::RGBPixel<unsigned long long>());

  CheckFixedArrayTraits(itk::RGBPixel<float>());
  CheckFixedArrayTraits(itk::RGBPixel<double>());
  CheckFixedArrayTraits(itk::RGBPixel<long double>());


  // itk::RGBAPixel<char>()
  CheckFixedArrayTraits(itk::RGBAPixel<char>());
  CheckFixedArrayTraits(itk::RGBAPixel<signed char>());
  CheckFixedArrayTraits(itk::RGBAPixel<unsigned char>());

  CheckFixedArrayTraits(itk::RGBAPixel<short>());
  CheckFixedArrayTraits(itk::RGBAPixel<signed short>());
  CheckFixedArrayTraits(itk::RGBAPixel<unsigned short>());

  CheckFixedArrayTraits(itk::RGBAPixel<int>());
  CheckFixedArrayTraits(itk::RGBAPixel<signed int>());
  CheckFixedArrayTraits(itk::RGBAPixel<unsigned int>());

  CheckFixedArrayTraits(itk::RGBAPixel<long>());
  CheckFixedArrayTraits(itk::RGBAPixel<signed long>());
  CheckFixedArrayTraits(itk::RGBAPixel<unsigned long>());

  CheckFixedArrayTraits(itk::RGBAPixel<long long>());
  CheckFixedArrayTraits(itk::RGBAPixel<signed long long>());
  CheckFixedArrayTraits(itk::RGBAPixel<unsigned long long>());

  CheckFixedArrayTraits(itk::RGBAPixel<float>());
  CheckFixedArrayTraits(itk::RGBAPixel<double>());
  CheckFixedArrayTraits(itk::RGBAPixel<long double>());


  // itk::SymmetricSecondRankTensor<char, 1>()
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<char, 1>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed char, 1>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned char, 1>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<short, 1>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed short, 1>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned short, 1>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<int, 1>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed int, 1>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned int, 1>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long, 1>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed long, 1>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned long, 1>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long long, 1>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed long long, 1>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned long long, 1>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<float, 1>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<double, 1>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long double, 1>());


  // itk::SymmetricSecondRankTensor<char, 2>()
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<char, 2>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed char, 2>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned char, 2>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<short, 2>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed short, 2>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned short, 2>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<int, 2>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed int, 2>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned int, 2>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long, 2>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed long, 2>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned long, 2>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long long, 2>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed long long, 2>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned long long, 2>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<float, 2>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<double, 2>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long double, 2>());


  // itk::SymmetricSecondRankTensor<char, 3>()
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<char, 3>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed char, 3>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned char, 3>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<short, 3>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed short, 3>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned short, 3>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<int, 3>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed int, 3>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned int, 3>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long, 3>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed long, 3>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned long, 3>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long long, 3>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed long long, 3>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned long long, 3>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<float, 3>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<double, 3>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long double, 3>());


  // itk::SymmetricSecondRankTensor<char, 4>()
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<char, 4>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed char, 4>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned char, 4>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<short, 4>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed short, 4>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned short, 4>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<int, 4>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed int, 4>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned int, 4>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long, 4>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed long, 4>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned long, 4>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long long, 4>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed long long, 4>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned long long, 4>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<float, 4>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<double, 4>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long double, 4>());


  // itk::SymmetricSecondRankTensor<char, 5>()
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<char, 5>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed char, 5>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned char, 5>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<short, 5>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed short, 5>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned short, 5>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<int, 5>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed int, 5>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned int, 5>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long, 5>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed long, 5>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned long, 5>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long long, 5>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<signed long long, 5>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<unsigned long long, 5>());

  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<float, 5>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<double, 5>());
  CheckFixedArrayTraits(itk::SymmetricSecondRankTensor<long double, 5>());


  // itk::VariableLengthVector<char>(1)
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<char>(1));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed char>(1));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned char>(1));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<short>(1));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed short>(1));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned short>(1));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<int>(1));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed int>(1));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned int>(1));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long>(1));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed long>(1));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned long>(1));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long long>(1));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed long long>(1));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned long long>(1));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<float>(1));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<double>(1));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long double>(1));


  // itk::VariableLengthVector<char>(2)
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<char>(2));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed char>(2));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned char>(2));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<short>(2));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed short>(2));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned short>(2));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<int>(2));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed int>(2));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned int>(2));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long>(2));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed long>(2));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned long>(2));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long long>(2));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed long long>(2));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned long long>(2));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<float>(2));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<double>(2));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long double>(2));


  // itk::VariableLengthVector<char>(3)
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<char>(3));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed char>(3));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned char>(3));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<short>(3));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed short>(3));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned short>(3));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<int>(3));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed int>(3));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned int>(3));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long>(3));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed long>(3));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned long>(3));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long long>(3));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed long long>(3));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned long long>(3));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<float>(3));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<double>(3));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long double>(3));


  // itk::VariableLengthVector<char>(4)
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<char>(4));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed char>(4));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned char>(4));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<short>(4));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed short>(4));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned short>(4));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<int>(4));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed int>(4));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned int>(4));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long>(4));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed long>(4));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned long>(4));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long long>(4));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed long long>(4));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned long long>(4));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<float>(4));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<double>(4));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long double>(4));


  // itk::VariableLengthVector<char>(5)
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<char>(5));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed char>(5));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned char>(5));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<short>(5));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed short>(5));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned short>(5));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<int>(5));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed int>(5));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned int>(5));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long>(5));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed long>(5));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned long>(5));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long long>(5));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<signed long long>(5));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<unsigned long long>(5));

  CheckVariableLengthArrayTraits(itk::VariableLengthVector<float>(5));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<double>(5));
  CheckVariableLengthArrayTraits(itk::VariableLengthVector<long double>(5));


  // itk::Array<char>(1)
  CheckVariableLengthArrayTraits(itk::Array<char>(1));
  CheckVariableLengthArrayTraits(itk::Array<signed char>(1));
  CheckVariableLengthArrayTraits(itk::Array<unsigned char>(1));

  CheckVariableLengthArrayTraits(itk::Array<short>(1));
  CheckVariableLengthArrayTraits(itk::Array<signed short>(1));
  CheckVariableLengthArrayTraits(itk::Array<unsigned short>(1));

  CheckVariableLengthArrayTraits(itk::Array<int>(1));
  CheckVariableLengthArrayTraits(itk::Array<signed int>(1));
  CheckVariableLengthArrayTraits(itk::Array<unsigned int>(1));

  CheckVariableLengthArrayTraits(itk::Array<long>(1));
  CheckVariableLengthArrayTraits(itk::Array<signed long>(1));
  CheckVariableLengthArrayTraits(itk::Array<unsigned long>(1));

//   CheckVariableLengthArrayTraits(itk::Array<long long>(1));
//   CheckVariableLengthArrayTraits(itk::Array<signed long long>(1));
//   CheckVariableLengthArrayTraits(itk::Array<unsigned long long>(1));

  CheckVariableLengthArrayTraits(itk::Array<float>(1));
  CheckVariableLengthArrayTraits(itk::Array<double>(1));
  CheckVariableLengthArrayTraits(itk::Array<long double>(1));


  // itk::Array<char>(2)
  CheckVariableLengthArrayTraits(itk::Array<char>(2));
  CheckVariableLengthArrayTraits(itk::Array<signed char>(2));
  CheckVariableLengthArrayTraits(itk::Array<unsigned char>(2));

  CheckVariableLengthArrayTraits(itk::Array<short>(2));
  CheckVariableLengthArrayTraits(itk::Array<signed short>(2));
  CheckVariableLengthArrayTraits(itk::Array<unsigned short>(2));

  CheckVariableLengthArrayTraits(itk::Array<int>(2));
  CheckVariableLengthArrayTraits(itk::Array<signed int>(2));
  CheckVariableLengthArrayTraits(itk::Array<unsigned int>(2));

  CheckVariableLengthArrayTraits(itk::Array<long>(2));
  CheckVariableLengthArrayTraits(itk::Array<signed long>(2));
  CheckVariableLengthArrayTraits(itk::Array<unsigned long>(2));

//   CheckVariableLengthArrayTraits(itk::Array<long long>(2));
//   CheckVariableLengthArrayTraits(itk::Array<signed long long>(2));
//   CheckVariableLengthArrayTraits(itk::Array<unsigned long long>(2));

  CheckVariableLengthArrayTraits(itk::Array<float>(2));
  CheckVariableLengthArrayTraits(itk::Array<double>(2));
  CheckVariableLengthArrayTraits(itk::Array<long double>(2));


  // itk::Array<char>(3)
  CheckVariableLengthArrayTraits(itk::Array<char>(3));
  CheckVariableLengthArrayTraits(itk::Array<signed char>(3));
  CheckVariableLengthArrayTraits(itk::Array<unsigned char>(3));

  CheckVariableLengthArrayTraits(itk::Array<short>(3));
  CheckVariableLengthArrayTraits(itk::Array<signed short>(3));
  CheckVariableLengthArrayTraits(itk::Array<unsigned short>(3));

  CheckVariableLengthArrayTraits(itk::Array<int>(3));
  CheckVariableLengthArrayTraits(itk::Array<signed int>(3));
  CheckVariableLengthArrayTraits(itk::Array<unsigned int>(3));

  CheckVariableLengthArrayTraits(itk::Array<long>(3));
  CheckVariableLengthArrayTraits(itk::Array<signed long>(3));
  CheckVariableLengthArrayTraits(itk::Array<unsigned long>(3));

//   CheckVariableLengthArrayTraits(itk::Array<long long>(3));
//   CheckVariableLengthArrayTraits(itk::Array<signed long long>(3));
//   CheckVariableLengthArrayTraits(itk::Array<unsigned long long>(3));

  CheckVariableLengthArrayTraits(itk::Array<float>(3));
  CheckVariableLengthArrayTraits(itk::Array<double>(3));
  CheckVariableLengthArrayTraits(itk::Array<long double>(3));


  // itk::Array<char>(4)
  CheckVariableLengthArrayTraits(itk::Array<char>(4));
  CheckVariableLengthArrayTraits(itk::Array<signed char>(4));
  CheckVariableLengthArrayTraits(itk::Array<unsigned char>(4));

  CheckVariableLengthArrayTraits(itk::Array<short>(4));
  CheckVariableLengthArrayTraits(itk::Array<signed short>(4));
  CheckVariableLengthArrayTraits(itk::Array<unsigned short>(4));

  CheckVariableLengthArrayTraits(itk::Array<int>(4));
  CheckVariableLengthArrayTraits(itk::Array<signed int>(4));
  CheckVariableLengthArrayTraits(itk::Array<unsigned int>(4));

  CheckVariableLengthArrayTraits(itk::Array<long>(4));
  CheckVariableLengthArrayTraits(itk::Array<signed long>(4));
  CheckVariableLengthArrayTraits(itk::Array<unsigned long>(4));

//   CheckVariableLengthArrayTraits(itk::Array<long long>(4));
//   CheckVariableLengthArrayTraits(itk::Array<signed long long>(4));
//   CheckVariableLengthArrayTraits(itk::Array<unsigned long long>(4));

  CheckVariableLengthArrayTraits(itk::Array<float>(4));
  CheckVariableLengthArrayTraits(itk::Array<double>(4));
  CheckVariableLengthArrayTraits(itk::Array<long double>(4));


  // itk::Array<char>(5)
  CheckVariableLengthArrayTraits(itk::Array<char>(5));
  CheckVariableLengthArrayTraits(itk::Array<signed char>(5));
  CheckVariableLengthArrayTraits(itk::Array<unsigned char>(5));

  CheckVariableLengthArrayTraits(itk::Array<short>(5));
  CheckVariableLengthArrayTraits(itk::Array<signed short>(5));
  CheckVariableLengthArrayTraits(itk::Array<unsigned short>(5));

  CheckVariableLengthArrayTraits(itk::Array<int>(5));
  CheckVariableLengthArrayTraits(itk::Array<signed int>(5));
  CheckVariableLengthArrayTraits(itk::Array<unsigned int>(5));

  CheckVariableLengthArrayTraits(itk::Array<long>(5));
  CheckVariableLengthArrayTraits(itk::Array<signed long>(5));
  CheckVariableLengthArrayTraits(itk::Array<unsigned long>(5));

//   CheckVariableLengthArrayTraits(itk::Array<long long>(5));
//   CheckVariableLengthArrayTraits(itk::Array<signed long long>(5));
//   CheckVariableLengthArrayTraits(itk::Array<unsigned long long>(5));

  CheckVariableLengthArrayTraits(itk::Array<float>(5));
  CheckVariableLengthArrayTraits(itk::Array<double>(5));
  CheckVariableLengthArrayTraits(itk::Array<long double>(5));


  // std::complex
  CheckFixedArrayTraits(std::complex<char>());
  CheckFixedArrayTraits(std::complex<unsigned char>());
  CheckFixedArrayTraits(std::complex<short>());
  CheckFixedArrayTraits(std::complex<unsigned short>());
  CheckFixedArrayTraits(std::complex<int>());
  CheckFixedArrayTraits(std::complex<unsigned int>());
  CheckFixedArrayTraits(std::complex<long>());
  CheckFixedArrayTraits(std::complex<unsigned long>());
  CheckFixedArrayTraits(std::complex<float>());
  CheckFixedArrayTraits(std::complex<double>());
  CheckFixedArrayTraits(std::complex<long double>());
#endif // __EMSCRIPTEN__

  //  check the new Integer and Signed traits
  testPassedStatus &= CheckAllSignedAndIntegerTraits();

  // Check IsComplex traits
  testPassedStatus &= CheckIsComplexTraits();

  return ( testPassedStatus ) ? EXIT_SUCCESS : EXIT_FAILURE;
}
