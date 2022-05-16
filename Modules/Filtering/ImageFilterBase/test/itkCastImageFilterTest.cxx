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

#include <iostream>
#include <limits>
#include <stdexcept>
#include <type_traits>

#include "itkCastImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkVectorImage.h"
#include "itkFloatingPointExceptions.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"

namespace itk
{}

// Better name demanging for gcc
#if defined(__GNUC__) && !defined(__EMSCRIPTEN__)
#  define GCC_USEDEMANGLE
#endif

#ifdef GCC_USEDEMANGLE
#  include <cstdlib>
#  include <cxxabi.h>
#  include "itkMath.h"
#endif


// Compile time check
template class itk::CastImageFilter<itk::Image<std::complex<float>, 2>, itk::Image<std::complex<float>, 2>>;
template class itk::CastImageFilter<itk::Image<std::complex<double>, 2>, itk::Image<std::complex<double>, 2>>;
template class itk::CastImageFilter<itk::Image<std::complex<float>, 2>, itk::Image<std::complex<double>, 2>>;
template class itk::CastImageFilter<itk::Image<std::complex<double>, 2>, itk::Image<std::complex<float>, 2>>;

template class itk::CastImageFilter<itk::Image<itk::RGBPixel<unsigned char>, 2>,
                                    itk::Image<itk::RGBPixel<unsigned short>, 2>>;
template class itk::CastImageFilter<itk::Image<itk::RGBPixel<unsigned char>, 2>, itk::Image<itk::Vector<float, 3>, 2>>;
template class itk::CastImageFilter<itk::Image<itk::Vector<float, 3>, 2>, itk::Image<itk::RGBPixel<unsigned char>, 2>>;
template class itk::CastImageFilter<itk::Image<itk::RGBPixel<unsigned char>, 2>,
                                    itk::Image<itk::CovariantVector<double, 3>, 2>>;
template class itk::CastImageFilter<itk::Image<itk::RGBAPixel<unsigned char>, 2>,
                                    itk::Image<itk::RGBAPixel<unsigned short>, 2>>;
template class itk::CastImageFilter<itk::Image<itk::RGBAPixel<unsigned char>, 2>, itk::Image<itk::Vector<float, 4>, 2>>;
template class itk::CastImageFilter<itk::Image<itk::Vector<float, 4>, 2>, itk::Image<itk::RGBAPixel<unsigned char>, 2>>;
template class itk::CastImageFilter<itk::Image<itk::RGBAPixel<unsigned char>, 2>,
                                    itk::Image<itk::CovariantVector<double, 4>, 2>>;
template class itk::CastImageFilter<itk::Image<itk::Vector<int, 3>, 2>, itk::Image<itk::CovariantVector<float, 3>, 2>>;
template class itk::CastImageFilter<itk::Image<itk::CovariantVector<short, 4>, 2>,
                                    itk::Image<itk::Vector<double, 4>, 2>>;

template class itk::CastImageFilter<itk::VectorImage<short, 2>, itk::VectorImage<double, 2>>;
template class itk::CastImageFilter<itk::VectorImage<short, 2>, itk::Image<itk::Vector<float, 4>, 2>>;
template class itk::CastImageFilter<itk::Image<itk::CovariantVector<short, 3>, 2>, itk::VectorImage<double, 2>>;
template class itk::CastImageFilter<itk::VectorImage<unsigned char, 2>, itk::Image<itk::RGBPixel<short>, 2>>;
template class itk::CastImageFilter<itk::Image<itk::RGBAPixel<unsigned short>, 2>, itk::VectorImage<int, 2>>;

template <typename T>
std::string
GetCastTypeName()
{
  std::string name;
#ifdef GCC_USEDEMANGLE
  char const * mangledName = typeid(T).name();
  int          status;
  char *       unmangled = abi::__cxa_demangle(mangledName, nullptr, nullptr, &status);
  name = unmangled;
  free(unmangled);
#else
  name = typeid(T).name();
#endif

  return name;
}

// A reference defining C++ behavior:
// https://www.cplusplus.com/doc/tutorial/typecasting/
// static_cast_is_well_defined function returns true if the result of the static cast is well defined
// and false if the result is undefined.
template <typename TInput, typename TOutput>
static std::enable_if_t<std::is_integral<TOutput>::value && std::is_integral<TInput>::value, bool>
  static_cast_is_well_defined(TInput)
{
  return true; // casting from int to int types employes deterministic 2's complement behavior
}

template <typename TInput, typename TOutput>
static std::enable_if_t<std::is_floating_point<TOutput>::value &&
                          (std::is_floating_point<TInput>::value || std::is_integral<TInput>::value),
                        bool>
  static_cast_is_well_defined(TInput)
{
  return true; // Floating point to floating point static casts are always consistently defined.
}

template <typename TInput, typename TOutput>
static std::enable_if_t<std::is_integral<TOutput>::value && std::is_unsigned<TOutput>::value &&
                          std::is_floating_point<TInput>::value,
                        bool>
static_cast_is_well_defined(TInput value)
{
  if (value < 0.0 || value > static_cast<TInput>(std::numeric_limits<TOutput>::max()))
  {
    return false;
  }
  return true;
}

template <typename TInput, typename TOutput>
static std::enable_if_t<std::is_integral<TOutput>::value && std::is_signed<TOutput>::value &&
                          std::is_floating_point<TInput>::value,
                        bool>
static_cast_is_well_defined(TInput value)
{
  if (value < static_cast<TInput>(std::numeric_limits<TOutput>::min()) ||
      value > static_cast<TInput>(std::numeric_limits<TOutput>::max()))
  {
    return false;
  }
  return true;
}

template <typename TInputPixelType, typename TOutputPixelType>
bool
TestCastFromTo()
{
  using InputImageType = itk::Image<TInputPixelType, 3>;
  using OutputImageType = itk::Image<TOutputPixelType, 3>;
  using FilterType = itk::CastImageFilter<InputImageType, OutputImageType>;

  using SourceType = itk::RandomImageSource<InputImageType>;
  auto randomValuesImageSource = SourceType::New();
  {
    typename InputImageType::SizeValueType randomSize[3] = { 18, 17, 23 };
    randomValuesImageSource->SetSize(randomSize);
  }
  randomValuesImageSource->UpdateLargestPossibleRegion();
  typename InputImageType::Pointer randomSourceImagePtr = randomValuesImageSource->GetOutput();
  {
    typename InputImageType::IndexType Index000{ { 0, 0, 0 } };
    typename InputImageType::IndexType Index100{ { 1, 0, 0 } };
    typename InputImageType::IndexType Index200{ { 2, 0, 0 } };
    typename InputImageType::IndexType Index300{ { 3, 0, 0 } };
    typename InputImageType::IndexType Index400{ { 4, 0, 0 } };

    /* Exercise input image type domain values (important for float -> integer conversions)
     *
     * Casting a float/double to an integer when integer isn't big enough to hold the value yields undefined behaviour.
     *
     * [n3290: 4.9/1]: A prvalue of a floating point type can be converted to a prvalue of an integer type.
     *  The conversion truncates; that is, the fractional part is discarded.
     *
     *  The behavior is undefined if the truncated value cannot be represented in the destination type.
     *                  ^^^^^^^^^
     * With scanline based optimization, SSE instructions can be used for converting blocks of
     * floats to integer, which results in different 'undefined' behavior for values
     * than an isolated static_cast<short>(outOfRangeFloat).
     */
    randomSourceImagePtr->SetPixel(Index000, std::numeric_limits<TInputPixelType>::max());
    randomSourceImagePtr->SetPixel(Index100, std::numeric_limits<TInputPixelType>::min());
    randomSourceImagePtr->SetPixel(Index200, std::numeric_limits<TInputPixelType>::epsilon());
    randomSourceImagePtr->SetPixel(Index300, std::numeric_limits<TInputPixelType>::denorm_min());
    randomSourceImagePtr->SetPixel(Index400, std::numeric_limits<TInputPixelType>::round_error());
  }

  auto filter = FilterType::New();
  filter->SetInput(randomSourceImagePtr);
  filter->UpdateLargestPossibleRegion();

  using InputIteratorType = itk::ImageRegionConstIterator<InputImageType>;
  using OutputIteratorType = itk::ImageRegionConstIterator<OutputImageType>;

  InputIteratorType  it(randomValuesImageSource->GetOutput(),
                       randomValuesImageSource->GetOutput()->GetLargestPossibleRegion());
  OutputIteratorType ot(filter->GetOutput(), filter->GetOutput()->GetLargestPossibleRegion());

  bool success = true;

  std::cout << "Casting from " << GetCastTypeName<TInputPixelType>() << " to " << GetCastTypeName<TOutputPixelType>()
            << " ... ";

  it.GoToBegin();
  ot.GoToBegin();
  while (!it.IsAtEnd())
  {
    const TInputPixelType  inValue = it.Value();
    const TOutputPixelType outValue = ot.Value();
    const auto             expectedValue = static_cast<TOutputPixelType>(inValue);

    if (!static_cast_is_well_defined<TInputPixelType, TOutputPixelType>(inValue))
    {
      ++it;
      ++ot;
      std::cout << std::flush;
      // Debugging code left here for testing __MINGW32__ behavior below that may be redundant with this code
      // std::cout << "NOTICE: static_cast<" << GetCastTypeName< TOutputPixelType >() << ">(" << inValue << ") => "
      //         << outValue << " may not equal " << expectedValue
      //         << "  Casting out of range floating point values to integers is 'undefined' behavior."
      //         << std::flush << std::endl;
      continue; // Simply skip the case where where the conversion is undefined.
    }


    /** Warning:
     * expectedValue == static_cast< TOutputPixelType( inValue ) is
     * false on some systems and compilers with some values of inValue. */
#if defined(__MINGW32__) // NOTE:  This may be the same problem identified above related to 'undefined' behavior
    if (itk::Math::NotAlmostEquals(outValue, expectedValue))
#else
    if (itk::Math::NotExactlyEquals(outValue, expectedValue))
#endif
    {
      std::cout << std::flush;
      std::cerr << "ERROR: staic_cast<" << GetCastTypeName<TOutputPixelType>() << ">(" << inValue << ") => " << outValue
                << " != " << expectedValue << std::flush << std::endl;
      success = false;
      break;
    }

    ++it;
    ++ot;
  }

  if (success)
  {
    std::cout << "[PASSED]" << std::endl;
  }
  else
  {
    std::cout << "[FAILED]" << std::endl;
  }

  return success;
}


template <typename TInputPixelType>
bool
TestCastFrom()
{
  bool success = true;
  success &= TestCastFromTo<TInputPixelType, char>();
  success &= TestCastFromTo<TInputPixelType, signed char>();
  success &= TestCastFromTo<TInputPixelType, unsigned char>();
  success &= TestCastFromTo<TInputPixelType, short>();
  success &= TestCastFromTo<TInputPixelType, unsigned short>();
  success &= TestCastFromTo<TInputPixelType, int>();
  success &= TestCastFromTo<TInputPixelType, unsigned int>();
  success &= TestCastFromTo<TInputPixelType, long>();
  success &= TestCastFromTo<TInputPixelType, unsigned long>();
  success &= TestCastFromTo<TInputPixelType, long long>();
  success &= TestCastFromTo<TInputPixelType, unsigned long long>();
  success &= TestCastFromTo<TInputPixelType, float>();
  success &= TestCastFromTo<TInputPixelType, double>();

  return success;
}

bool
TestVectorImageCast1()
{
  // This function casts a VectorImage<float, 2>
  // to a VectorImage<unsigned char, 2>
  std::cout << "Casting from a VectorImage<float, 2> to VectorImage<unsigned char, 2> ... ";

  using UnsignedCharVectorImageType = itk::VectorImage<unsigned char, 2>;
  using FloatVectorImageType = itk::VectorImage<float, 2>;

  // Create a 1x3 image of 2D vectors
  auto image = FloatVectorImageType::New();

  const itk::Size<2>  size{ { 1, 3 } };
  const itk::Index<2> start{ { 0, 0 } };

  itk::ImageRegion<2> region(start, size);
  image->SetNumberOfComponentsPerPixel(2);
  image->SetRegions(region);
  image->Allocate();
  itk::VariableLengthVector<float> vec;
  vec.SetSize(2);
  // All pixels will be the vector (1.3, 5.3)
  vec[0] = 1.3;
  vec[1] = 5.3;
  image->FillBuffer(vec);

  using CastImageFilterType = itk::CastImageFilter<FloatVectorImageType, UnsignedCharVectorImageType>;
  auto castImageFilter = CastImageFilterType::New();
  castImageFilter->SetInput(image);
  castImageFilter->Update();

  // Setup iterators for the original and casted images
  itk::ImageRegionConstIterator<UnsignedCharVectorImageType> castedImageIterator(
    castImageFilter->GetOutput(), castImageFilter->GetOutput()->GetLargestPossibleRegion());

  itk::ImageRegionConstIterator<FloatVectorImageType> originalImageIterator(image, image->GetLargestPossibleRegion());

  // Compare both dimensions of all of the pixels from the manually
  // casted original image to the corresponding pixels in the filter-casted
  // image
  bool success = true;
  while (!originalImageIterator.IsAtEnd())
  {
    if (static_cast<unsigned char>(originalImageIterator.Get()[0]) != castedImageIterator.Get()[0] ||
        static_cast<unsigned char>(originalImageIterator.Get()[1]) != castedImageIterator.Get()[1])
    {
      std::cerr << "Error in TestVectorImageCast1!" << std::endl;
      success = false;
    }
    ++originalImageIterator;
    ++castedImageIterator;
  }

  if (success)
  {
    std::cout << "[PASSED]" << std::endl;
  }
  else
  {
    std::cout << "[FAILED]" << std::endl;
  }

  return success;
}


bool
TestVectorImageCast2()
{
  // This function casts a VectorImage<float, 2>
  // to an Image<Vector<unsigned char, 2>, 2>
  std::cout << "Casting from a VectorImage<float, 2> to Image<Vector<unsigned char, 2>, 2> ... ";

  using UnsignedCharVectorImageType = itk::Image<itk::Vector<unsigned char, 2>, 2>;
  using FloatVectorImageType = itk::VectorImage<float, 2>;

  // Create a 1x3 image of 2D vectors
  auto image = FloatVectorImageType::New();

  const itk::Size<2>  size{ { 1, 3 } };
  const itk::Index<2> start{ { 0, 0 } };

  itk::ImageRegion<2> region(start, size);
  image->SetNumberOfComponentsPerPixel(2);
  image->SetRegions(region);
  image->Allocate();
  itk::VariableLengthVector<float> vec;
  vec.SetSize(2);
  // All pixels will be the vector (1.3, 5.3)
  vec[0] = 1.3;
  vec[1] = 5.3;
  image->FillBuffer(vec);

  using CastImageFilterType = itk::CastImageFilter<FloatVectorImageType, UnsignedCharVectorImageType>;
  auto castImageFilter = CastImageFilterType::New();
  castImageFilter->SetInput(image);
  castImageFilter->Update();

  // Setup iterators for the original and casted images
  itk::ImageRegionConstIterator<UnsignedCharVectorImageType> castedImageIterator(
    castImageFilter->GetOutput(), castImageFilter->GetOutput()->GetLargestPossibleRegion());

  itk::ImageRegionConstIterator<FloatVectorImageType> originalImageIterator(image, image->GetLargestPossibleRegion());

  // Compare both dimensions of all of the pixels from the manually
  // casted original image to the corresponding pixels in the filter-casted
  // image
  bool success = true;
  while (!originalImageIterator.IsAtEnd())
  {
    if (static_cast<unsigned char>(originalImageIterator.Get()[0]) != castedImageIterator.Get()[0] ||
        static_cast<unsigned char>(originalImageIterator.Get()[1]) != castedImageIterator.Get()[1])
    {
      std::cerr << "Error in TestVectorImageCast1!" << std::endl;
      success = false;
    }
    ++originalImageIterator;
    ++castedImageIterator;
  }

  if (success)
  {
    std::cout << "[PASSED]" << std::endl;
  }
  else
  {
    std::cout << "[FAILED]" << std::endl;
  }

  return success;
}


int
itkCastImageFilterTest(int, char *[])
{
  std::cout << "itkCastImageFilterTest Start" << std::endl;

  // This test casts floats to char, generating float point exceptions.
  // We disable float point exceptions only for this tests
  bool fpeSupport = itk::FloatingPointExceptions::HasFloatingPointExceptionsSupport();
  bool fpeStatus = itk::FloatingPointExceptions::GetEnabled();
  if (fpeSupport && fpeStatus)
  {
    std::cout << "FloatingPointExceptions are disabled only for this test." << std::endl;
    itk::FloatingPointExceptions::Disable();
  }

  bool success = true;
  success &= TestCastFrom<char>();
  success &= TestCastFrom<signed char>();
  success &= TestCastFrom<unsigned char>();
  success &= TestCastFrom<short>();
  success &= TestCastFrom<unsigned short>();
  success &= TestCastFrom<int>();
  success &= TestCastFrom<unsigned int>();
  success &= TestCastFrom<long>();
  success &= TestCastFrom<unsigned long>();
  success &= TestCastFrom<long long>();
  success &= TestCastFrom<unsigned long long>();
  success &= TestCastFrom<float>();
  success &= TestCastFrom<double>();
  success &= TestVectorImageCast1();
  success &= TestVectorImageCast2();

  std::cout << std::endl;
  if (!success)
  {
    std::cout << "An itkCastImageFilter test FAILED." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "All itkCastImageFilter tests PASSED." << std::endl;

  return EXIT_SUCCESS;
}
