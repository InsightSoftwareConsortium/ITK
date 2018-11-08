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

#include "itkCastImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkVectorImage.h"
#include <iostream>
#include "itkFloatingPointExceptions.h"

// Better name demanging for gcc
#if __GNUC__ > 3 || ( __GNUC__ == 3 && __GNUC_MINOR__ > 0 )
#ifndef __EMSCRIPTEN__
#define GCC_USEDEMANGLE
#endif
#endif

#ifdef GCC_USEDEMANGLE
#include <cstdlib>
#include <cxxabi.h>
#include "itkMath.h"
#endif

template< typename T >
std::string GetCastTypeName()
{
  std::string name;
#ifdef GCC_USEDEMANGLE
  char const *mangledName = typeid( T ).name();
  int         status;
  char *      unmangled = abi::__cxa_demangle(mangledName, nullptr, nullptr, &status);
  name = unmangled;
  free(unmangled);
#else
  name = typeid( T ).name();
#endif

  return name;
}


template < typename TInputPixelType, typename TOutputPixelType >
bool TestCastFromTo()
{
  using InputImageType = itk::Image< TInputPixelType, 3 >;
  using OutputImageType = itk::Image< TOutputPixelType, 3 >;
  using FilterType = itk::CastImageFilter< InputImageType, OutputImageType >;

  using SourceType = itk::RandomImageSource< InputImageType >;
  typename SourceType::Pointer source = SourceType::New();

  typename InputImageType::SizeValueType randomSize[3] = {18, 17, 23};
  source->SetSize( randomSize );

  typename FilterType::Pointer filter = FilterType::New();
  filter->SetInput( source->GetOutput() );
  filter->UpdateLargestPossibleRegion();

  using InputIteratorType = itk::ImageRegionConstIterator< InputImageType >;
  using OutputIteratorType = itk::ImageRegionConstIterator< OutputImageType >;

  InputIteratorType  it( source->GetOutput(),
                         source->GetOutput()->GetLargestPossibleRegion() );
  OutputIteratorType ot( filter->GetOutput(),
                         filter->GetOutput()->GetLargestPossibleRegion() );

  bool success = true;

  std::cout << "Casting from " << GetCastTypeName< TInputPixelType >()
            << " to " << GetCastTypeName< TOutputPixelType >() << " ... ";

  it.GoToBegin();
  ot.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    TInputPixelType  inValue  = it.Value();
    TOutputPixelType outValue = ot.Value();
    auto expectedValue = static_cast< TOutputPixelType >( inValue );

    /** Warning:
     * expectedValue == static_cast< TOutputPixelType( inValue ) is
     * false on some systems and compilers with some values of inValue. */
#if defined(__MINGW32__)
    if ( itk::Math::NotAlmostEquals(outValue, expectedValue) )
#else
    if ( itk::Math::NotExactlyEquals(outValue, expectedValue) )
#endif
      {
      std::cerr << "ERROR: " << outValue << " != " << expectedValue << std::endl;
      success = false;
      break;
      }

    ++it;
    ++ot;
    }

  if ( success )
    {
    std::cout << "[PASSED]" << std::endl;
    }
  else
    {
    std::cout << "[FAILED]" << std::endl;
    }

  return success;
}


template < typename TInputPixelType >
bool TestCastFrom()
{
  bool success = true;
  success &= TestCastFromTo< TInputPixelType, char >();
  success &= TestCastFromTo< TInputPixelType, signed char >();
  success &= TestCastFromTo< TInputPixelType, unsigned char >();
  success &= TestCastFromTo< TInputPixelType, short >();
  success &= TestCastFromTo< TInputPixelType, unsigned short >();
  success &= TestCastFromTo< TInputPixelType, int >();
  success &= TestCastFromTo< TInputPixelType, unsigned int >();
  success &= TestCastFromTo< TInputPixelType, long >();
  success &= TestCastFromTo< TInputPixelType, unsigned long >();
  success &= TestCastFromTo< TInputPixelType, long long >();
  success &= TestCastFromTo< TInputPixelType, unsigned long long >();
  success &= TestCastFromTo< TInputPixelType, float >();
  success &= TestCastFromTo< TInputPixelType, double >();

  return success;
}

bool TestVectorImageCast()
{
  // This function casts a VectorImage<float, 2>
  // to a VectorImage<unsigned char, 2>
  std::cout << "Casting from a VectorImage<float, 2> \
                to VectorImage<unsigned char, 2> ..." << std::endl;

  using UnsignedCharVectorImageType = itk::VectorImage<unsigned char, 2>;
  using FloatVectorImageType = itk::VectorImage<float, 2>;

  // Create a 1x3 image of 2D vectors
  FloatVectorImageType::Pointer image = FloatVectorImageType::New();

  itk::Size<2> size;
  size[0] = 1;
  size[1] = 3;

  itk::Index<2> start;
  start.Fill(0);

  itk::ImageRegion<2> region(start,size);
  image->SetNumberOfComponentsPerPixel(2);
  image->SetRegions(region);
  image->Allocate();
  itk::VariableLengthVector<float> vec;
  vec.SetSize(2);
  // All pixels will be the vector (1.3, 5.3)
  vec[0] = 1.3;
  vec[1] = 5.3;
  image->FillBuffer(vec);

  using CastImageFilterType = itk::CastImageFilter< FloatVectorImageType,
               UnsignedCharVectorImageType >;
  CastImageFilterType::Pointer castImageFilter = CastImageFilterType::New();
  castImageFilter->SetInput(image);
  castImageFilter->Update();

  // Setup iterators for the original and casted images
  itk::ImageRegionConstIterator<UnsignedCharVectorImageType>
       castedImageIterator(castImageFilter->GetOutput(),
       castImageFilter->GetOutput()->GetLargestPossibleRegion());

  itk::ImageRegionConstIterator<FloatVectorImageType>
    originalImageIterator(image, image->GetLargestPossibleRegion());

  // Compare both dimensions of all of the pixels from the manually
  // casted original image to the corresponding pixels in the filter-casted
  // image
  bool success = true;
  while(!originalImageIterator.IsAtEnd())
    {
    if(static_cast<unsigned char>(originalImageIterator.Get()[0]) !=
                                  castedImageIterator.Get()[0] ||
       static_cast<unsigned char>(originalImageIterator.Get()[1]) !=
                                  castedImageIterator.Get()[1])
      {
      std::cerr << "Error in TestVectorImageCast!" << std::endl;
      success = false;
      }
    ++originalImageIterator;
    ++castedImageIterator;
    }

  if ( success )
    {
    std::cout << "[PASSED]" << std::endl;
    }
  else
    {
    std::cout << "[FAILED]" << std::endl;
    }

  return success;
}


int itkCastImageFilterTest( int, char* [] )
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
  success &= TestCastFrom< char >();
  success &= TestCastFrom< signed char >();
  success &= TestCastFrom< unsigned char >();
  success &= TestCastFrom< short >();
  success &= TestCastFrom< unsigned short >();
  success &= TestCastFrom< int >();
  success &= TestCastFrom< unsigned int >();
  success &= TestCastFrom< long >();
  success &= TestCastFrom< unsigned long >();
  success &= TestCastFrom< long long >();
  success &= TestCastFrom< unsigned long long >();
  success &= TestCastFrom< float >();
  success &= TestCastFrom< double >();
  success &= TestVectorImageCast();

  std::cout << std::endl;
  if ( !success )
    {
    std::cout << "An itkCastImageFilter test FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "All itkCastImageFilter tests PASSED." << std::endl;

  return EXIT_SUCCESS;
}
