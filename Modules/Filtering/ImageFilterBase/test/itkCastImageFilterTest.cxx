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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkImage.h"
#include "itkCastImageFilter.h"
#include "itkRandomImageSource.h"

// Better name demanging for gcc
#if __GNUC__ > 3 || ( __GNUC__ == 3 && __GNUC_MINOR__ > 0 )
#define GCC_USEDEMANGLE
#endif

#ifdef GCC_USEDEMANGLE
#include <cstdlib>
#include <cxxabi.h>
#endif

template< class T >
std::string GetCastTypeName()
{
  std::string name;
#ifdef GCC_USEDEMANGLE
  char const *mangledName = typeid( T ).name();
  int         status;
  char *      unmangled = abi::__cxa_demangle(mangledName, 0, 0, &status);
  name = unmangled;
  free(unmangled);
#else
  name = typeid( T ).name();
#endif

  return name;
}


template < class TInputPixelType, class TOutputPixelType >
bool TestCastFromTo()
{
  typedef itk::Image< TInputPixelType, 3 >                        InputImageType;
  typedef itk::Image< TOutputPixelType, 3 >                       OutputImageType;
  typedef itk::CastImageFilter< InputImageType, OutputImageType > FilterType;

  typedef itk::RandomImageSource< InputImageType > SourceType;
  typename SourceType::Pointer source = SourceType::New();

  typename InputImageType::SizeValueType randomSize[3] = {18, 17, 23};
  source->SetSize( randomSize );

  typename FilterType::Pointer filter = FilterType::New();
  filter->SetInput( source->GetOutput() );
  filter->UpdateLargestPossibleRegion();

  typedef itk::ImageRegionConstIterator< InputImageType >  InputIteratorType;
  typedef itk::ImageRegionConstIterator< OutputImageType > OutputIteratorType;

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
    TOutputPixelType expectedValue = static_cast< TOutputPixelType >( inValue );

    /** Warning:
     * expectedValue == static_cast< TOutputPixelType( inValue ) is
     * false on some systems and compilers with some values of inValue. */
    if ( outValue != expectedValue )
      {
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


template < class TInputPixelType >
bool TestCastFrom()
{
  bool success =
    TestCastFromTo< TInputPixelType, char >() &&
    TestCastFromTo< TInputPixelType, unsigned char >() &&
    TestCastFromTo< TInputPixelType, short >() &&
    TestCastFromTo< TInputPixelType, unsigned short >() &&
    TestCastFromTo< TInputPixelType, int >() &&
    TestCastFromTo< TInputPixelType, unsigned int >() &&
    TestCastFromTo< TInputPixelType, long >() &&
    TestCastFromTo< TInputPixelType, unsigned long >() &&
    TestCastFromTo< TInputPixelType, long long >() &&
    TestCastFromTo< TInputPixelType, unsigned long long >() &&
    TestCastFromTo< TInputPixelType, float >() &&
    TestCastFromTo< TInputPixelType, double >();

  return success;
}


int itkCastImageFilterTest( int, char* [] )
{
  std::cout << "itkCastImageFilterTest Start" << std::endl;

  bool success =
    TestCastFrom< char >() &&
    TestCastFrom< unsigned char >() &&
    TestCastFrom< short >() &&
    TestCastFrom< unsigned short >() &&
    TestCastFrom< int >() &&
    TestCastFrom< unsigned int >() &&
    TestCastFrom< long >() &&
    TestCastFrom< unsigned long >() &&
    TestCastFrom< long long >() &&
    TestCastFrom< unsigned long long >() &&
    TestCastFrom< float >() &&
    TestCastFrom< double >();

  std::cout << std::endl;
  if ( !success )
    {
    std::cout << "An itkCastImageFilter test FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "All itkCastImageFilter tests PASSED." << std::endl;

  return EXIT_SUCCESS;
}
