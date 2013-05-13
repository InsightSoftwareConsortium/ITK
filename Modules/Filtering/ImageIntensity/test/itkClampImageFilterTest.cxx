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

#include "itkClampImageFilter.h"
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
std::string GetClampTypeName()
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
bool TestClampFromTo()
{
  typedef itk::Image< TInputPixelType, 3 >                        InputImageType;
  typedef itk::Image< TOutputPixelType, 3 >                       OutputImageType;
  typedef itk::ClampImageFilter< InputImageType, OutputImageType > FilterType;

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

  std::cout << "Casting from " << GetClampTypeName< TInputPixelType >()
            << " to " << GetClampTypeName< TOutputPixelType >() << " ... ";

  it.GoToBegin();
  ot.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    TInputPixelType  inValue       = it.Value();
    TOutputPixelType outValue      = ot.Value();
    TOutputPixelType expectedValue;

    double dInValue = static_cast< double >( inValue );
    double expectedMin = itk::NumericTraits< TOutputPixelType >::NonpositiveMin();
    double expectedMax = itk::NumericTraits< TOutputPixelType >::max();

    if ( dInValue < expectedMin )
      {
      expectedValue = itk::NumericTraits< TOutputPixelType >::NonpositiveMin();
      }
    else if ( dInValue > expectedMax )
      {
      expectedValue = itk::NumericTraits< TOutputPixelType >::max();
      }
    else
      {
      expectedValue = static_cast< TOutputPixelType >( inValue );
      }

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
bool TestClampFrom()
{
  bool success =
    TestClampFromTo< TInputPixelType, char >() &&
    TestClampFromTo< TInputPixelType, unsigned char >() &&
    TestClampFromTo< TInputPixelType, short >() &&
    TestClampFromTo< TInputPixelType, unsigned short >() &&
    TestClampFromTo< TInputPixelType, int >() &&
    TestClampFromTo< TInputPixelType, unsigned int >() &&
    TestClampFromTo< TInputPixelType, long >() &&
    TestClampFromTo< TInputPixelType, unsigned long >() &&
// Visual Studio has a false failure in due to
// imprecise integer to double conversion. It causes the comparison
// dInValue > expectedMax to pass when it should fail.
#ifndef _MSC_VER
    TestClampFromTo< TInputPixelType, long long >() &&
    TestClampFromTo< TInputPixelType, unsigned long long >() &&
#endif
    TestClampFromTo< TInputPixelType, float >() &&
    TestClampFromTo< TInputPixelType, double >();

  return success;
}

template < class TInputPixelType, class TOutputPixelType >
bool TestClampFromToWithCustomBounds()
{
  typedef itk::Image< TInputPixelType, 3 >                        InputImageType;
  typedef itk::Image< TOutputPixelType, 3 >                       OutputImageType;
  typedef itk::ClampImageFilter< InputImageType, OutputImageType > FilterType;

  typedef itk::RandomImageSource< InputImageType > SourceType;
  typename SourceType::Pointer source = SourceType::New();
  source->SetMin(static_cast< TInputPixelType >(0));
  source->SetMax(static_cast< TInputPixelType >(20));

  typename InputImageType::SizeValueType randomSize[3] = {18, 17, 23};
  source->SetSize( randomSize );

  typename FilterType::Pointer filter = FilterType::New();
  filter->SetBounds(static_cast< TOutputPixelType>(5), static_cast< TOutputPixelType>(15));
  filter->SetInput( source->GetOutput() );
  filter->UpdateLargestPossibleRegion();

  typedef itk::ImageRegionConstIterator< InputImageType >  InputIteratorType;
  typedef itk::ImageRegionConstIterator< OutputImageType > OutputIteratorType;

  InputIteratorType  it( source->GetOutput(),
                         source->GetOutput()->GetLargestPossibleRegion() );
  OutputIteratorType ot( filter->GetOutput(),
                         filter->GetOutput()->GetLargestPossibleRegion() );

  bool success = true;

  std::cout << "Casting from " << GetClampTypeName< TInputPixelType >()
            << " to " << GetClampTypeName< TOutputPixelType >() << " using custom bounds ... ";

  it.GoToBegin();
  ot.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    TInputPixelType  inValue       = it.Value();
    TOutputPixelType outValue      = ot.Value();
    TOutputPixelType expectedValue;

    double dInValue = static_cast< double >( inValue );
    double expectedMin = filter->GetLowerBound();
    double expectedMax = filter->GetUpperBound();


    if ( dInValue < expectedMin )
      {
      expectedValue = filter->GetLowerBound();
      }
    else if ( dInValue > expectedMax )
      {
      expectedValue = filter->GetUpperBound();
      }
    else
      {
      expectedValue = static_cast< TOutputPixelType >( inValue );
      }

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
bool TestClampFromWithCustomBounds()
{
  bool success =
    TestClampFromToWithCustomBounds< TInputPixelType, char >() &&
    TestClampFromToWithCustomBounds< TInputPixelType, unsigned char >() &&
    TestClampFromToWithCustomBounds< TInputPixelType, short >() &&
    TestClampFromToWithCustomBounds< TInputPixelType, unsigned short >() &&
    TestClampFromToWithCustomBounds< TInputPixelType, int >() &&
    TestClampFromToWithCustomBounds< TInputPixelType, unsigned int >() &&
    TestClampFromToWithCustomBounds< TInputPixelType, long >() &&
    TestClampFromToWithCustomBounds< TInputPixelType, unsigned long >() &&
    TestClampFromToWithCustomBounds< TInputPixelType, long long >() &&
    TestClampFromToWithCustomBounds< TInputPixelType, unsigned long long >() &&
    TestClampFromToWithCustomBounds< TInputPixelType, float >() &&
    TestClampFromToWithCustomBounds< TInputPixelType, double >();

  return success;
}


int itkClampImageFilterTest( int, char* [] )
{
  std::cout << "itkClampImageFilterTest Start" << std::endl;

  bool success =
    TestClampFrom< char >() &&
    TestClampFrom< unsigned char >() &&
    TestClampFrom< short >() &&
    TestClampFrom< unsigned short >() &&
    TestClampFrom< int >() &&
    TestClampFrom< unsigned int >() &&
    TestClampFrom< long >() &&
    TestClampFrom< unsigned long >() &&
    TestClampFrom< long long >() &&
    TestClampFrom< unsigned long long >() &&
    TestClampFrom< float >() &&
    TestClampFrom< double >() &&

    TestClampFromWithCustomBounds< char >() &&
    TestClampFromWithCustomBounds< unsigned char >() &&
    TestClampFromWithCustomBounds< short >() &&
    TestClampFromWithCustomBounds< unsigned short >() &&
    TestClampFromWithCustomBounds< int >() &&
    TestClampFromWithCustomBounds< unsigned int >() &&
    TestClampFromWithCustomBounds< long >() &&
    TestClampFromWithCustomBounds< unsigned long >() &&
    TestClampFromWithCustomBounds< long long >() &&
    TestClampFromWithCustomBounds< unsigned long long >() &&
    TestClampFromWithCustomBounds< float >() &&
    TestClampFromWithCustomBounds< double >();

  std::cout << std::endl;
  if ( !success )
    {
    std::cout << "An itkClampImageFilter test FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "All itkClampImageFilter tests PASSED." << std::endl;

  return EXIT_SUCCESS;
}
