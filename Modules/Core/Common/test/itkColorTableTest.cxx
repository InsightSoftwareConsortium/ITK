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

#include "itkColorTable.h"
#include "itkTestingMacros.h"


template< typename TPixel >
int ColorTableTestExpectedConditionChecker(
  typename itk::ColorTable< TPixel >::Pointer colors, unsigned int colorId,
  itk::RGBPixel< TPixel > rgbPixel, const char* name )
{
  if( colors->GetColor( colorId ) != rgbPixel )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::ColorTable::GetColor" << std::endl;
    std::cerr << "Expected: " << rgbPixel << ", but got: "
      << colors->GetColor( colorId ) << std::endl;
    return EXIT_FAILURE;
    }
  if( colors->GetColorName( colorId ) != name )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::ColorTable::GetColorName" << std::endl;
    std::cerr << "Expected: " << name << ", but got: "
      << colors->GetColorName( colorId ) << std::endl;
    return EXIT_FAILURE;
    }

  std::string rgb = "rgb";
  for( unsigned int i = 0; i < rgb.length(); ++i )
    {
    TPixel resultPixelComponentValue =
      colors->GetColorComponent( colorId, rgb.at(i) );
    if( resultPixelComponentValue != rgbPixel[i] )
      {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in itk::ColorTable::GetColorComponent" << std::endl;
      std::cerr << "Expected: " << rgbPixel.GetNthComponent( i ) <<", but got: "
        << resultPixelComponentValue << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}

template< typename TPixel >
int ColorTableTestSpecialConditionChecker(
  typename itk::ColorTable< TPixel >::Pointer colors, unsigned int numberOfColors )
{
  typedef itk::RGBPixel< TPixel > RGBPixelType;

  RGBPixelType zeroPixel;
  zeroPixel.Set( 0, 0, 0 );

  if( colors->GetNumberOfColors() != numberOfColors )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::ColorTable::GetNumberOfColors" << std::endl;
    std::cerr << "Expected: " << numberOfColors << ", but got: "
      << colors->GetNumberOfColors() << std::endl;
    return EXIT_FAILURE;
    }

  RGBPixelType pixel = colors->GetColor( numberOfColors );
  if( pixel != zeroPixel )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::ColorTable::GetColor" << std::endl;
    std::cerr << "Expected: " << zeroPixel << ", but got: "
      << pixel << std::endl;
    return EXIT_FAILURE;
    }

  bool tf = colors->SetColor( numberOfColors, 0, 0, 0, "NoMatterTheName" );
  if( tf != false )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::ColorTable::SetColor" << std::endl;
    std::cerr << "Expected: " << false << ", but got: "
      << tf << std::endl;
    return EXIT_FAILURE;
    }

  std::string name = colors->GetColorName( numberOfColors );
  if( name != "" )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::ColorTable::GetColorName" << std::endl;
    std::cerr << "Expected: \"\" (empty string) " << ", but got: "
      << name << std::endl;
    return EXIT_FAILURE;
    }

  char rgb = 'r';
  TPixel zeroComponent( 0 );
  pixel = colors->GetColorComponent( numberOfColors, rgb );
  if( pixel != zeroComponent )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::ColorTable::GetColorComponent" << std::endl;
    std::cerr << "Expected: 0 " << ", but got: "
      << pixel << std::endl;
    return EXIT_FAILURE;
    }
  rgb = 'a';
  pixel = colors->GetColorComponent( numberOfColors - 1, rgb );
  if( pixel != zeroComponent )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::ColorTable::GetColorComponent" << std::endl;
    std::cerr << "Expected: 0 " << ", but got: "
      << pixel << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

template< typename TPixel >
int ColorTableTestHelper( const char *name, unsigned int numberOfColors )
{
  int testStatus = EXIT_SUCCESS;

  typedef itk::ColorTable< TPixel > ColorTableType;
  typename ColorTableType::Pointer colors = ColorTableType::New();

  std::cout << "Testing for type: " << name << std::endl;

  std::cout << "Random Colors" << std::endl;
  colors->UseRandomColors( numberOfColors );
  colors->Print( std::cout );
  std::cout << std::endl;
  testStatus |= ColorTableTestSpecialConditionChecker< TPixel >( colors, numberOfColors );

  std::cout << "Heat Colors" << std::endl;
  colors->UseHeatColors( numberOfColors );
  colors->Print( std::cout );
  std::cout << std::endl;
  testStatus |= ColorTableTestSpecialConditionChecker< TPixel >( colors, numberOfColors );

  std::cout << "Gray Colors" << std::endl;
  colors->UseGrayColors( numberOfColors );
  colors->Print( std::cout );
  std::cout << std::endl;
  testStatus |= ColorTableTestSpecialConditionChecker< TPixel >( colors, numberOfColors );

  colors->UseDiscreteColors();
  std::cout << "Discrete Colors" << std::endl;
  colors->Print(std::cout);
  std::cout << std::endl;
  testStatus |= ColorTableTestSpecialConditionChecker< TPixel >( colors, 8 );

  return testStatus;
}

int itkColorTableTest( int argc, char* argv[] )
{
  if( argc != 2 )
    {
    std::cerr << "Usage: " << argv[0] << " numberOfColors" << std::endl;
    return EXIT_FAILURE;
    }

  int testStatus = EXIT_SUCCESS;

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  typedef itk::ColorTable< unsigned char > ColorTableType;
  ColorTableType::Pointer colors = ColorTableType::New();

  EXERCISE_BASIC_OBJECT_METHODS( colors, ColorTable, Object );

  unsigned int numberOfColors = static_cast< unsigned int >( atoi( argv[1] ) );

  testStatus |= ColorTableTestHelper< unsigned char >( "unsigned char", numberOfColors );
  testStatus |= ColorTableTestHelper< char >( "char", numberOfColors );
  testStatus |= ColorTableTestHelper< unsigned short >( "unsigned short", numberOfColors );
  testStatus |= ColorTableTestHelper< short >( "short", numberOfColors );
  testStatus |= ColorTableTestHelper< unsigned int >( "unsigned int", numberOfColors );
  testStatus |= ColorTableTestHelper< int >( "int", numberOfColors );
  testStatus |= ColorTableTestHelper< unsigned long >( "unsigned long", numberOfColors );
  testStatus |= ColorTableTestHelper< long >( "long", numberOfColors );
  testStatus |= ColorTableTestHelper< unsigned long long>("unsigned long long", numberOfColors);
  testStatus |= ColorTableTestHelper< long  long>("long long", numberOfColors);
  testStatus |= ColorTableTestHelper< float >( "float", numberOfColors );
  testStatus |= ColorTableTestHelper< double >( "double", numberOfColors );

  // Find the closest color for a few colors
  unsigned int id;
  typedef itk::RGBPixel< unsigned char > RGBPixelType;
  RGBPixelType pixel;
  colors->UseRandomColors( 10000 );
  pixel.Set( 255, 0, 0 );
  id = colors->GetClosestColorTableId( pixel[0], pixel[1], pixel[2] );
  std::cout << "Pixel : " << pixel
            << " is closest to id: " << id
            << " which has the color: " << colors->GetColor( id )
            << " and name " << colors->GetColorName( id ) << std::endl;

  colors->UseDiscreteColors();
  pixel.Set( 255, 0, 0 );
  id = colors->GetClosestColorTableId( pixel[0], pixel[1], pixel[2] );
  std::cout << "Pixel : " << pixel
            << " is closest to id: " << id
            << " which has the color: " << colors->GetColor( id )
            << " and name " << colors->GetColorName( id ) << std::endl;

  colors->UseGrayColors();
  pixel.Set( 17, 17, 17 );
  id = colors->GetClosestColorTableId( pixel[0], pixel[1], pixel[2] );
  std::cout << "Pixel : " << pixel
            << " is closest to id: " << id
            << " which has the color: " << colors->GetColor( id )
            << " and name " << colors->GetColorName( id ) << std::endl;

  // Check for degenerate case
  colors->UseGrayColors( 1 );
  colors->Print( std::cout );

  // Exercise the SetColorMethod
  colors->UseRandomColors( 4 );
  const char* name = "Background";
  bool tf = colors->SetColor( 0, 0, 0, 0, name );
  if( tf != true )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::ColorTable::SetColor" << std::endl;
    std::cerr << "Expected: " << true << ", but got: "
      << tf << std::endl;
    return EXIT_FAILURE;
    }
  pixel.Set( 0, 0, 0 );
  unsigned int colorId = 0;
  testStatus |= ColorTableTestExpectedConditionChecker< unsigned char >(
    colors, colorId, pixel, name );

  name = "Red";
  tf = colors->SetColor( 1, 255, 0, 0, name );
  if( tf != true )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::ColorTable::SetColor" << std::endl;
    std::cerr << "Expected: " << true << ", but got: "
      << tf << std::endl;
    return EXIT_FAILURE;
    }
  pixel.Set( 255, 0, 0 );
  colorId = 1;
  testStatus |= ColorTableTestExpectedConditionChecker< unsigned char >(
    colors, colorId, pixel, name );

  name = "Yellow";
  tf = colors->SetColor( 2, 255, 255, 0, name );
  if( tf != true )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::ColorTable::SetColor" << std::endl;
    std::cerr << "Expected: " << true << ", but got: "
      << tf << std::endl;
    return EXIT_FAILURE;
    }
  pixel.Set( 255, 255, 0 );
  colorId = 2;
  testStatus |= ColorTableTestExpectedConditionChecker< unsigned char >(
    colors, colorId, pixel, name );

  pixel.Set( 255, 255, 255 );
  name = "White";
  tf = colors->SetColor( 3, pixel, name );
  if( tf != true )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::ColorTable::SetColor" << std::endl;
    std::cerr << "Expected: " << true << ", but got: "
      << tf << std::endl;
    return EXIT_FAILURE;
    }
  colorId = 3;
  testStatus |= ColorTableTestExpectedConditionChecker< unsigned char >(
    colors, colorId, pixel, name );

  colors->Print(std::cout);

  std::cout << "Test finished " << std::endl;
  return testStatus;
}
