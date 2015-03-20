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


#include "itkCustomColormapFunction.h"
#include "itkImageFileReader.h"
#include "itkTestingMacros.h"


namespace itk {
namespace Function {

/**
 *  This test exercises the functionality of the itk::Function::CustomColormapFunction
 *  class.
 *
 */

template<typename TScalar, typename TRGBPixel>
class CustomColormapFunctionHelper : public CustomColormapFunction< TScalar, TRGBPixel >
{
public:
  typedef CustomColormapFunctionHelper                Self;
  typedef CustomColormapFunction<TScalar, TRGBPixel>  Superclass;
  typedef SmartPointer<Self>                          Pointer;
  typedef SmartPointer<const Self>                    ConstPointer;


  typedef TScalar                                           ScalarType;
  typedef TRGBPixel                                         RGBPixelType;
  typedef typename NumericTraits< ScalarType >::RealType    RealType;

  itkTypeMacro( CustomColormapFunctionHelper, CustomColormapFunction );

  itkNewMacro(Self);

  static int Exercise( std::vector<RealType> redChannel,
    std::vector<RealType> greenChannel,
    std::vector<RealType> blueChannel,
    ScalarType val)
  {

    typedef itk::Function::CustomColormapFunction<
      ScalarType, RGBPixelType> ColormapType;

    typename ColormapType::Pointer colormap = ColormapType::New();

    // Set the RGB channels
    colormap->SetRedChannel(redChannel);
    colormap->SetGreenChannel(greenChannel);
    colormap->SetBlueChannel(blueChannel);

    // Test the Get/Set functions
    for (unsigned int i = 0; i < redChannel.size(); ++i )
      {
      TEST_SET_GET_VALUE(redChannel[i], colormap->GetRedChannel()[i]);
      }
    for (unsigned int i = 0; i < greenChannel.size(); ++i )
      {
      TEST_SET_GET_VALUE(greenChannel[i], colormap->GetGreenChannel()[i]);
      }
    for (unsigned int i = 0; i < blueChannel.size(); ++i )
      {
      TEST_SET_GET_VALUE(blueChannel[i], colormap->GetBlueChannel()[i]);
      }

    // Set the maximum and minimum input values
    colormap->SetMinimumInputValue(0);
    colormap->SetMaximumInputValue(127);

    // Map the scalar value into an RGB colormap value
    RGBPixelType pixel = colormap->operator()(val);

    std::cout << "RGB colormap for value " << (int) val << std::endl;
    std::cout << "R: " << (int) pixel[0] << "; G: " << (int) pixel[1] <<
      "; B: " << (int) pixel[2] << std::endl;

    std::cout << "Test succeeded." << std::endl;
    return EXIT_SUCCESS;

  }

protected:

};

} // end namespace Function
} // end namespace itk


int itkCustomColormapFunctionTest( int argc, char* argv[] )
{

  if ( argc < 2 )
    {
    std::cout << "Usage: " << argv[0] <<
      " customColormapFile scalarValue" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::RGBPixel<unsigned char>    RGBPixelType;

  double value;
  std::vector<double> redChannel, greenChannel, blueChannel;

  std::ifstream str( argv[1] );
  std::string line;

  // Get red values
  std::getline( str, line );
  std::istringstream issr( line );
  while ( issr >> value )
    {
    redChannel.push_back( value );
    }

  // Get green values
  std::getline( str, line );
  std::istringstream issg( line );
  while ( issg >> value )
    {
    greenChannel.push_back( value );
    }

  // Get blue values
  std::getline( str, line );
  std::istringstream issb( line );
  while ( issb >> value )
    {
    blueChannel.push_back( value );
    }

  // The scalar valur to be mapped into an RGB colormap value
  int scalarValue = atoi(argv[2]);

  // Test for all possible scalar pixel types
  itk::Function::CustomColormapFunctionHelper<unsigned char, RGBPixelType>::Exercise(
    redChannel,
    greenChannel,
    blueChannel,
    scalarValue);

  itk::Function::CustomColormapFunctionHelper<char, RGBPixelType>::Exercise(
    redChannel,
    greenChannel,
    blueChannel,
    scalarValue);

  itk::Function::CustomColormapFunctionHelper<unsigned short, RGBPixelType>::Exercise(
    redChannel,
    greenChannel,
    blueChannel,
    scalarValue);

  itk::Function::CustomColormapFunctionHelper<short, RGBPixelType>::Exercise(
    redChannel,
    greenChannel,
    blueChannel,
    scalarValue);

  itk::Function::CustomColormapFunctionHelper<unsigned int, RGBPixelType>::Exercise(
    redChannel,
    greenChannel,
    blueChannel,
    scalarValue);

  itk::Function::CustomColormapFunctionHelper<int, RGBPixelType>::Exercise(
    redChannel,
    greenChannel,
    blueChannel,
    scalarValue);

  itk::Function::CustomColormapFunctionHelper<unsigned long, RGBPixelType>::Exercise(
    redChannel,
    greenChannel,
    blueChannel,
    scalarValue);

  itk::Function::CustomColormapFunctionHelper<long, RGBPixelType>::Exercise(
    redChannel,
    greenChannel,
    blueChannel,
    scalarValue);

  itk::Function::CustomColormapFunctionHelper<float, RGBPixelType>::Exercise(
    redChannel,
    greenChannel,
    blueChannel,
    scalarValue);

  itk::Function::CustomColormapFunctionHelper<double, RGBPixelType>::Exercise(
    redChannel,
    greenChannel,
    blueChannel,
    scalarValue);

  return EXIT_SUCCESS;

}
