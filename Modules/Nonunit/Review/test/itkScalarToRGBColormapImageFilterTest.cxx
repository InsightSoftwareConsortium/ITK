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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"

#include "itkRedColormapFunction.h"
#include "itkGreenColormapFunction.h"
#include "itkBlueColormapFunction.h"
#include "itkGreyColormapFunction.h"
#include "itkHotColormapFunction.h"
#include "itkCoolColormapFunction.h"
#include "itkSpringColormapFunction.h"
#include "itkSummerColormapFunction.h"
#include "itkAutumnColormapFunction.h"
#include "itkWinterColormapFunction.h"
#include "itkCopperColormapFunction.h"
#include "itkHSVColormapFunction.h"
#include "itkJetColormapFunction.h"
#include "itkCustomColormapFunction.h"
#include "itkOverUnderColormapFunction.h"

#include "itkScalarToRGBColormapImageFilter.h"


int itkScalarToRGBColormapImageFilterTest( int argc, char *argv[] )
{
  if ( argc < 4 )
    {
    std::cout << "Usage: " << argv[0] <<
      " inputImage outputImage colormap [customColormapFile]" << std::endl;
    std::cout <<
      "  Possible colormaps: grey, red, green, blue, copper, jet, hsv, ";
    std::cout <<
      "spring, summer, autumn, winter, hot, cool, custom" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int ImageDimension = 2;

  typedef unsigned int                    PixelType;
  typedef itk::RGBPixel<unsigned char>    RGBPixelType;

  typedef itk::Image<PixelType, ImageDimension>     ImageType;
  typedef itk::Image<float, ImageDimension>         RealImageType;
  typedef itk::Image<RGBPixelType, ImageDimension>  RGBImageType;

  typedef itk::ImageFileReader<ImageType>           ReaderType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();

  std::string colormapString( argv[3] );

  typedef itk::ScalarToRGBColormapImageFilter<ImageType, RGBImageType> RGBFilterType;
  RGBFilterType::Pointer rgbfilter = RGBFilterType::New();

  rgbfilter->Print( std::cout );

  rgbfilter->SetInput( reader->GetOutput() );

  if ( colormapString == "red" )
    {
    rgbfilter->SetColormap( RGBFilterType::Red );
    }
  else if ( colormapString == "green"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Green );
    }
  else if ( colormapString == "blue"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Blue );
    }
  else if ( colormapString == "grey"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Grey );
    }
  else if ( colormapString == "cool"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Cool );
    }
  else if ( colormapString == "hot"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Hot );
    }
  else if ( colormapString == "spring"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Spring );
    }
  else if ( colormapString == "autumn"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Autumn );
    }
  else if ( colormapString == "winter"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Winter );
    }
  else if ( colormapString == "copper"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Copper );
    }
  else if ( colormapString == "summer"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Summer );
    }
  else if ( colormapString == "jet"  )
    {
    typedef itk::Function::JetColormapFunction<
      ImageType::PixelType, RGBImageType::PixelType> ColormapType;

    ColormapType::Pointer colormap = ColormapType::New();

    rgbfilter->SetColormap( colormap );
    }
  else if ( colormapString == "hsv"  )
    {
    typedef itk::Function::HSVColormapFunction<
      ImageType::PixelType, RGBImageType::PixelType> ColormapType;

    ColormapType::Pointer colormap = ColormapType::New();
    rgbfilter->SetColormap( colormap );
    }
  else if ( colormapString == "overunder"  )
    {
    rgbfilter->SetColormap( RGBFilterType::OverUnder );
    }
  else if ( colormapString == "custom"  )
    {
    typedef itk::Function::CustomColormapFunction<
      ImageType::PixelType, RGBImageType::PixelType> ColormapType;

    ColormapType::Pointer colormap = ColormapType::New();

    std::ifstream str( argv[4] );
    std::string line;

    float value;
    ColormapType::ChannelType channel;

    // Get red values
    std::getline( str, line );
    std::istringstream issr( line );
    while ( issr >> value )
      {
      channel.push_back( value );
      }
    colormap->SetRedChannel( channel );

    // Get green values
    std::getline( str, line );
    std::istringstream issg( line );
    while ( issg >> value )
      {
      channel.push_back( value );
      }
    colormap->SetGreenChannel( channel );

    // Get blue values
    std::getline( str, line );
    std::istringstream issb( line );
    while ( issb >> value )
      {
      channel.push_back( value );
      }
    colormap->SetBlueChannel( channel );
    rgbfilter->SetColormap( colormap );
    }

  rgbfilter->GetColormap()->SetMinimumRGBComponentValue( 0 );
  rgbfilter->GetColormap()->SetMaximumRGBComponentValue( 255 );

  try
    {
    rgbfilter->Update();
    }
  catch (...)
    {
    return EXIT_FAILURE;
    }

  typedef itk::ImageFileWriter<RGBImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( rgbfilter->GetOutput() );
  writer->Update();

  return EXIT_SUCCESS;
}
