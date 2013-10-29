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

//  Software Guide : BeginLatex
//
//  Resampling can also be performed in multi-component images.
//
//  \index{itk::VectorResampleImageFilter!Image internal transform}
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVectorResampleImageFilter.h"
#include "itkIdentityTransform.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkRGBPixel.h"


int main( int argc, char * argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int                          Dimension = 2;
  typedef unsigned char                       PixelComponentType;
  typedef itk::RGBPixel< PixelComponentType > PixelType;

  typedef itk::Image< PixelType,  Dimension >   ImageType;


  typedef itk::ImageFileReader< ImageType >  ReaderType;
  typedef itk::ImageFileWriter< ImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  typedef itk::VectorResampleImageFilter<
                            ImageType, ImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  typedef itk::VectorLinearInterpolateImageFunction<
                       ImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  filter->SetInterpolator( interpolator );


  typedef itk::IdentityTransform< double, Dimension >  TransformType;
  TransformType::Pointer transform = TransformType::New();

  filter->SetTransform( transform );


  // Software Guide : BeginCodeSnippet
  PixelType defaultValue;
  defaultValue.Fill(50);

  filter->SetDefaultPixelValue( defaultValue );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  ImageType::SpacingType spacing;
  spacing[0] = .5; // pixel spacing in millimeters along X
  spacing[1] = .5; // pixel spacing in millimeters along Y

  filter->SetOutputSpacing( spacing );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  ImageType::PointType origin;
  origin[0] = 30.0;  // X space coordinate of origin
  origin[1] = 40.0;  // Y space coordinate of origin
  filter->SetOutputOrigin( origin );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  ImageType::DirectionType direction;
  direction.SetIdentity();
  filter->SetOutputDirection( direction );
  // Software Guide : EndCodeSnippet


  ImageType::SizeType   size;

  size[0] = 300;  // number of pixels along X
  size[1] = 300;  // number of pixels along Y

  filter->SetSize( size );

  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );


  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    }

  return EXIT_SUCCESS;

}
