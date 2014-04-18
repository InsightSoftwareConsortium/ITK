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
//  The following example illustrates how to use the
//  \doxygen{BSplineInterpolateImageFunction} for resampling an image.  In this
//  particular case an \doxygen{AffineTransform} is used to map the input space
//  into the output space.
//
//  \index{itk::AffineTransform!resampling}
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkBSplineInterpolateImageFunction.h"

//  Software Guide : BeginLatex
//
//  The header of the affine transform is included below.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkAffineTransform.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  degrees" << std::endl;
    return EXIT_FAILURE;
    }

  const     unsigned int   Dimension = 2;
  typedef   unsigned char  InputPixelType;
  typedef   unsigned char  OutputPixelType;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  const double angleInDegrees = atof( argv[3] );

  //  Software Guide : BeginLatex
  //
  //  The Resampling filter is instantiated and created just like in previous examples.
  //  The Transform is instantiated and connected to the resampling filter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ResampleImageFilter<
                  InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  typedef itk::AffineTransform< double, Dimension >  TransformType;

  TransformType::Pointer transform = TransformType::New();

  filter->SetTransform( transform );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The salient feature of this example is the use of the
  //  \doxygen{BSplineInterpolateImageFunction}, which uses cubic BSplines in
  //  order to interpolate the resampled image.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::BSplineInterpolateImageFunction<
                       InputImageType, double >  InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  filter->SetInterpolator( interpolator );

  filter->SetDefaultPixelValue( 100 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The parameters of the output image are taken from the input image.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  reader->Update();
  const InputImageType::SpacingType&
    spacing = reader->GetOutput()->GetSpacing();
  const InputImageType::PointType&
    origin  = reader->GetOutput()->GetOrigin();
  const InputImageType::DirectionType&
    direction  = reader->GetOutput()->GetDirection();
  InputImageType::SizeType size =
      reader->GetOutput()->GetLargestPossibleRegion().GetSize();
  filter->SetOutputOrigin( origin );
  filter->SetOutputSpacing( spacing );
  filter->SetOutputDirection( direction );
  filter->SetSize( size );
  // Software Guide : EndCodeSnippet


  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );


  TransformType::OutputVectorType translation1;

  const double imageCenterX = origin[0] + spacing[0] * size[0] / 2.0;
  const double imageCenterY = origin[1] + spacing[1] * size[1] / 2.0;

  translation1[0] =   -imageCenterX;
  translation1[1] =   -imageCenterY;

  transform->Translate( translation1 );


  std::cout << "imageCenterX = " << imageCenterX << std::endl;
  std::cout << "imageCenterY = " << imageCenterY << std::endl;


  const double degreesToRadians = std::atan(1.0) / 45.0;
  const double angle = angleInDegrees * degreesToRadians;
  transform->Rotate2D( -angle, false );


  TransformType::OutputVectorType translation2;
  translation2[0] =   imageCenterX;
  translation2[1] =   imageCenterY;
  transform->Translate( translation2, false );


  //  Software Guide : BeginLatex
  //
  //  The output of the resampling filter is connected to a writer and the
  //  execution of the pipeline is triggered by a writer update.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
