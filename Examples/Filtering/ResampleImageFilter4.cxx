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

//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {ResampleImageFilterOutput10.png}
//    ARGUMENTS:    -15
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginLatex
//
//  The following example illustrates how to rotate an image around its
//  center.  In this particular case an \doxygen{AffineTransform} is used to
//  map the input space into the output space.
//
//  \index{itk::AffineTransform!resampling}
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"


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

  typedef itk::ResampleImageFilter<
                  InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();


  //  Software Guide : BeginLatex
  //
  //  The transform type is instantiated using the coordinate representation
  //  type and the space dimension. Then a transform object is constructed
  //  with the \code{New()} method and passed to a \doxygen{SmartPointer}.
  //
  //  \index{itk::AffineTransform!instantiation}
  //  \index{itk::AffineTransform!New()}
  //  \index{itk::AffineTransform!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::AffineTransform< double, Dimension >  TransformType;
  TransformType::Pointer transform = TransformType::New();
  // Software Guide : EndCodeSnippet


  typedef itk::LinearInterpolateImageFunction<
                       InputImageType, double >  InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  filter->SetInterpolator( interpolator );

  filter->SetDefaultPixelValue( 100 );


  //  Software Guide : BeginLatex
  //
  //  The parameters of the output image are taken from the input image.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  reader->Update();

  const InputImageType * inputImage = reader->GetOutput();

  const InputImageType::SpacingType & spacing = inputImage->GetSpacing();
  const InputImageType::PointType & origin  = inputImage->GetOrigin();
  InputImageType::SizeType size =
      inputImage->GetLargestPossibleRegion().GetSize();

  filter->SetOutputOrigin( origin );
  filter->SetOutputSpacing( spacing );
  filter->SetOutputDirection( inputImage->GetDirection() );
  filter->SetSize( size );
  // Software Guide : EndCodeSnippet


  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );


  //  Software Guide : BeginLatex
  //
  //  Rotations are performed around the origin of physical coordinates---not
  //  the image origin nor the image center. Hence, the process of
  //  positioning the output image frame as it is shown in Figure
  //  \ref{fig:ResampleImageFilterOutput10} requires three steps.  First, the
  //  image origin must be moved to the origin of the coordinate system. This
  //  is done by applying a translation equal to the negative values of the
  //  image origin.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20}
  // \includegraphics[width=0.44\textwidth]{ResampleImageFilterOutput10}
  // \itkcaption[Effect of the Resample filter rotating an image]{Effect of the
  // resample filter rotating an image.}
  // \label{fig:ResampleImageFilterOutput10}
  // \end{figure}
  //
  //
  //  \index{itk::AffineTransform!Translate()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::OutputVectorType translation1;

  const double imageCenterX = origin[0] + spacing[0] * size[0] / 2.0;
  const double imageCenterY = origin[1] + spacing[1] * size[1] / 2.0;

  translation1[0] =   -imageCenterX;
  translation1[1] =   -imageCenterY;

  transform->Translate( translation1 );
  // Software Guide : EndCodeSnippet

  std::cout << "imageCenterX = " << imageCenterX << std::endl;
  std::cout << "imageCenterY = " << imageCenterY << std::endl;


  //  Software Guide : BeginLatex
  //
  //  In a second step, the rotation is specified using the method
  //  \code{Rotate2D()}.
  //
  //  \index{itk::AffineTransform!Rotate2D()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  const double degreesToRadians = std::atan(1.0) / 45.0;
  const double angle = angleInDegrees * degreesToRadians;
  transform->Rotate2D( -angle, false );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The third and final step requires translating the image origin back to
  //  its previous location. This is be done by applying a translation equal
  //  to the origin values.
  //
  //  \index{itk::AffineTransform!Translate()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::OutputVectorType translation2;
  translation2[0] =   imageCenterX;
  translation2[1] =   imageCenterY;
  transform->Translate( translation2, false );
  filter->SetTransform( transform );
  // Software Guide : EndCodeSnippet


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
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
