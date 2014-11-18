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
//  This example illustrates the use of the \doxygen{Similarity2DTransform}. A
//  similarity transform involves rotation, translation and scaling. Since the
//  parameterization of rotations is difficult to get in a generic $ND$ case, a
//  particular implementation is available for $2D$.
//
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"


//  Software Guide : BeginLatex
//
//  The header file of the transform is included below.
//
//  \index{itk::Similarity2DTransform!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSimilarity2DTransform.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  degrees  scale" << std::endl;
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
  const double scale          = atof( argv[4] );

  typedef itk::ResampleImageFilter<
                  InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();


  //  Software Guide : BeginLatex
  //
  //  The transform type is instantiated using the coordinate representation
  //  type as the single template parameter.
  //
  //  \index{itk::Similarity2DTransform!instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Similarity2DTransform< double >  TransformType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  A transform object is constructed by calling \code{New()} and passing
  //  the result to a \doxygen{SmartPointer}.
  //
  //  \index{itk::Similarity2DTransform!New()}
  //  \index{itk::Similarity2DTransform!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
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


  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );


  //  Software Guide : BeginLatex
  //
  //  The Similarity2DTransform allows the user to select the
  //  center of rotation.  This center is used for both rotation and scaling
  //  operations.
  //
  //  \index{itk::Similarity2DTransform!SetRotationCenter()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::InputPointType rotationCenter;
  rotationCenter[0] = origin[0] + spacing[0] * size[0] / 2.0;
  rotationCenter[1] = origin[1] + spacing[1] * size[1] / 2.0;
  transform->SetCenter( rotationCenter );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The rotation is specified with the method \code{SetAngle()}.
  //
  //  \index{itk::Similarity2DTransform!SetAngle()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const double degreesToRadians = std::atan(1.0) / 45.0;
  const double angle = angleInDegrees * degreesToRadians;
  transform->SetAngle( angle );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The scale change is defined using the method \code{SetScale()}.
  //  \index{itk::Similarity2DTransform!SetScale()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  transform->SetScale( scale );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  A translation to be applied after the rotation and scaling can be
  //  specified with the method \code{SetTranslation()}.
  //
  //  \index{itk::AffineTransform!Translate()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::OutputVectorType translation;

  translation[0] =   13.0;
  translation[1] =   17.0;

  transform->SetTranslation( translation );

  filter->SetTransform( transform );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Note that the order in which rotation, scaling and translation are
  //  defined is irrelevant in this transform. This is not the case in the
  //  Affine transform which is very generic and allows different combinations
  //  for initialization. In the Similarity2DTransform class the
  //  rotation and scaling will always be applied before the translation.
  //
  //  \index{itk::Similarity2DTransform!SetScale()}
  //
  //  Software Guide : EndLatex


  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }


  //  Software Guide : BeginLatex
  //
  //  \begin{figure}
  //  \center
  //  \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20}
  //  \includegraphics[width=0.44\textwidth]{ResampleImageFilterOutput11}
  //  \itkcaption[Effect of the Resample filter rotating and scaling an
  //  image]{Effect of the resample filter rotating and scaling an image.}
  //  \label{fig:ResampleImageFilterOutput11}
  //  \end{figure}
  //
  //  Figure \ref{fig:ResampleImageFilterOutput11} shows the effect of this
  //  rotation, translation and scaling on a slice of a brain MRI. The scale
  //  applied for producing this figure was $1.2$ and the rotation angle was
  //  $10^{\circ}$.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
