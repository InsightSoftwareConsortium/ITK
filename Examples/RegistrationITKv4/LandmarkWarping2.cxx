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
//  This example illustrates how to deform an image using a KernelBase spline
//  and two sets of landmarks.
//
//  \index{WarpImageFilter}
//  \index{LandmarkDisplacementFieldSource}
//
//  In addition to standard headers included in previous examples, this example
//  requires the following includes:
//
//  Software Guide : EndLatex

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkWarpImageFilter.h"
// Software Guide : BeginCodeSnippet
#include "itkVector.h"
#include "itkLandmarkDisplacementFieldSource.h"
#include <fstream>
// Software Guide : EndCodeSnippet

int main( int argc, char * argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " landmarksFile fixedImage ";
    std::cerr << "movingImage deformedMovingImage" << std::endl;
    return EXIT_FAILURE;
    }

  const     unsigned int   Dimension = 2;
  typedef   float          VectorComponentType;

  typedef   itk::Vector< VectorComponentType, Dimension >    VectorType;

  typedef   itk::Image< VectorType, Dimension >   DisplacementFieldType;

  typedef   unsigned char                            PixelType;
  typedef   itk::Image< PixelType, Dimension >       FixedImageType;
  typedef   itk::Image< PixelType, Dimension >       MovingImageType;

  typedef   itk::ImageFileReader< FixedImageType  >  FixedReaderType;
  typedef   itk::ImageFileReader< MovingImageType >  MovingReaderType;

  typedef   itk::ImageFileWriter< MovingImageType >  MovingWriterType;


  FixedReaderType::Pointer fixedReader = FixedReaderType::New();
  fixedReader->SetFileName( argv[2] );

  try
    {
    fixedReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  MovingReaderType::Pointer movingReader = MovingReaderType::New();
  MovingWriterType::Pointer movingWriter = MovingWriterType::New();

  movingReader->SetFileName( argv[3] );
  movingWriter->SetFileName( argv[4] );


  FixedImageType::ConstPointer fixedImage = fixedReader->GetOutput();

//  Software Guide : BeginLatex
//
//  After reading in the fixed and moving images, the \code{deformer} object is
//  instantiated from the \code{itk::LandmarkDisplacementFieldSource} class, and
//  parameters of the image space and orientation are set.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet

  typedef itk::LandmarkDisplacementFieldSource<
                                DisplacementFieldType
                                             >  DisplacementSourceType;

  DisplacementSourceType::Pointer deformer = DisplacementSourceType::New();

  deformer->SetOutputSpacing( fixedImage->GetSpacing() );
  deformer->SetOutputOrigin(  fixedImage->GetOrigin() );
  deformer->SetOutputRegion(  fixedImage->GetLargestPossibleRegion() );
  deformer->SetOutputDirection( fixedImage->GetDirection() );

// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  Source and target landmarks are then created, and the points themselves are
//  read in from a file stream.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet

  typedef DisplacementSourceType::LandmarkContainer LandmarkContainerType;
  typedef DisplacementSourceType::LandmarkPointType LandmarkPointType;

  LandmarkContainerType::Pointer sourceLandmarks =
    LandmarkContainerType::New();
  LandmarkContainerType::Pointer targetLandmarks =
    LandmarkContainerType::New();

  LandmarkPointType sourcePoint;
  LandmarkPointType targetPoint;

  std::ifstream pointsFile;
  pointsFile.open( argv[1] );

  unsigned int pointId = 0;

  pointsFile >> sourcePoint;
  pointsFile >> targetPoint;

  while( !pointsFile.fail() )
    {
    sourceLandmarks->InsertElement( pointId, sourcePoint );
    targetLandmarks->InsertElement( pointId, targetPoint );
    ++pointId;

    pointsFile >> sourcePoint;
    pointsFile >> targetPoint;

    }

  pointsFile.close();

//  Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  The source and target landmark objects are then assigned to \code{deformer}.
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet

  deformer->SetSourceLandmarks( sourceLandmarks.GetPointer() );
  deformer->SetTargetLandmarks( targetLandmarks.GetPointer() );

//  Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  After calling \code{UpdateLargestPossibleRegion()} on the \code{deformer},
//  the displacement field may be obtained via the \code{GetOutput()} method.
//
//  Software Guide : EndLatex

  try
    {
    deformer->UpdateLargestPossibleRegion();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  DisplacementFieldType::ConstPointer displacementField = deformer->GetOutput();

  typedef itk::WarpImageFilter< MovingImageType,
                                MovingImageType,
                                DisplacementFieldType  >  FilterType;

  FilterType::Pointer warper = FilterType::New();

  typedef itk::LinearInterpolateImageFunction<
                       MovingImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  warper->SetInterpolator( interpolator );


  warper->SetOutputSpacing( displacementField->GetSpacing() );
  warper->SetOutputOrigin(  displacementField->GetOrigin() );

  warper->SetDisplacementField( displacementField );

  warper->SetInput( movingReader->GetOutput() );

  movingWriter->SetInput( warper->GetOutput() );

  try
    {
    movingWriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
