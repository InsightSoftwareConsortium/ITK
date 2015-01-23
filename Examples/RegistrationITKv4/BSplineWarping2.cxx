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
//  This example illustrates how to deform a 3D image using a BSplineTransform.
//
//  \index{BSplineTransform}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"

#include "itkBSplineTransform.h"
#include "itkTransformFileWriter.h"
//  Software Guide : EndCodeSnippet

#include <fstream>

//  The following section of code implements a Command observer
//  used to monitor the evolution of the registration process.
//
#include "itkCommand.h"
class CommandProgressUpdate : public itk::Command
{
public:
  typedef  CommandProgressUpdate   Self;
  typedef  itk::Command            Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );

protected:
  CommandProgressUpdate() {};

public:
  void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
      Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
      const itk::ProcessObject * filter = static_cast< const itk::ProcessObject * >( object );
      if( ! itk::ProgressEvent().CheckEvent( &event ) )
        {
        return;
        }
      std::cout << filter->GetProgress() << std::endl;
    }
};


int main( int argc, char * argv[] )
{

  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " coefficientsFile fixedImage ";
    std::cerr << "movingImage deformedMovingImage" << std::endl;
    std::cerr << "[deformationField]" << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  Begin by creating the relevant types.
  //
  //  Software Guide: EndLatex

  // Software Guide : BeginCodeSnippet
  const     unsigned int   ImageDimension = 3;

  typedef   unsigned char                            PixelType;
  typedef   itk::Image< PixelType, ImageDimension >  FixedImageType;
  typedef   itk::Image< PixelType, ImageDimension >  MovingImageType;

  typedef   itk::ImageFileReader< FixedImageType  >  FixedReaderType;
  typedef   itk::ImageFileReader< MovingImageType >  MovingReaderType;

  typedef   itk::ImageFileWriter< MovingImageType >  MovingWriterType;
  // Software Guide : EndCodeSnippet


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

  //  Software Guide : BeginLatex
  //
  //  Setup the moving reader and writer, and get the filenames from the
  //  command line arguments.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  MovingReaderType::Pointer movingReader = MovingReaderType::New();
  MovingWriterType::Pointer movingWriter = MovingWriterType::New();

  movingReader->SetFileName( argv[3] );
  movingWriter->SetFileName( argv[4] );
  // Software Guide : EndCodeSnippet

  FixedImageType::ConstPointer fixedImage = fixedReader->GetOutput();


  typedef itk::ResampleImageFilter< MovingImageType,
                                    FixedImageType  >  FilterType;

  FilterType::Pointer resampler = FilterType::New();

  typedef itk::LinearInterpolateImageFunction<
                       MovingImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  resampler->SetInterpolator( interpolator );

  FixedImageType::SpacingType   fixedSpacing    = fixedImage->GetSpacing();
  FixedImageType::PointType     fixedOrigin     = fixedImage->GetOrigin();
  FixedImageType::DirectionType fixedDirection  = fixedImage->GetDirection();

  //  Software Guide : BeginLatex
  //
  //  Set the resampler spacing, origin, and direction to that of the fixed
  //  input image.  Do the same with the size and output start index.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  resampler->SetOutputSpacing( fixedSpacing );
  resampler->SetOutputOrigin(  fixedOrigin  );
  resampler->SetOutputDirection(  fixedDirection  );

  FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();
  FixedImageType::SizeType   fixedSize =  fixedRegion.GetSize();
  resampler->SetSize( fixedSize );
  resampler->SetOutputStartIndex(  fixedRegion.GetIndex() );
  // Software Guide : EndCodeSnippet


  resampler->SetInput( movingReader->GetOutput() );

  movingWriter->SetInput( resampler->GetOutput() );


  //  Software Guide : BeginLatex
  //
  //  We instantiate now the type of the \code{BSplineTransform} using
  //  as template parameters the type for coordinates representation, the
  //  dimension of the space, and the order of the B-spline.
  //
  //  \index{BSplineTransform!New}
  //  \index{BSplineTransform!Instantiation}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  const unsigned int SpaceDimension = ImageDimension;
  const unsigned int SplineOrder = 3;
  typedef double CoordinateRepType;

  typedef itk::BSplineTransform<
                            CoordinateRepType,
                            SpaceDimension,
                            SplineOrder >     TransformType;

  TransformType::Pointer bsplineTransform = TransformType::New();
  //  Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  const unsigned int numberOfGridNodes = 8;

  TransformType::PhysicalDimensionsType   fixedPhysicalDimensions;
  TransformType::MeshSizeType             meshSize;

  for( unsigned int i=0; i< SpaceDimension; i++ )
    {
    fixedPhysicalDimensions[i] = fixedSpacing[i] * static_cast<double>(
      fixedSize[i] - 1 );
    }
  meshSize.Fill( numberOfGridNodes - SplineOrder );

  bsplineTransform->SetTransformDomainOrigin( fixedOrigin );
  bsplineTransform->SetTransformDomainPhysicalDimensions(
    fixedPhysicalDimensions );
  bsplineTransform->SetTransformDomainMeshSize( meshSize );
  bsplineTransform->SetTransformDomainDirection( fixedDirection );


  typedef TransformType::ParametersType     ParametersType;
  const unsigned int numberOfParameters =
               bsplineTransform->GetNumberOfParameters();

  const unsigned int numberOfNodes = numberOfParameters / SpaceDimension;

  ParametersType parameters( numberOfParameters );
  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The B-spline grid should now be fed with coefficients at each node. Since
  //  this is a two-dimensional grid, each node should receive two coefficients.
  //  Each coefficient pair is representing a displacement vector at this node.
  //  The coefficients can be passed to the B-spline in the form of an array where
  //  the first set of elements are the first component of the displacements for
  //  all the nodes, and the second set of elemets is formed by the second
  //  component of the displacements for all the nodes.
  //
  //  In this example we read such displacements from a file, but for convenience
  //  we have written this file using the pairs of $(x,y)$ displacement for every
  //  node. The elements read from the file should therefore be reorganized when
  //  assigned to the elements of the array. We do this by storing all the odd
  //  elements from the file in the first block of the array, and all the even
  //  elements from the file in the second block of the array. Finally the array
  //  is passed to the B-spline transform using the \code{SetParameters()}.
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  std::ifstream infile;

  infile.open( argv[1] );

  for( unsigned int n=0; n < numberOfNodes; n++ )
    {
    infile >>  parameters[n];                  // X coordinate
    infile >>  parameters[n+numberOfNodes];    // Y coordinate
    infile >>  parameters[n+numberOfNodes*2];  // Z coordinate
    }

  infile.close();
  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //   Finally the array is passed to the B-spline transform using the
  //   \code{SetParameters()}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  bsplineTransform->SetParameters( parameters );
  //  Software Guide : EndCodeSnippet

   CommandProgressUpdate::Pointer observer = CommandProgressUpdate::New();

   resampler->AddObserver( itk::ProgressEvent(), observer );


  //  Software Guide : BeginLatex
  //
  //  At this point we are ready to use the transform as part of the resample
  //  filter. We trigger the execution of the pipeline by invoking
  //  \code{Update()} on the last filter of the pipeline, in this case the
  //  writer.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  resampler->SetTransform( bsplineTransform );

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
  //  Software Guide : EndCodeSnippet

  typedef itk::Vector< float, ImageDimension >      VectorType;
  typedef itk::Image< VectorType, ImageDimension >  DisplacementFieldType;

  DisplacementFieldType::Pointer field = DisplacementFieldType::New();
  field->SetRegions( fixedRegion );
  field->SetOrigin( fixedOrigin );
  field->SetSpacing( fixedSpacing );
  field->SetDirection( fixedDirection );
  field->Allocate();

  typedef itk::ImageRegionIterator< DisplacementFieldType > FieldIterator;
  FieldIterator fi( field, fixedRegion );

  fi.GoToBegin();

  TransformType::InputPointType  fixedPoint;
  TransformType::OutputPointType movingPoint;
  DisplacementFieldType::IndexType index;

  VectorType displacement;

  while( ! fi.IsAtEnd() )
    {
    index = fi.GetIndex();
    field->TransformIndexToPhysicalPoint( index, fixedPoint );
    movingPoint = bsplineTransform->TransformPoint( fixedPoint );
    displacement = movingPoint - fixedPoint;
    fi.Set( displacement );
    ++fi;
    }

  typedef itk::ImageFileWriter< DisplacementFieldType >  FieldWriterType;
  FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

  fieldWriter->SetInput( field );

  if( argc >= 6 )
    {
    fieldWriter->SetFileName( argv[5] );
    try
      {
      fieldWriter->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception thrown " << std::endl;
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
      }
    }

  if( argc >= 7 )
    {
    fieldWriter->SetFileName( argv[6] );
    try
      {
      typedef itk::TransformFileWriter    TransformWriterType;
      TransformWriterType::Pointer transformWriter = TransformWriterType::New();
      transformWriter->AddTransform( bsplineTransform );
      transformWriter->SetFileName( argv[6] );
      transformWriter->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception thrown " << std::endl;
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}
