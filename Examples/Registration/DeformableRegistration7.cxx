/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DeformableRegistration7.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif




// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{BSplineDeformableTransform}
// class for performing registration of two $3D$ images. The example code is
// for the most part identical to the code presented in
// Section~\ref{sec:DeformableRegistration}.  The major difference is that this
// example we set the image dimension to 3 and replace the
// \doxygen{LBFGSOptimizer} optimizer with the \doxygen{LBFGSBOptimizer}.  This
// optimizer is more appropriate for performing optimization in a parametric
// spaces of higher dimensions.
// 
//
// \index{itk::BSplineDeformableTransform}
// \index{itk::BSplineDeformableTransform!DeformableRegistration}
// \index{itk::LBFGSOptimizer}
//
//
// Software Guide : EndLatex 

#include "itkImageRegistrationMethod.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkImage.h"

#include "itkTimeProbesCollectorBase.h"

//  Software Guide : BeginLatex
//  
//  The following are the most relevant headers to this example.
//
//  \index{itk::BSplineDeformableTransform!header}
//  \index{itk::LBFGSOptimizer!header}
// 
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkBSplineDeformableTransform.h"
#include "itkLBFGSBOptimizer.h"
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//  
//  The parameter space of the \code{BSplineDeformableTransform} is composed by
//  the set of all the deformations associated with the nodes of the BSpline
//  grid.  This large number of parameters makes possible to represent a wide
//  variety of deformations, but it also has the price of requiring a
//  significant amount of computation time.
//
//  \index{itk::BSplineDeformableTransform!header}
// 
//  Software Guide : EndLatex 

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"


//  The following section of code implements a Command observer
//  used to monitor the evolution of the registration process.
//
#include "itkCommand.h"
class CommandIterationUpdate : public itk::Command 
{
public:
  typedef  CommandIterationUpdate   Self;
  typedef  itk::Command             Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );
protected:
  CommandIterationUpdate() {};
public:
  typedef itk::LBFGSBOptimizer     OptimizerType;
  typedef   const OptimizerType   *    OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event)
    {
      Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event)
    {
      OptimizerPointer optimizer = 
        dynamic_cast< OptimizerPointer >( object );
      if( !(itk::IterationEvent().CheckEvent( &event )) )
        {
        return;
        }
      std::cout << optimizer->GetCurrentIteration() << "   ";
      std::cout << optimizer->GetValue() << "   ";
      std::cout << optimizer->GetInfinityNormOfProjectedGradient() << std::endl;
    }
};


int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile outputImagefile  ";
    std::cerr << " [differenceOutputfile] [differenceBeforeRegistration] ";
    std::cerr << " [deformationField] ";
    return 1;
    }
  
  const    unsigned int    ImageDimension = 3;
  typedef  float           PixelType;

  typedef itk::Image< PixelType, ImageDimension >  FixedImageType;
  typedef itk::Image< PixelType, ImageDimension >  MovingImageType;


  //  Software Guide : BeginLatex
  //
  //  We instantiate now the type of the \code{BSplineDeformableTransform} using
  //  as template parameters the type for coordinates representation, the
  //  dimension of the space, and the order of the BSpline. 
  // 
  //  \index{BSplineDeformableTransform!New}
  //  \index{BSplineDeformableTransform!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  const unsigned int SpaceDimension = ImageDimension;
  const unsigned int SplineOrder = 3;
  typedef double CoordinateRepType;

  typedef itk::BSplineDeformableTransform<
                            CoordinateRepType,
                            SpaceDimension,
                            SplineOrder >     TransformType;
  // Software Guide : EndCodeSnippet


  typedef itk::LBFGSBOptimizer       OptimizerType;


  typedef itk::MeanSquaresImageToImageMetric< 
                                    FixedImageType, 
                                    MovingImageType >    MetricType;

  typedef itk:: LinearInterpolateImageFunction< 
                                    MovingImageType,
                                    double          >    InterpolatorType;

  typedef itk::ImageRegistrationMethod< 
                                    FixedImageType, 
                                    MovingImageType >    RegistrationType;

  MetricType::Pointer         metric        = MetricType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  

  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetInterpolator(  interpolator  );


  //  Software Guide : BeginLatex
  //
  //  The transform object is constructed below and passed to the registration
  //  method.
  //  \index{itk::RegistrationMethod!SetTransform()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer  transform = TransformType::New();
  registration->SetTransform( transform );
  // Software Guide : EndCodeSnippet

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  FixedImageType::ConstPointer fixedImage = fixedImageReader->GetOutput();

  registration->SetFixedImage(  fixedImage   );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );

  fixedImageReader->Update();

  FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();
  
 registration->SetFixedImageRegion( fixedRegion );

  //  Software Guide : BeginLatex
  //
  //  Here we define the parameters of the BSplineDeformableTransform grid.  We
  //  arbitrarily decide to use a grid with $5 \times 5$ nodes within the image. 
  //  The reader should note that the BSpline computation requires a
  //  finite support region ( 1 grid node at the lower borders and 2
  //  grid nodes at upper borders). Therefore in this example, we set
  //  the grid size to be $8 \times 8$ and place the grid origin such that
  //  grid node (1,1) coincides with the first pixel in the fixed image.
  // 
  //  \index{BSplineDeformableTransform}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  typedef TransformType::RegionType RegionType;
  RegionType bsplineRegion;
  RegionType::SizeType   gridSizeOnImage;
  RegionType::SizeType   gridBorderSize;
  RegionType::SizeType   totalGridSize;

  gridSizeOnImage.Fill( 5 );
  gridBorderSize.Fill( 3 );    // Border for spline order = 3 ( 1 lower, 2 upper )
  totalGridSize = gridSizeOnImage + gridBorderSize;

  bsplineRegion.SetSize( totalGridSize );

  typedef TransformType::SpacingType SpacingType;
  SpacingType spacing = fixedImage->GetSpacing();

  typedef TransformType::OriginType OriginType;
  OriginType origin = fixedImage->GetOrigin();;

  FixedImageType::SizeType fixedImageSize = fixedRegion.GetSize();

  for(unsigned int r=0; r<ImageDimension; r++)
    {
    spacing[r] *= floor( static_cast<double>(fixedImageSize[r] - 1)  / 
                  static_cast<double>(gridSizeOnImage[r] - 1) );
    origin[r]  -=  spacing[r]; 
    }

  transform->SetGridSpacing( spacing );
  transform->SetGridOrigin( origin );
  transform->SetGridRegion( bsplineRegion );
  

  typedef TransformType::ParametersType     ParametersType;

  const unsigned int numberOfParameters =
               transform->GetNumberOfParameters();
  
  ParametersType parameters( numberOfParameters );

  parameters.Fill( 0.0 );

  transform->SetParameters( parameters );
  //  Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  We now pass the parameters of the current transform as the initial
  //  parameters to be used when the registration process starts. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  registration->SetInitialTransformParameters( transform->GetParameters() );
  // Software Guide : EndCodeSnippet

  std::cout << "Intial Parameters = " << std::endl;
  std::cout << transform->GetParameters() << std::endl;

  //  Software Guide : BeginLatex
  //  
  //  Next we set the parameters of the LBFGSB Optimizer. 
  //
  //  Software Guide : EndLatex 
  OptimizerType::BoundSelectionType boundSelect( transform->GetNumberOfParameters() );
  OptimizerType::BoundValueType upperBound( transform->GetNumberOfParameters() );
  OptimizerType::BoundValueType lowerBound( transform->GetNumberOfParameters() );

  boundSelect.Fill( 0 );
  upperBound.Fill( 0.0 );
  lowerBound.Fill( 0.0 );

  optimizer->SetBoundSelection( boundSelect );
  optimizer->SetUpperBound( upperBound );
  optimizer->SetLowerBound( lowerBound );

  optimizer->SetCostFunctionConvergenceFactor( 1e+12 );
  optimizer->SetProjectedGradientTolerance( 1.0 );
  optimizer->SetMaximumNumberOfIterations( 500 );
  optimizer->SetMaximumNumberOfEvaluations( 500 );
  optimizer->SetMaximumNumberOfCorrections( 5 );

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );


  // Add a time probe
  itk::TimeProbesCollectorBase collector;

  std::cout << std::endl << "Starting Registration" << std::endl;

  try 
    { 
    collector.Start( "Registration" );
    registration->StartRegistration(); 
    collector.Stop( "Registration" );
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return -1;
    } 
  
  OptimizerType::ParametersType finalParameters = 
                    registration->GetLastTransformParameters();

  std::cout << "Last Transform Parameters" << std::endl;
  std::cout << finalParameters << std::endl;


  // Report the time taken by the registration
  collector.Report();

  // Software Guide : BeginCodeSnippet
  transform->SetParameters( finalParameters );
  // Software Guide : EndCodeSnippet


  typedef itk::ResampleImageFilter< 
                            MovingImageType, 
                            FixedImageType >    ResampleFilterType;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( transform );
  resample->SetInput( movingImageReader->GetOutput() );

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetDefaultPixelValue( 100 );
  
  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< OutputPixelType, ImageDimension > OutputImageType;
  
  typedef itk::CastImageFilter< 
                        FixedImageType,
                        OutputImageType > CastFilterType;
                    
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;


  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();


  writer->SetFileName( argv[3] );


  caster->SetInput( resample->GetOutput() );
  writer->SetInput( caster->GetOutput()   );


  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err ) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return -1;
    } 
 


  typedef itk::SquaredDifferenceImageFilter< 
                                  FixedImageType, 
                                  FixedImageType, 
                                  OutputImageType > DifferenceFilterType;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( difference->GetOutput() );  
  

  // Compute the difference image between the 
  // fixed and resampled moving image.
  if( argc >= 5 )
    {
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( resample->GetOutput() );
    writer2->SetFileName( argv[4] );
    try
      {
      writer2->Update();
      }
    catch( itk::ExceptionObject & err ) 
      { 
      std::cerr << "ExceptionObject caught !" << std::endl; 
      std::cerr << err << std::endl; 
      return -1;
      } 
    }


  // Compute the difference image between the 
  // fixed and moving image before registration.
  if( argc >= 6 )
    {
    writer2->SetFileName( argv[5] );
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( movingImageReader->GetOutput() );
    try
      {
      writer2->Update();
      }
    catch( itk::ExceptionObject & err ) 
      { 
      std::cerr << "ExceptionObject caught !" << std::endl; 
      std::cerr << err << std::endl; 
      return -1;
      } 
    }



  // Generate the explicit deformation field resulting from 
  // the registration.
  if( argc >= 7 )
    {

    typedef itk::Vector< float, ImageDimension >  VectorType;
    typedef itk::Image< VectorType, ImageDimension >  DeformationFieldType;

    DeformationFieldType::Pointer field = DeformationFieldType::New();
    field->SetRegions( fixedRegion );
    field->SetOrigin( fixedImage->GetOrigin() );
    field->SetSpacing( fixedImage->GetSpacing() );
    field->Allocate();

    typedef itk::ImageRegionIterator< DeformationFieldType > FieldIterator;
    FieldIterator fi( field, fixedRegion );

    fi.GoToBegin();

    TransformType::InputPointType  fixedPoint;
    TransformType::OutputPointType movingPoint;
    DeformationFieldType::IndexType index;

    VectorType displacement;

    while( ! fi.IsAtEnd() )
      {
      index = fi.GetIndex();
      field->TransformIndexToPhysicalPoint( index, fixedPoint );
      movingPoint = transform->TransformPoint( fixedPoint );
      displacement[0] = movingPoint[0] - fixedPoint[0];
      displacement[1] = movingPoint[1] - fixedPoint[1];
      fi.Set( displacement );
      ++fi;
      }



    typedef itk::ImageFileWriter< DeformationFieldType >  FieldWriterType;
    FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

    fieldWriter->SetInput( field );

    fieldWriter->SetFileName( argv[6] );
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

  return 0;
}

