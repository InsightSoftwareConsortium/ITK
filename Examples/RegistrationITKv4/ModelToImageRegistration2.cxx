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

// Software Guide : BeginLatex
//
//  This example illustrates the use of the \doxygen{SpatialObject} as a
//  component of the registration framework in order to perform model based
//  registration. In this case, a SpatialObject is used for generating a
//  \doxygen{PointSet} whose points are located in a narrow band around the
//  edges of the SpatialObject. This PointSet is then used in order to perform
//  PointSet to Image registration.
//
// Software Guide : EndLatex


//  Software Guide : BeginLatex
//
//  In this example we use the \doxygen{BoxSpatialObject}, that is one of the
//  simplest SpatialObjects in ITK.
//
//  \index{itk::BoxSpatialObject!header}
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
#include "itkBoxSpatialObject.h"
//  Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  The generation of the PointSet is done in two stages. First the
//  SpatialObject is rasterized in order to generate an image containing a
//  binary mask that represents the inside and outside of the SpatialObject.
//  Second, this mask is used for computing a distance map, and the points
//  close to the boundary of the mask are taken as elements of the final
//  PointSet. The pixel values associated to the point in the PointSet are the
//  values of distance from each point to the binary mask.  The first stage is
//  performed by the \doxygen{SpatialObjectToImageFilter}, while the second
//  stage is performed witht eh \doxygen{BinaryMaskToNarrowBandPointSetFilter}
//
//  \index{itk::Spatial\-Object\-To\-Image\-Filter!header}
//  \index{itk::Binary\-Mask\-To\-Narrow\-Band\-Point\-Set\-Filter!header}
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
#include "itkSpatialObjectToImageFilter.h"
#include "itkBinaryMaskToNarrowBandPointSetFilter.h"
//  Software Guide : EndCodeSnippet

#include "itkBinaryMaskToNarrowBandPointSetFilter.h"

#include "itkPointSet.h"
#include "itkPointSetToImageRegistrationMethod.h"
#include "itkNormalizedCorrelationPointSetToImageMetric.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkRigid2DTransform.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkResampleImageFilter.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

//
// Observer to the optimizer
//
class CommandIterationUpdate : public itk::Command
{
public:
  typedef  CommandIterationUpdate   Self;
  typedef  itk::Command             Superclass;
  typedef  itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );

protected:
  CommandIterationUpdate() {};

public:
  typedef   itk::RegularStepGradientDescentOptimizer  OptimizerType;
  typedef   const OptimizerType *                     OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    OptimizerPointer optimizer = static_cast< OptimizerPointer >( object );
    if( typeid( event ) != typeid( itk::IterationEvent ) )
      {
      return;
      }

    OptimizerType::DerivativeType gradient = optimizer->GetGradient();
    OptimizerType::ScalesType     scales   = optimizer->GetScales();

    double magnitude2 = 0.0;

    for(unsigned int i=0; i<gradient.size(); i++)
      {
      const double fc = gradient[i] / scales[i];
      magnitude2 += fc * fc;
      }

    const double gradientMagnitude = std::sqrt( magnitude2 );

    std::cout << optimizer->GetCurrentIteration() << "   ";
    std::cout << optimizer->GetValue() << "   ";
    std::cout << gradientMagnitude << "   ";
    std::cout << optimizer->GetCurrentPosition() << std::endl;
    }
};

int main( int argc, char * argv [] )
{

  if( argc < 2 )
    {
    std::cerr << "Missing argument" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " movingImageFileName [initialX initialY] " << std::endl;
    std::cerr << "[rasterizedObjectFileName] [BoxSizeX BoxSizeY]" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned char MaskPixelType;

  typedef itk::Image< MaskPixelType, Dimension > MaskImageType;


  typedef itk::BoxSpatialObject< Dimension >    SpatialObjectType;

  typedef itk::SpatialObjectToImageFilter<
                              SpatialObjectType,
                              MaskImageType
                                >   SpatialObjectToImageFilterType;

  typedef itk::PointSet< float, Dimension >       FixedPointSetType;


  typedef itk::BinaryMaskToNarrowBandPointSetFilter<
                                    MaskImageType,
                                    FixedPointSetType
                                            >  NarrowBandFilterType;

  typedef  signed short   PixelType;

  typedef itk::Image< PixelType, Dimension >  ImageType;

  typedef  unsigned char       MaskPixelType;

  typedef itk::Image< MaskPixelType, Dimension >  MaskImageType;


  typedef itk::Rigid2DTransform< double  > TransformType;

  typedef TransformType::ParametersType          ParametersType;


  typedef itk::RegularStepGradientDescentOptimizer    OptimizerType;

  typedef itk::LinearInterpolateImageFunction<
                                    ImageType,
                                    double     > LinearInterpolatorType;


  typedef itk::NormalizedCorrelationPointSetToImageMetric<
                                    FixedPointSetType,
                                    ImageType  >   MetricType;


  typedef OptimizerType::ScalesType       OptimizerScalesType;


  typedef itk::PointSetToImageRegistrationMethod<
                                    FixedPointSetType,
                                    ImageType  >   RegistrationType;


  typedef CommandIterationUpdate    IterationObserverType;

  typedef itk::ImageFileReader< ImageType >      ImageReaderType;

  SpatialObjectType::Pointer            spatialObject;
  TransformType::Pointer                transform;
  OptimizerType::Pointer                optimizer;
  IterationObserverType::Pointer        iterationObserver;
  LinearInterpolatorType::Pointer       linearInterpolator;
  MetricType::Pointer                   metric;
  RegistrationType::Pointer             registrationMethod;
  ImageReaderType::Pointer              movingImageReader;
  FixedPointSetType::Pointer            fixedPointSet;
  ImageType::ConstPointer               movingImage;

  SpatialObjectToImageFilterType::Pointer rasterizationFilter;
  NarrowBandFilterType::Pointer           narrowBandPointSetFilter;


  metric              = MetricType::New();
  transform           = TransformType::New();
  optimizer           = OptimizerType::New();
  linearInterpolator  = LinearInterpolatorType::New();
  registrationMethod  = RegistrationType::New();
  iterationObserver   = IterationObserverType::New();

  spatialObject            = SpatialObjectType::New();
  rasterizationFilter      = SpatialObjectToImageFilterType::New();
  narrowBandPointSetFilter = NarrowBandFilterType::New();

  movingImageReader        = ImageReaderType::New();

  movingImageReader->SetFileName( argv[1] );

  try
    {
    movingImageReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem reading Moving image from = " << std::endl;
    std::cerr << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  movingImage = movingImageReader->GetOutput();

  SpatialObjectType::SizeType boxSize;
  boxSize[0] = 60.0;  // mm
  boxSize[1] = 60.0;  // mm

  if( argc > 6 )
    {
    boxSize[0] = atof( argv[5] );
    boxSize[1] = atof( argv[6] );
    }

  //
  // The geometry of the BoxSpatialObject is such that one of
  // its corners is located at the origin of coordinates.
  //
  spatialObject->SetSize( boxSize );

  ImageType::RegionType region =
                movingImage->GetLargestPossibleRegion();

  ImageType::SizeType   imageSize = region.GetSize();

  ImageType::SpacingType spacing = movingImage->GetSpacing();
  ImageType::PointType  origin;
  origin[0] = ( boxSize[0] - imageSize[0] * spacing[0] ) / 2.0;
  origin[1] = ( boxSize[1] - imageSize[1] * spacing[1] ) / 2.0;

  rasterizationFilter->SetInput( spatialObject );
  rasterizationFilter->SetSize( imageSize );
  rasterizationFilter->SetSpacing( spacing );
  rasterizationFilter->SetOrigin( origin );


  narrowBandPointSetFilter->SetBandWidth( 5.0 );

  narrowBandPointSetFilter->SetInput(
                    rasterizationFilter->GetOutput() );

  narrowBandPointSetFilter->Update();

  if( argc > 4 )
    {
    typedef itk::ImageFileWriter< MaskImageType > MaskWriterType;
    MaskWriterType::Pointer maskWriter = MaskWriterType::New();
    maskWriter->SetInput( rasterizationFilter->GetOutput() );
    maskWriter->SetFileName( argv[4] );
    maskWriter->Update();
    }

  fixedPointSet = narrowBandPointSetFilter->GetOutput();

  fixedPointSet->Print( std::cout );

  registrationMethod->SetOptimizer(     optimizer     );
  registrationMethod->SetInterpolator(  linearInterpolator  );
  registrationMethod->SetMetric(        metric        );
  registrationMethod->SetTransform(     transform     );

  registrationMethod->SetMovingImage(   movingImage  );
  registrationMethod->SetFixedPointSet( fixedPointSet );


  optimizer->SetMaximumStepLength( 1.00 );
  optimizer->SetMinimumStepLength( 0.001 );
  optimizer->SetNumberOfIterations( 300 );
  optimizer->SetRelaxationFactor( 0.90 );
  optimizer->SetGradientMagnitudeTolerance( 0.05 );
  optimizer->MinimizeOn();
  optimizer->AddObserver( itk::IterationEvent(), iterationObserver );

  TransformType::TranslationType  initialTranslation;
  initialTranslation[0] = 0.0;
  initialTranslation[1] = 0.0;

  if( argc >= 4 )
    {
    initialTranslation[0] = atof( argv[2] );
    initialTranslation[1] = atof( argv[3] );
    }


  TransformType::OutputPointType rotationCenter;
  rotationCenter[0] = boxSize[0] / 2.0;
  rotationCenter[1] = boxSize[1] / 2.0;

  transform->SetIdentity();
  transform->SetCenter( rotationCenter );
  transform->SetTranslation( initialTranslation );

  registrationMethod->SetInitialTransformParameters(
                                  transform->GetParameters() );

  OptimizerScalesType optimizerScales( transform->GetNumberOfParameters() );

  const double translationScale = 1.0 / 1000.0;

  optimizerScales[0] = 1.0;
  optimizerScales[1] = translationScale;
  optimizerScales[2] = translationScale;

  optimizer->SetScales( optimizerScales );

  try
    {
    registrationMethod->Update();
    std::cout << "Optimizer stop condition: "
              << registrationMethod->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem found during the registration" << std::endl;
    std::cerr << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  ParametersType transformParameters =
         registrationMethod->GetLastTransformParameters();


  TransformType::OutputPointType center = transform->GetCenter();

  std::cout << "Registration parameter = " << std::endl;
  std::cout << "Rotation center = " << center << std::endl;
  std::cout << "Parameters = " << transformParameters << std::endl;


  return EXIT_SUCCESS;
}
