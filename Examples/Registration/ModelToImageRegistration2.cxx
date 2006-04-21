/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ModelToImageRegistration2.cxx
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

#include "itkImage.h"
#include "itkPointSet.h"
#include "itkPointSetToImageRegistrationMethod.h"
#include "itkMeanSquaresPointSetToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkRigid2DTransform.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkResampleImageFilter.h"

#include "itkImageFileReader.h"



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
  typedef   const OptimizerType   *           OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event)
    {
      Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event)
    {
      OptimizerPointer optimizer = 
        dynamic_cast< OptimizerPointer >( object );
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

      const double gradientMagnitude = sqrt( magnitude2 );

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
    std::cerr << argv[0] << " movingImageFileName " << std::endl;
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


  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;

  typedef itk::LinearInterpolateImageFunction< 
                                    ImageType,
                                    double     > LinearInterpolatorType;


  typedef itk::MeanSquaresPointSetToImageMetric< 
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

  SpatialObjectToImageFilterType::Pointer    rasterizationFilter;

  NarrowBandFilterType::Pointer      narrowBandPointSetFilter;

  
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
  boxSize[0] = 60;
  boxSize[1] = 60;
  
  spatialObject->SetSize( boxSize );

  rasterizationFilter->SetInput( spatialObject );
      
  narrowBandPointSetFilter->SetInput( 
                    rasterizationFilter->GetOutput() ); 

  narrowBandPointSetFilter->Update();

  fixedPointSet = narrowBandPointSetFilter->GetOutput();
  
  registrationMethod->SetOptimizer(     optimizer     );
  registrationMethod->SetInterpolator(  linearInterpolator  );
  registrationMethod->SetMetric(        metric        );
  registrationMethod->SetTransform(     transform     );

  registrationMethod->SetMovingImage(   movingImage  );
  registrationMethod->SetFixedPointSet( fixedPointSet );


  optimizer->SetMaximumStepLength( 2.00 );
  optimizer->SetMinimumStepLength( 0.001 );
  optimizer->SetNumberOfIterations( 300 );
  optimizer->SetRelaxationFactor( 0.90 );
  optimizer->SetGradientMagnitudeTolerance( 0.05 );





  optimizer->AddObserver( itk::IterationEvent(), iterationObserver );


  TransformType::TranslationType  initialTranslation;
  initialTranslation[0] = 50.0;
  initialTranslation[1] = 50.0;

  transform->SetIdentity();
  transform->SetTranslation( initialTranslation );

  registrationMethod->SetInitialTransformParameters( 
                                  transform->GetParameters() ); 

  registrationMethod->StartRegistration(); 

  ParametersType transformParameters = 
         registrationMethod->GetLastTransformParameters();


  TransformType::OutputPointType center = transform->GetCenter();

  std::cout << "Registration parameter = " << std::endl;
  std::cout << "Rotation center = " << center << std::endl;
  std::cout << "Parameters = " << transformParameters << std::endl;


  return 0;
}


