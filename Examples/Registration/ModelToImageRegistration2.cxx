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



#include "itkImage.h"
#include "itkPointSet.h"
#include "itkPointSetToImageRegistrationMethod.h"
#include "itkMeanSquaresPointSetToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkRigid2DTransform.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkResampleImageFilter.h"



int main()
{

  const unsigned int Dimension = 2;

  typedef unsigned char MaskPixelType;

  typedef itk::Image< MaskPixelType, Dimension > MaskImageType;


  typedef itk::BoxSpatialObject< Dimension >    SpatialObjectType;

  typedef itk::SpatialObjectToImageFilter< 
                              SpatialObjectType, 
                              MaskImageType >   
                                            SpatialObjectToImageFilterType;


  typedef  signed short        PixelType;
  
  typedef itk::Image< PixelType, Dimension >  ImageType;

  typedef  unsigned char       MaskPixelType;
  
  typedef itk::Image< MaskPixelType, Dimension >  MaskImageType;

  
  typedef itk::Rigid2DTransform< double  > TransformType;

  typedef TransformType::ParametersType          ParametersType;


  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;

  typedef itk::LinearInterpolateImageFunction< 
                                    ImageType,
                                    double             > LinearInterpolatorType;


  typedef itk::PointSet< float, Dimension >       FixedPointSetType;


  typedef itk::MeanSquaresPointSetToImageMetric< 
                                    FixedPointSetType, 
                                    ImageType          >   MetricType;                                          
                                          

  typedef OptimizerType::ScalesType       OptimizerScalesType;


  typedef itk::PointSetToImageRegistrationMethod< 
                                    FixedPointSetType,
                                    ImageType  >         RegistrationType;


  typedef itk::ResampleImageFilter< 
                            ImageType, 
                            ImageType >    ResampleFilterType;



  TransformType::Pointer                transform;
  OptimizerType::Pointer                optimizer;

  LinearInterpolatorType::Pointer       linearInterpolator;

  MetricType::Pointer                   metric;

  RegistrationType::Pointer             registrationMethod;

  FixedPointSetType::Pointer            fixedPointSet;

  ImageType::ConstPointer               fixedImage;
  ImageType::ConstPointer               movingImage;

  metric              = MetricType::New();
  transform           = TransformType::New();
  optimizer           = OptimizerType::New();
  linearInterpolator  = LinearInterpolatorType::New();

  registrationMethod  = RegistrationType::New();

  registrationMethod->SetOptimizer(     optimizer     );
  registrationMethod->SetInterpolator(  linearInterpolator  );
  registrationMethod->SetMetric(        metric        );
  registrationMethod->SetTransform(     transform     );



  optimizer->SetMaximumStepLength( 2.00 );
  optimizer->SetMinimumStepLength( 0.001 );
  optimizer->SetNumberOfIterations( 300 );
  optimizer->SetRelaxationFactor( 0.90 );
  optimizer->SetGradientMagnitudeTolerance( 0.05 );

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


