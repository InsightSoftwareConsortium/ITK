/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapePriorMAPCostFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkShapePriorMAPCostFunction.h"
#include "itkSphereSignedDistanceFunction.h"
#include "itkAmoebaOptimizer.h"

#include "itkImageRegionIteratorWithIndex.h"

/**
 * This module tests the ShapePriorMAPCostFunction class.
 *
 * The active region nodes are generated from the shape signed distance
 * function with parameters perturbed from the mean.
 *
 * Starting from the mean parameters, the cost function is optimized
 * using the Amoeba optimizers. The output parameters are compared with
 * the parameters used to generate the active region.
 *
 * The test fails if the output parameters are not within a tolerance
 * of the original parameters.
 *
 */
int itkShapePriorMAPCostFunctionTest( int, char *[])
{

  typedef float PixelType;
  const unsigned int Dimension = 2;
  typedef itk::Image<PixelType,Dimension> ImageType;

  /**
   * Set up the shape signed distance function
   */
  typedef itk::SphereSignedDistanceFunction<float,Dimension> ShapeFunctionType;
  ShapeFunctionType::Pointer shape = ShapeFunctionType::New();
  shape->Initialize();

 /**
   * Set up a statistical model of the shape parameters.
   */
  typedef itk::ShapePriorMAPCostFunction<ImageType,PixelType> CostFunctionType;
  typedef CostFunctionType::NodeType NodeType;
  typedef CostFunctionType::NodeContainerType NodeContainerType;
  CostFunctionType::ArrayType mean( shape->GetNumberOfParameters() );
  CostFunctionType::ArrayType stddev( shape->GetNumberOfParameters() );

  mean[0] = 10.0;
  mean[1] = 50.0;
  mean[2] = 50.0;

  stddev[0] = 0.1;
  stddev[1] = 2.5;
  stddev[2] = 2.5;

 
  /**
   * Set the shape parameters to be perturbation of the mean
   */
  ShapeFunctionType::ParametersType parameters( shape->GetNumberOfParameters() );
  parameters[0] = mean[0] - 0.1;
  parameters[1] = mean[1] - 4.0;
  parameters[2] = mean[2] - 6.0;
  shape->SetParameters( parameters );


  /**
   * Create an input level set and active region container
   */
  ImageType::SizeType size;
  size.Fill( 128 );
  ImageType::RegionType region;
  region.SetSize( size );

  ImageType::Pointer input = ImageType::New();
  input->SetRegions( region );
  input->Allocate();

  NodeContainerType::Pointer activeRegion  = NodeContainerType::New();


  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator iter( input, region );
  iter.GoToBegin();

  unsigned int counter = 0;
  PixelType activeRegionThreshold = 3.0;

  while ( !iter.IsAtEnd() )
    {
    ImageType::IndexType index;
    ShapeFunctionType::PointType point;
    index = iter.GetIndex();
    input->TransformIndexToPhysicalPoint( index, point );

    float value  = shape->Evaluate( point );
    iter.Set( value );

    if ( vnl_math_abs( value ) < activeRegionThreshold )
      {
      NodeType node;
      node.SetIndex( index );
      node.SetValue( value );
      activeRegion->InsertElement( counter++, node );
      }

    ++iter;
    }

  std::cout << "No. nodes: " << activeRegion->Size() << std::endl;

  /**
   * Create a dummy edge potential image.
   */
  ImageType::Pointer edgeMap = ImageType::New();
  edgeMap->SetRegions( region );
  edgeMap->Allocate();
  edgeMap->FillBuffer( 1.0 );


  /**
   * Set up the cost function
   */
  CostFunctionType::Pointer costFunction = CostFunctionType::New();

  costFunction->SetShapeFunction( shape );
  costFunction->SetActiveRegion( activeRegion );
  costFunction->SetFeatureImage( edgeMap );

  CostFunctionType::ArrayType shapeMean( shape->GetNumberOfShapeParameters() );
  CostFunctionType::ArrayType shapeStdDev( shape->GetNumberOfShapeParameters() );
  shapeMean[0] = mean[0];
  shapeStdDev[0] = stddev[0];

  costFunction->SetShapeParameterMeans( shapeMean );
  costFunction->SetShapeParameterStandardDeviations( shapeStdDev );


  // Initialize cost function before use
  try
    {
    costFunction->Initialize();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  // exercise Print method
  costFunction->Print( std::cout );

  // exercise Get methods

  /**
   * Attempt to plug the cost function into an optimizer
   */
  typedef itk::AmoebaOptimizer OptimizerType;
  OptimizerType::Pointer optimizer = OptimizerType::New();

  optimizer->SetCostFunction( costFunction );
  optimizer->SetInitialPosition( mean );
 
  optimizer->SetFunctionConvergenceTolerance( 0.1 );
  optimizer->SetParametersConvergenceTolerance( 0.5 );
  optimizer->GetOptimizer()->verbose = true;
  
  try
    {
    optimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Target parameters: " << parameters << std::endl;
  std::cout << "Final parameters: " << optimizer->GetCurrentPosition() << std::endl;

  for ( unsigned int j = 0; j < costFunction->GetNumberOfParameters(); j++ )
    {
    if ( vnl_math_abs( parameters[j] - optimizer->GetCurrentPosition()[j] ) > 0.5 )
      {
      std::cout << "Final parameters not within tolerance. " << std::endl;
      return EXIT_FAILURE;
      }
    }
  
  // excercise error testing

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
