/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineDeformableTransformTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkBSplineDeformableTransform.h"
#include "itkBSplineInterpolateImageFunction.h"

#include "itkImage.h"
#include "itkRigid3DTransform.h"

/**
 * This module test the functionality of the BSplineDeformableTransform class.
 *
 */
int itkBSplineDeformableTransformTest(int, char * [] )
{

  const unsigned int SpaceDimension = 3;
  const unsigned int SplineOrder = 3;
  typedef double CoordinateRepType;
  typedef itk::BSplineDeformableTransform<CoordinateRepType,SpaceDimension,SplineOrder> 
    TransformType;
   
  typedef TransformType::ParametersType ParametersType;

  unsigned int j;

  /**
   * Define the deformable grid region, spacing and origin
   */
  typedef TransformType::RegionType RegionType;
  RegionType region;
  RegionType::SizeType   size;
  size.Fill( 10 );
  region.SetSize( size );
  std::cout << region << std::endl;

  typedef TransformType::SpacingType SpacingType;
  SpacingType spacing;
  spacing.Fill( 2.0 );

  typedef TransformType::OriginType OriginType;
  OriginType origin;
  origin.Fill( 0.0 );

  /**
   * Instantiate a transform
   */
  TransformType::Pointer transform = TransformType::New();

  transform->SetGridSpacing( spacing );
  transform->SetGridOrigin( origin );
  transform->SetGridRegion( region );
  transform->Print( std::cout );

  /** 
   * Allocate memory for the parameters
   */
  unsigned long numberOfParameters = transform->GetNumberOfParameters();
  ParametersType parameters( numberOfParameters );

  /**
   * Define N * N-D grid of spline coefficients by wrapping the
   * flat array into N images.
   * Initialize by setting all elements to zero
   */
  typedef ParametersType::ValueType CoefficientType;
  typedef itk::Image<CoefficientType,SpaceDimension> CoefficientImageType;

  CoefficientImageType::Pointer coeffImage[SpaceDimension];
  unsigned int numberOfPixels = region.GetNumberOfPixels();
  CoefficientType * dataPointer = parameters.data_block();

  for ( j = 0; j < SpaceDimension; j++ )
    {
    coeffImage[j] = CoefficientImageType::New();
    coeffImage[j]->SetRegions( region );
    coeffImage[j]->GetPixelContainer()->
      SetImportPointer( dataPointer, numberOfPixels );
    dataPointer += numberOfPixels;
    coeffImage[j]->FillBuffer( 0.0 );
    }


  /**
   * Populate the spline coefficients with some values.
   */
  CoefficientImageType::IndexType index;
  index.Fill( 5 );

  coeffImage[1]->SetPixel( index, 1.0 );

  unsigned long n = coeffImage[1]->ComputeOffset( index ) +
    numberOfPixels;

  /**
   * Set the parameters in the transform
   */
  transform->SetParameters( parameters );


  /**
   * Get the parameters back
   */

  // outParametersRef should point back to parameters
  const ParametersType & outParametersRef = transform->GetParameters();

  if ( &outParametersRef != &parameters )
    {
    std::cout << "outParametersRef should point to the same memory as parameters";
    std::cout << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  // outParametersCopy should make a copy of the parameters
  ParametersType outParametersCopy = transform->GetParameters();

  if ( outParametersCopy != parameters )
    {
    std::cout << "outParametersCopy should be the same as parameters";
    std::cout << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  if ( &outParametersCopy == &parameters )
    {
    std::cout << "outParametersCopy should point to memory different to parameters";
    std::cout << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  /**
   * Set a bulk transform
   */
  typedef itk::Rigid3DTransform<CoordinateRepType> BulkTransformType;
  BulkTransformType::Pointer bulkTransform = BulkTransformType::New();

  // optional: set bulk transform parameters

  transform->SetBulkTransform( bulkTransform );


  /**
   * Transform some points
   */
  typedef TransformType::InputPointType PointType;

  PointType inputPoint;
  PointType outputPoint;

  // point within the grid support region
  inputPoint.Fill( 9.0 );
  outputPoint = transform->TransformPoint( inputPoint );

  std::cout << "Input Point: " << inputPoint << std::endl;
  std::cout << "Output Point: " << outputPoint << std::endl;
  std::cout << std::endl;
  
  // point outside the grid support region
  inputPoint.Fill( 40.0 );
  outputPoint = transform->TransformPoint( inputPoint );

  std::cout << "Input Point: " << inputPoint << std::endl;
  std::cout << "Output Point: " << outputPoint << std::endl;
  std::cout << std::endl;

  // use the other version of TransformPoint
  typedef TransformType::WeightsType WeightsType;
  typedef TransformType::IndexType IndexType;
  typedef TransformType::ParameterIndexArrayType IndexArrayType;

  WeightsType weights( transform->GetNumberOfWeights() );
  IndexArrayType indices( transform->GetNumberOfWeights() );
  bool inside;

  inputPoint.Fill( 8.3 );
  transform->TransformPoint( inputPoint, outputPoint, weights, indices, inside );

  std::cout << "Number of Parameters: " << transform->GetNumberOfParameters() << std::endl;
  std::cout << "Number of Parameters per dimension: " << 
    transform->GetNumberOfParametersPerDimension() << std::endl;
  std::cout << "Input Point: " << inputPoint << std::endl;
  std::cout << "Output Point: " << outputPoint << std::endl;
  std::cout << "Indices: " << indices << std::endl;
  std::cout << "Weights: " << weights << std::endl;
  std::cout << "Inside: " << inside << std::endl;
  std::cout << std::endl;

  // cycling through all the parameters and weights used in the previous
  // transformation
  unsigned int numberOfCoefficientInSupportRegion = transform->GetNumberOfWeights();
  unsigned int numberOfParametersPerDimension = transform->GetNumberOfParametersPerDimension();
  unsigned int linearIndex;
  unsigned int baseIndex;

  std::cout << "Index" << "\t" << "Value" << "\t" << "Weight" << std::endl;
  for ( j = 0; j < SpaceDimension; j++ )
    {
    baseIndex = j * numberOfParametersPerDimension;
    for ( unsigned int k = 0; k < numberOfCoefficientInSupportRegion; k++ )
      {
      linearIndex = indices[k] + baseIndex;
      std::cout << linearIndex << "\t";
      std::cout << parameters[linearIndex] << "\t";
      std::cout << weights[k] << "\t";
      std::cout << std::endl;
      }
    }


  /**
   * TODO: add test to check the numerical accuarcy of the transform
   */



  /**
   * Compute the Jacobian for various points
   */
  typedef TransformType::JacobianType JacobianType;

#define PRINT_VALUE(R,C) \
  std::cout << "Jacobian[" #R "," #C "] = "; \
  std::cout << jacobian[R][C] << std::endl;

  {
    // point inside the grid support region
    inputPoint.Fill( 10.0 );
    const JacobianType & jacobian = transform->GetJacobian( inputPoint );
    PRINT_VALUE( 0, n );
    PRINT_VALUE( 1, n );
    PRINT_VALUE( 2, n );
    std::cout << std::endl;

  }


  {
    // point outside the grid support region
    inputPoint.Fill( -10.0 );
    const JacobianType & jacobian = transform->GetJacobian( inputPoint );
    PRINT_VALUE( 0, n );
    PRINT_VALUE( 1, n );
    PRINT_VALUE( 2, n );
    std::cout << std::endl;

  }


  /**
   * TODO: add test to check the numerical accuarcy of the jacobian output
   */


  /**
   * TransformVector and TransformCovariant are not applicable for this
   * transform and should throw exceptions
   */
  {
    typedef TransformType::InputVectorType VectorType;
    VectorType vector;
    vector.Fill ( 1.0 );

    bool pass = false;
    try
      {
      transform->TransformVector( vector );
      }
    catch( itk::ExceptionObject & err )
      {
      std::cout << "Caught expected exception." << std::endl;
      std::cout << err << std::endl;
      pass = true;
      }
    if ( !pass )
      {
      std::cout << "Did not catch expected exception." << std::endl;
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
      }

  }

  {
    typedef TransformType::InputCovariantVectorType VectorType;
    VectorType vector;
    vector.Fill ( 1.0 );

    bool pass = false;
    try
      {
      transform->TransformCovariantVector( vector );
      }
    catch( itk::ExceptionObject & err )
      {
      std::cout << "Caught expected exception." << std::endl;
      std::cout << err << std::endl;
      pass = true;
      }
    if ( !pass )
      {
      std::cout << "Did not catch expected exception." << std::endl;
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
      }

  }

  {
    typedef TransformType::InputVnlVectorType VectorType;
    VectorType vector;
    vector.fill ( 1.0 );

    bool pass = false;
    try
      {
      transform->TransformVector( vector );
      }
    catch( itk::ExceptionObject & err )
      {
      std::cout << "Caught expected exception." << std::endl;
      std::cout << err << std::endl;
      pass = true;
      }
    if ( !pass )
      {
      std::cout << "Did not catch expected exception." << std::endl;
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
      }

  }

  /**
   * Exercise other methods
   */
  std::cout << transform->GetGridRegion() << std::endl;
  std::cout << transform->GetGridSpacing() << std::endl;
  std::cout << transform->GetGridOrigin() << std::endl;
  std::cout << transform->GetValidRegion() << std::endl;
 
  /**
   * Parameters should remain even when the transform has been destroyed
   */
  transform = NULL;

  if ( outParametersCopy != parameters )
    {
    std::cout << "parameters should remain intact after transform is destroyed";
    std::cout << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Test passed." << std::endl;  
  return EXIT_SUCCESS;

}
