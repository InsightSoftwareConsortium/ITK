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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/

#include "itkBSplineTransform.h"


#include "itkTextOutput.h"

/**
 * This module test the functionality of the BSplineTransform class.
 *
 */
int itkBSplineTransformTest1()
{

  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New() );

  // Uncomment the following if you want to see each message independently
  // itk::OutputWindow::GetInstance()->PromptUserOn();

  const unsigned int SpaceDimension = 3;
  const unsigned int SplineOrder = 3;
  typedef double CoordinateRepType;
  typedef itk::BSplineTransform
  <CoordinateRepType, SpaceDimension, SplineOrder> TransformType;

  typedef TransformType::ParametersType ParametersType;

  unsigned int j;

  /**
   * Define the transformation domain
   */

  typedef TransformType::OriginType OriginType;
  OriginType origin;
  origin.Fill( 0.0 );

  typedef TransformType::PhysicalDimensionsType PhysicalDimensionsType;
  PhysicalDimensionsType dimensions;
  dimensions.Fill( 100 );

  typedef TransformType::MeshSizeType MeshSizeType;
  MeshSizeType meshSize;
  meshSize.Fill( 10 );

  typedef TransformType::DirectionType DirectionType;
  DirectionType direction;
  direction.SetIdentity();

  /**
   * Instantiate a transform
   */
  TransformType::Pointer transform = TransformType::New();

  /**
   * Set fixed parameters which store the following information:
   *     transform domain size
   *     transform domain origin
   *     transform domain spacing
   *     transform domain direction
   *     transform domain mesh size
   *     spline order
   *  The size of these is equal to the  NInputDimensions
   */

  ParametersType fixedParameters;
  fixedParameters.SetSize( SpaceDimension * ( SpaceDimension + 3 ) );

  unsigned int fixedParametersCount = 0;
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    fixedParameters[fixedParametersCount++] = meshSize[i] + SplineOrder;
    }
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    double delta = dimensions[i] / static_cast<double>( meshSize[i] );
    fixedParameters[fixedParametersCount++] = origin[i] - delta;
    }
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    fixedParameters[fixedParametersCount++] = dimensions[i] /
      static_cast<double>( meshSize[i] );
    }
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    for( j = 0; j < SpaceDimension; j++ )
      {
      fixedParameters[fixedParametersCount++] = direction[i][j];
      }
    }

  std::cout << "Fixed parameters set: " << fixedParameters << std::endl;
  transform->SetFixedParameters( fixedParameters );
  ParametersType returnedParameters = transform->GetFixedParameters();
  std::cout << "Fixed parameters returned: " << returnedParameters << std::endl;

  if( fixedParameters != returnedParameters )
    {
    std::cout << "Set fixed parameters do not equal returned fixed parameters."
       << std::endl;
    return EXIT_FAILURE;
    }

  transform->Print( std::cout );

  /**
   * Allocate memory for the parameters
   */
  unsigned long  numberOfParameters = transform->GetNumberOfParameters();
  ParametersType parameters( numberOfParameters );
  parameters.Fill( itk::NumericTraits<ParametersType::ValueType>::ZeroValue() );

  /**
   * Define N * N-D grid of spline coefficients by wrapping the
   * flat array into N images.
   * Initialize by setting all elements to zero
   */
  typedef ParametersType::ValueType                   CoefficientType;
  typedef itk::Image<CoefficientType, SpaceDimension> CoefficientImageType;

  CoefficientImageType::Pointer  coeffImage[SpaceDimension];
  CoefficientImageType::SizeType size;
  unsigned int                   numberOfControlPoints = 0;
  for( j = 0; j < SpaceDimension; j++ )
    {
    size[j] = ( meshSize[j] + SplineOrder );
    numberOfControlPoints += size[j];
    }
  CoefficientType * dataPointer = parameters.data_block();
  for( j = 0; j < SpaceDimension; j++ )
    {
    coeffImage[j] = CoefficientImageType::New();
    coeffImage[j]->SetRegions( size );
    coeffImage[j]->GetPixelContainer()->
    SetImportPointer( dataPointer, numberOfControlPoints );
    dataPointer += numberOfControlPoints;
    coeffImage[j]->FillBuffer( 0.0 );
    }

  /**
   * Populate the spline coefficients with some values.
   */
  CoefficientImageType::IndexType index;
  index.Fill( 5 );

  coeffImage[1]->SetPixel( index, 1.0 );

  unsigned long n = coeffImage[1]->ComputeOffset( index )
    + numberOfControlPoints;

  /**
   * Set the parameters in the transform
   */
  transform->SetParameters( parameters );

  // outParametersCopy should make a copy of the parameters
  ParametersType outParametersCopy = transform->GetParameters();

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

  // point inside the grid support region
  inputPoint.Fill( 2.0 );
  outputPoint = transform->TransformPoint( inputPoint );

  std::cout << "Input Point: " << inputPoint << std::endl;
  std::cout << "Output Point: " << outputPoint << std::endl;
  std::cout << std::endl;

  // point inside the grid support region
  inputPoint.Fill( 15.9 );
  outputPoint = transform->TransformPoint( inputPoint );

  std::cout << "Input Point: " << inputPoint << std::endl;
  std::cout << "Output Point: " << outputPoint << std::endl;
  std::cout << std::endl;

  // point outside the grid support region
  inputPoint.Fill( 1.9 );
  outputPoint = transform->TransformPoint( inputPoint );

  std::cout << "Input Point: " << inputPoint << std::endl;
  std::cout << "Output Point: " << outputPoint << std::endl;
  std::cout << std::endl;

  // point outside the grid support region
  inputPoint.Fill( 16.0 );
  outputPoint = transform->TransformPoint( inputPoint );

  std::cout << "Input Point: " << inputPoint << std::endl;
  std::cout << "Output Point: " << outputPoint << std::endl;
  std::cout << std::endl;

  // use the other version of TransformPoint
  typedef TransformType::WeightsType             WeightsType;
  typedef TransformType::ParameterIndexArrayType IndexArrayType;

  WeightsType    weights( transform->GetNumberOfWeights() );
  IndexArrayType indices( transform->GetNumberOfWeights() );
  bool           inside;

  inputPoint.Fill( 8.3 );
  transform->TransformPoint( inputPoint, outputPoint, weights, indices, inside );

  std::cout << "Number of Parameters: "
            << transform->GetNumberOfParameters() << std::endl;
  std::cout << "Number of Parameters per dimension: "
            << transform->GetNumberOfParametersPerDimension() << std::endl;
  std::cout << "Input Point: " << inputPoint << std::endl;
  std::cout << "Output Point: " << outputPoint << std::endl;
  std::cout << "Indices: " << indices << std::endl;
  std::cout << "Weights: " << weights << std::endl;
  std::cout << "Inside: " << inside << std::endl;
  std::cout << std::endl;

  // cycling through all the parameters and weights used in the previous
  // transformation
  unsigned int numberOfCoefficientInSupportRegion =
    transform->GetNumberOfWeights();
  unsigned int numberOfParametersPerDimension =
    transform->GetNumberOfParametersPerDimension();
  unsigned int linearIndex;
  unsigned int baseIndex;

  std::cout << "Index" << "\t" << "Value" << "\t" << "Weight" << std::endl;
  for( j = 0; j < SpaceDimension; j++ )
    {
    baseIndex = j * numberOfParametersPerDimension;
    for( unsigned int k = 0; k < numberOfCoefficientInSupportRegion; k++ )
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

#define PRINT_VALUE(R, C) \
  std::cout << "Jacobian[" #R "," #C "] = "; \
  std::cout << jacobian[R][C] << std::endl;

    {
    // point inside the grid support region
    inputPoint.Fill( 7.5 );
    JacobianType jacobian;
    transform->ComputeJacobianWithRespectToParameters( inputPoint, jacobian );
    PRINT_VALUE( 0, n );
    PRINT_VALUE( 1, n );
    PRINT_VALUE( 2, n );
    std::cout << std::endl;
    }

    {
    // point outside the grid support region
    inputPoint.Fill( -10.0 );
    JacobianType jacobian;
    transform->ComputeJacobianWithRespectToParameters( inputPoint, jacobian );
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
    vector.Fill( 1.0 );

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
    if( !pass )
      {
      std::cout << "Did not catch expected exception." << std::endl;
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
      }
    }

    {
    typedef TransformType::InputCovariantVectorType VectorType;
    VectorType vector;
    vector.Fill( 1.0 );

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
    if( !pass )
      {
      std::cout << "Did not catch expected exception." << std::endl;
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
      }
    }

    {
    typedef TransformType::InputVnlVectorType VectorType;
    VectorType vector;
    vector.fill( 1.0 );

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
    if( !pass )
      {
      std::cout << "Did not catch expected exception." << std::endl;
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
      }
    }

    {
    bool pass = false;
    try
      {
      ParametersType temp( transform->GetNumberOfParameters() - 1 );
      temp.Fill( 4.0 );
      transform->SetParameters( temp );
      }
    catch( itk::ExceptionObject & err )
      {
      std::cout << "Caught expected exception." << std::endl;
      std::cout << err << std::endl;
      pass = true;
      }
    if( !pass )
      {
      std::cout << "Did not catch expected exception." << std::endl;
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
      }
    }

  /**
   * Exercise other methods
   */
  std::cout << transform->GetTransformDomainPhysicalDimensions() << std::endl;
  std::cout << transform->GetTransformDomainOrigin() << std::endl;
  std::cout << transform->GetTransformDomainMeshSize() << std::endl;
  std::cout << transform->GetTransformDomainDirection() << std::endl;

  typedef itk::BSplineTransform<CoordinateRepType, SpaceDimension, 2> EvenOrderTransformType;
  EvenOrderTransformType::Pointer evenOrderTransform = EvenOrderTransformType::New();
  if( evenOrderTransform.IsNull() )
    {
    return EXIT_FAILURE;
    }

  /**
   * Parameters should remain even when the transform has been destroyed
   */
  transform = ITK_NULLPTR;

  if( outParametersCopy != parameters )
    {
    std::cout << "parameters should remain intact after transform is destroyed";
    std::cout << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /**
   * Exercise the SetIdentity() Method
   */
    {
    std::cout << "Exercising SetIdentity() " << std::endl;
    TransformType::Pointer transform2 = TransformType::New();
    transform2->SetTransformDomainOrigin( origin );
    transform2->SetTransformDomainPhysicalDimensions( dimensions );
    transform2->SetTransformDomainMeshSize( meshSize );
    transform2->SetTransformDomainDirection( direction );
    transform2->SetIdentity();
    TransformType::ParametersType parameters2 = transform2->GetParameters();
    const unsigned int            numberOfParameters2 = transform2->GetNumberOfParameters();
    std::cout << "numberOfParameters =  " << numberOfParameters2 << std::endl;
    for( unsigned int i = 0; i < numberOfParameters2; i++ )
      {
      if( std::fabs( parameters2[i] ) > 1e-10 )
        {
        std::cerr << "SetIdentity failed, parameters are not null "
                  << "after invoking SetIdentity() " << std::endl;
        return EXIT_FAILURE;
        }
      }
    } // end of SetIdentity() test

  std::cout << "Test passed.  Woohoo!" << std::endl;
  return EXIT_SUCCESS;
}

int itkBSplineTransformTest2()
{
  /**
   * This function tests the Set/GetCoefficientImage interface
   */
  itk::OutputWindow::SetInstance(itk::TextOutput::New() );

  unsigned int j;

  /**
   * Define a vector field as Dimension number of images
   */
  const unsigned int Dimension = 2;
  typedef double PixelType;

  typedef itk::Image<PixelType, Dimension> ImageType;

  // Set up the transform
  const unsigned int SplineOrder = 3;
  typedef double CoordRep;

  typedef itk::BSplineTransform<CoordRep, Dimension, SplineOrder> TransformType;

  TransformType::InputPointType  inputPoint;
  TransformType::OutputPointType outputPoint;

  TransformType::Pointer transform = TransformType::New();

  // Set up field spacing, origin, region
  double                spacing[Dimension];
  double                origin[Dimension];
  ImageType::SizeType   size;
  ImageType::RegionType region;
  for( j = 0; j < Dimension; j++ )
    {
    spacing[j] = 10.0;
    origin[j]  = -10.0;
    }

  size[0] = 5;
  size[1] = 7;

  region.SetSize( size );

  TransformType::CoefficientImageArray field;
  for( j = 0; j < Dimension; j++ )
    {
    field[j] = ImageType::New();
    field[j]->SetSpacing( spacing );
    field[j]->SetOrigin( origin );
    field[j]->SetRegions( region );
    field[j]->Allocate();
    }

  // fill the field with a constant displacment
  itk::Vector<double, Dimension> v;
  v[0] = 5;
  v[1] = 7;
  for( j = 0; j < Dimension; j++ )
    {
    field[j]->FillBuffer( v[j] );
    }

  // This should generate a warning about parameters not being set
  inputPoint.Fill( 0.0 );
  outputPoint = transform->TransformPoint( inputPoint );

  // Set the coefficient images
  transform->SetCoefficientImages( field );

  // Exercise get and print methods
  transform->Print( std::cout );
  std::cout << "CoefficientImage[0]: "
            << transform->GetCoefficientImages()[0].GetPointer() << std::endl;

  /**
   * Transform some points
   */
  try
    {

    // try a point inside the valid region
    inputPoint.Fill( 10.0 );
    outputPoint = transform->TransformPoint( inputPoint );
    std::cout << " InputPoint: " << inputPoint;
    std::cout << " OutputPoint: " << outputPoint;
    std::cout << std::endl;

    // try a point on the valid region boundary
    inputPoint.Fill( 0.0 );
    outputPoint = transform->TransformPoint( inputPoint );
    std::cout << " InputPoint: " << inputPoint;
    std::cout << " OutputPoint: " << outputPoint;
    std::cout << std::endl;

    // try a point on the valid region boundary
    inputPoint[0] = 19.9;
    inputPoint[1] = 30.0;
    outputPoint = transform->TransformPoint( inputPoint );
    std::cout << " InputPoint: " << inputPoint;
    std::cout << " OutputPoint: " << outputPoint;
    std::cout << std::endl;

    // try a point outside the valid region
    inputPoint[0] = 220.0;
    inputPoint[1] = 230.0;
    outputPoint = transform->TransformPoint( inputPoint );
    std::cout << " InputPoint: " << inputPoint;
    std::cout << " OutputPoint: " << outputPoint;
    std::cout << std::endl;

    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed. Woohoo!" << std::endl;
  return EXIT_SUCCESS;
}

int itkBSplineTransformTest3()
{

  // This function tests the SetParametersByValue interface

  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New() );

  const unsigned int SpaceDimension = 3;
  const unsigned int SplineOrder = 3;
  typedef double CoordinateRepType;
  typedef itk::BSplineTransform
  <CoordinateRepType, SpaceDimension, SplineOrder> TransformType;

  typedef TransformType::ParametersType ParametersType;

  unsigned int j;

  /**
   * Define the transformation domain
   */

  typedef TransformType::OriginType OriginType;
  OriginType origin;
  origin.Fill( 0.0 );

  typedef TransformType::PhysicalDimensionsType PhysicalDimensionsType;
  PhysicalDimensionsType dimensions;
  dimensions.Fill( 100 );

  typedef TransformType::MeshSizeType MeshSizeType;
  MeshSizeType meshSize;
  meshSize.Fill( 10 );

  typedef TransformType::DirectionType DirectionType;
  DirectionType direction;
  direction.SetIdentity();

  /**
   * Instantiate a transform
   */
  TransformType::Pointer transform = TransformType::New();

  transform->SetTransformDomainOrigin( origin );
  transform->SetTransformDomainPhysicalDimensions( dimensions );
  transform->SetTransformDomainMeshSize( meshSize );
  transform->SetTransformDomainDirection( direction );

  transform->Print( std::cout );

  /**
   * Allocate memory for the parameters
   */
  unsigned long  numberOfParameters = transform->GetNumberOfParameters();
  ParametersType parameters( numberOfParameters );
  parameters.Fill( itk::NumericTraits<ParametersType::ValueType>::ZeroValue());

  /**
   * Define N * N-D grid of spline coefficients by wrapping the
   * flat array into N images.
   * Initialize by setting all elements to zero
   */
  typedef ParametersType::ValueType                   CoefficientType;
  typedef itk::Image<CoefficientType, SpaceDimension> CoefficientImageType;

  CoefficientImageType::Pointer  coeffImage[SpaceDimension];
  CoefficientImageType::SizeType size;
  unsigned int                   numberOfControlPoints = 0;
  for( j = 0; j < SpaceDimension; j++ )
    {
    size[j] = ( meshSize[j] + SplineOrder );
    numberOfControlPoints += size[j];
    }
  CoefficientType * dataPointer = parameters.data_block();
  for( j = 0; j < SpaceDimension; j++ )
    {
    coeffImage[j] = CoefficientImageType::New();
    coeffImage[j]->SetRegions( size );
    coeffImage[j]->GetPixelContainer()->
    SetImportPointer( dataPointer, numberOfControlPoints );
    dataPointer += numberOfControlPoints;
    coeffImage[j]->FillBuffer( 0.0 );
    }

  /**
   * Populate the spline coefficients with some values.
   */
  CoefficientImageType::IndexType index;
  index.Fill( 5 );

  coeffImage[1]->SetPixel( index, 1.0 );

  /**
   * Set the parameters in the transform
   */
  transform->SetParametersByValue( parameters );

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

  /**
   * Internal parameters should remain even when the external parameters
   *  has been destroyed
   */
  parameters = ParametersType(0);

  // point within the grid support region
  inputPoint.Fill( 9.0 );
  outputPoint = transform->TransformPoint( inputPoint );

  std::cout << "Input Point: " << inputPoint << std::endl;
  std::cout << "Output Point: " << outputPoint << std::endl;
  std::cout << std::endl;

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}

int itkBSplineTransformTest(int, char * [] )
{
  bool failed;

  failed = itkBSplineTransformTest1();
  if( failed )
    {
    return EXIT_FAILURE;
    }

  failed = itkBSplineTransformTest2();
  if( failed )
    {
    return EXIT_FAILURE;
    }

  failed = itkBSplineTransformTest3();
  if( failed )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
