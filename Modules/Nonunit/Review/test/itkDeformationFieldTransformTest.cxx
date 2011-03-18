/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformationFieldTransformTest.cxx
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

#include <iostream>

#include "itkDeformationFieldTransform.h"
#include "itkVectorInterpolateImageFunction.h"
#include "itkVectorLinearInterpolateImageFunction.h"

#define NDIMENSIONS 2
typedef itk::DeformationFieldTransform<double, NDIMENSIONS>
                                              DeformationTransformType;
typedef DeformationTransformType::ScalarType  ScalarType;

const ScalarType epsilon = 1e-10;

template <typename TPoint>
bool samePoint( const TPoint & p1, const TPoint & p2 )
  {
  bool pass=true;
  for ( unsigned int i = 0; i < TPoint::PointDimension; i++ )
    {
    if( vcl_fabs( p1[i] - p2[i] ) > epsilon )
      pass=false;
    }
  return pass;
  }


int itkDeformationFieldTransformTest(int ,char *[] )
{

  /* NOTE
   * Requires updating when DeformationFieldTransform is
   * fully implemented with operations for transforming
   * vectors as well. Currently this just tests transforming
   * points. */

  typedef  itk::Matrix<ScalarType, NDIMENSIONS, NDIMENSIONS>  Matrix2Type;
  typedef  itk::Vector<ScalarType, NDIMENSIONS>               Vector2Type;

  /* Create a deformation field transform */
  DeformationTransformType::Pointer deformationTransform =
      DeformationTransformType::New();
  typedef DeformationTransformType::DeformationFieldType FieldType;
  FieldType::Pointer field = FieldType::New(); //This is based on itk::Image

  FieldType::SizeType size;
  FieldType::IndexType start;
  FieldType::RegionType region;
  size.Fill( 30 );
  start.Fill( 0 );
  region.SetSize( size );
  region.SetIndex( start );
  field->SetRegions( region );
  field->Allocate();

  DeformationTransformType::OutputVectorType zeroVector;
  zeroVector.Fill( 0 );
  field->FillBuffer( zeroVector );

  /*
   * Set a deformation field that has only one point with
   * a non-zero vector. */
  FieldType::IndexType nonZeroFieldIndex;
  nonZeroFieldIndex[0] = 3;
  nonZeroFieldIndex[1] = 4;
  ScalarType data1[] = {4,-2.5};
  DeformationTransformType::OutputVectorType nonZeroFieldVector(data1);
  field->SetPixel( nonZeroFieldIndex, nonZeroFieldVector );

  deformationTransform->SetDeformationField( field );

  std::cout << "deformationTransform: " << std::endl
            << deformationTransform << std::endl;

  /* Test transforming some points. */

  DeformationTransformType::InputPointType testPoint;
  DeformationTransformType::OutputPointType deformOutput, deformTruth;

  /* Test a point with non-zero deformation */
  testPoint[0] = nonZeroFieldIndex[0];
  testPoint[1] = nonZeroFieldIndex[1];
  deformTruth[0] = nonZeroFieldIndex[0] + nonZeroFieldVector[0];
  deformTruth[1] = nonZeroFieldIndex[1] + nonZeroFieldVector[1];
  deformOutput = deformationTransform->TransformPoint( testPoint );
  std::cout << "point 1 transformed: " << deformOutput << std::endl;
  if( !samePoint( deformOutput, deformTruth ) )
      {
      std::cout << "Failed transforming point 1." << std::endl;
      return EXIT_FAILURE;
      }

  /* Test a point that should have zero deformation */
  testPoint[0] = nonZeroFieldIndex[0]+2;
  testPoint[1] = nonZeroFieldIndex[1]+1;
  deformTruth = testPoint;
  deformOutput = deformationTransform->TransformPoint( testPoint );
  std::cout << "zero-point transformed: " << deformOutput << std::endl;
  if( !samePoint( deformOutput, deformTruth ) )
      {
      std::cout << "Failed transforming zero point." << std::endl;
      return EXIT_FAILURE;
      }

  /* Test a non-integer point using the linear interpolator.
   * The default interpolator thus far is linear, but set it
   * just in case, and to test the set function and test TransforPoint's
   * handling of changed deformation field and interpolator objects. */
  typedef itk::VectorLinearInterpolateImageFunction<FieldType, ScalarType>
    LinearInterpolatorType;
  LinearInterpolatorType::Pointer interpolator = LinearInterpolatorType::New();
  std::cout << "Interpolator:" << std::endl << interpolator;
  deformationTransform->SetInterpolator( interpolator );
  std::cout << "\n***Transform after add interpolator: \n"
    << deformationTransform << std::endl;

  ScalarType xoffset = 0.4;
  testPoint[0] = nonZeroFieldIndex[0] + xoffset;
  testPoint[1] = nonZeroFieldIndex[1];
  deformTruth[0] = (nonZeroFieldIndex[0] + 1) * xoffset +
    ( nonZeroFieldIndex[0] + nonZeroFieldVector[0] ) * (1 - xoffset);
  deformTruth[1] = nonZeroFieldIndex[1] * xoffset +
    ( nonZeroFieldIndex[1] + nonZeroFieldVector[1] ) * (1 - xoffset);
  deformOutput = deformationTransform->TransformPoint( testPoint );
  std::cout << "Transform point with offset: " << std::endl
            << "  Test point: " << testPoint << std::endl
            << "  Truth: " << deformTruth << std::endl
            << "  Output: " << deformOutput << std::endl;
  if( !samePoint( deformOutput, deformTruth ) )
      {
      std::cout << "Failed transforming offset point." << std::endl;
      return EXIT_FAILURE;
      }

  /* Test IsLinear()
   * Should always return false */
  if( deformationTransform->IsLinear() )
    {
    std::cout << "DeformationFieldTransform returned 'true' for IsLinear()."
      " Expected 'false'." << std::endl;
    return EXIT_FAILURE;
    }

  /* Test inverse transform */

  /* We haven't set an inverse deformation field for the inverse deformation
   * transform, so we should get a false return here */
  DeformationTransformType::Pointer inverseTransform
    = DeformationTransformType::New();
  if( deformationTransform->GetInverse( inverseTransform ) )
    {
    std::cout << "Expected GetInverse() to fail." << std::endl;
    return EXIT_FAILURE;
    }

  /* Add an inverse deformation field */
  FieldType::Pointer inverseField = FieldType::New();
  DeformationTransformType::OutputVectorType inverseFieldVector;
  FieldType::IndexType inverseFieldIndex;
  /* Use same initializers that we used above */
  inverseField->SetRegions( region );
  inverseField->Allocate();
  inverseField->FillBuffer( zeroVector );
  inverseFieldVector[0] = -1;
  inverseFieldVector[1] = -2;
  inverseFieldIndex[0] = 7;
  inverseFieldIndex[1] = 11;
  inverseField->SetPixel( inverseFieldIndex, inverseFieldVector );

  deformationTransform->SetInverseDeformationField( inverseField );

  /* Retrieve the inverse transform */
  if( !deformationTransform->GetInverse( inverseTransform ) )
    {
    std::cout << "Expected GetInverse() to succeed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Transform a point using inverse. Not much of a different test
   * than for forwards transform. */
  DeformationTransformType::OutputPointType inverseTruth, inverseOutput;
  testPoint[0] = inverseFieldIndex[0];
  testPoint[1] = inverseFieldIndex[1];
  inverseTruth[0] = testPoint[0] + inverseFieldVector[0];
  inverseTruth[1] = testPoint[1] + inverseFieldVector[1];
  inverseOutput = inverseTransform->TransformPoint( testPoint );
  std::cout << "Transform point with inverse transform: "
            << std::endl
            << "  Test point: " << testPoint << std::endl
            << "  Truth: " << inverseTruth << std::endl
            << "  Output: " << inverseOutput << std::endl;
  if( !samePoint( inverseOutput, inverseTruth ) )
    {
    std::cout << "Failed to transform point with inverse transform."
              << std::endl;
    return EXIT_FAILURE;
    }
  /* Get inverse transform again, but using other accessor. */
  DeformationTransformType::Pointer inverseTransform2;
  inverseTransform2 = dynamic_cast<DeformationTransformType*>
    ( deformationTransform->GetInverseTransform().GetPointer() );
  if( ! inverseTransform2 )
    {
    std::cout << "Failed calling GetInverseTransform()." << std::endl;
    return EXIT_FAILURE;
    }
  inverseOutput = inverseTransform2->TransformPoint( testPoint );
  if( !samePoint( inverseOutput, inverseTruth ) )
    {
    std::cout << "Failed transform point with 2nd inverse transform."
              << std::endl;
    return EXIT_FAILURE;
    }

  /* Test GetJacobian - Since there are no parameters for this transform,
   * the Jacobian shouldn't be requested and will throw an exception. */
  DeformationTransformType::JacobianType jacobian;
  DeformationTransformType::InputPointType inputPoint;
  inputPoint[0]=1;
  inputPoint[1]=2;
  bool caughtException = false;
  try
    {
    jacobian = deformationTransform->GetJacobian( inputPoint );
    }
  catch( itk::ExceptionObject & e )
    {
    std::string description(e.GetDescription());
    caughtException =
      description.find("DeformationFieldTransform") != std::string::npos;
    }
  if( !caughtException )
    {
    std::cout << "Expected GetJacobian to throw exception." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Passed Jacobian test." << std::endl;

  return EXIT_SUCCESS;
}
