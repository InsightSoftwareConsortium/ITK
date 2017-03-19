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

#include "itkAffineTransform.h"
#include "itkCompositeTransform.h"
#include "itkDisplacementFieldTransform.h"
#include "itkImageRegionConstIterator.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


template <typename TVector>
bool testVector( const TVector & v1, const TVector & v2 )
{
  bool pass = true;
  const double tolerance = 1e-10;

  for( unsigned int i = 0; i < v1.Size() && i < v2.Size(); i++ )
    {
    if( !itk::Math::FloatAlmostEqual( v1[i], v2[i], 10, tolerance ) )
      {
      pass = false;
      }
    }
  return pass;
}

int itkDisplacementFieldTransformCloneTest( int, char *[] )
{
  const unsigned int Dimensions = 3;

  typedef double ParametersValueType;
  typedef itk::DisplacementFieldTransform< ParametersValueType, Dimensions >
                                                            DisplacementTransformType;
  typedef DisplacementTransformType::DisplacementFieldType  FieldType;

  // Create a displacement field transform
  DisplacementTransformType::Pointer displacementTransform =
    DisplacementTransformType::New();

  FieldType::Pointer field = FieldType::New();

  FieldType::SizeType   size;
  FieldType::IndexType  start;
  FieldType::RegionType region;
  int                   dimLength = 20;
  size.Fill( dimLength );
  start.Fill( 0 );
  region.SetSize( size );
  region.SetIndex( start );
  field->SetRegions( region );
  field->Allocate();

  DisplacementTransformType::OutputVectorType zeroVector;
  zeroVector.Fill( 1 );
  field->FillBuffer( zeroVector );
  displacementTransform->SetDisplacementField( field );

  DisplacementTransformType::Pointer displacementTransformClone =
    displacementTransform->Clone();

  EXERCISE_BASIC_OBJECT_METHODS( displacementTransformClone,
    DisplacementFieldTransform, Transform );


  if( displacementTransformClone.IsNull() )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Failed downcast to displacement transform.";
    return EXIT_FAILURE;
    }

  if( !testVector( displacementTransform->GetFixedParameters(),
                 displacementTransformClone->GetFixedParameters() ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Fixed parameters of clone do not match original." << std::endl;
    return EXIT_FAILURE;
    }

  FieldType::ConstPointer originalField =
    displacementTransform->GetDisplacementField();
  FieldType::ConstPointer cloneField =
    displacementTransformClone->GetDisplacementField();

  itk::ImageRegionConstIterator< FieldType > originalIt( originalField,
    originalField->GetLargestPossibleRegion() );
  itk::ImageRegionConstIterator< FieldType > cloneIt( cloneField,
    cloneField->GetLargestPossibleRegion() );

  for(; !originalIt.IsAtEnd() && !cloneIt.IsAtEnd(); ++originalIt, ++cloneIt )
    {
    if( !testVector( originalIt.Value(), cloneIt.Value() ) )
      {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Displacement Field voxel mismatch" << std::endl;
      return EXIT_FAILURE;
      }
    }

  if( !originalIt.IsAtEnd() || !cloneIt.IsAtEnd() )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Displacment field size mismatch" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
