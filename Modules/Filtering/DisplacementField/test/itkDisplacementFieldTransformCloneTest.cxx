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

const double epsilon = 1e-10;

template <typename TVector>
bool testVector( const TVector & v1, const TVector & v2 )
{
  bool pass = true;

  for( unsigned int i = 0; i < v1.Size() && i < v2.Size(); i++ )
    {
    if( std::fabs( v1[i] - v2[i] ) > epsilon )
      {
      pass = false;
      }
    }
  return pass;
}

int itkDisplacementFieldTransformCloneTest(int, char *[])
{
  //
  // Create a displacement field transform
  //
  typedef itk::DisplacementFieldTransform<double, 3> DisplacementTransformType;

  DisplacementTransformType::Pointer displacementTransform =
    DisplacementTransformType::New();
  typedef DisplacementTransformType::DisplacementFieldType FieldType;
  FieldType::Pointer field = FieldType::New(); // This is based on itk::Image

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
  if(displacementTransformClone.IsNull())
    {
    std::cerr << "Failed down cast to displacement transform.";
    return EXIT_FAILURE;
    }
  if(!testVector(displacementTransform->GetFixedParameters(),
                 displacementTransformClone->GetFixedParameters()))
    {
    std::cerr << "fixed parameters of clone do not match original." << std::endl;
    }
  FieldType::ConstPointer originalField = displacementTransform->GetDisplacementField();
  FieldType::ConstPointer cloneField = displacementTransformClone->GetDisplacementField();

  itk::ImageRegionConstIterator<FieldType> originalIt(originalField,originalField->GetLargestPossibleRegion());
  itk::ImageRegionConstIterator<FieldType> cloneIt(cloneField,cloneField->GetLargestPossibleRegion());

  for(; !originalIt.IsAtEnd() && !cloneIt.IsAtEnd(); ++originalIt, ++cloneIt)
    {
    if(!testVector(originalIt.Value(),cloneIt.Value()))
      {
      std::cerr << "Displacement Field voxel mismatch" << std::endl;
      return EXIT_FAILURE;
      }
    }
  if(!originalIt.IsAtEnd() || !cloneIt.IsAtEnd())
    {
    std::cerr << "Displacment field size mismatch" << std::endl;
    }
  return EXIT_SUCCESS;
}
