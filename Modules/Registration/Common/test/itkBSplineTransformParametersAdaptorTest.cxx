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

#include "itkBSplineTransform.h"
#include "itkBSplineTransformParametersAdaptor.h"

int itkBSplineTransformParametersAdaptorTest(int, char * [] )
{
  const unsigned int SpaceDimension = 3;
  const unsigned int SplineOrder = 3;
  typedef double CoordinateRepType;
  typedef itk::BSplineTransform<CoordinateRepType, SpaceDimension, SplineOrder> TransformType;

  /**
   * Define the transformation domain
   */

  typedef TransformType::OriginType OriginType;
  OriginType origin;
  origin.Fill( 5.0 );

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

  /**
   * Allocate memory for the parameters
   */
  typedef TransformType::ParametersType ParametersType;
  unsigned long  numberOfParameters = transform->GetNumberOfParameters();
  ParametersType parameters( numberOfParameters );
  parameters.Fill( itk::NumericTraits<ParametersType::ValueType>::ZeroValue());

  /**
   * Set the parameters in the transform
   */
  transform->SetParameters( parameters );

  typedef TransformType::ImageType CoefficientImageType;
  CoefficientImageType::IndexType index;
  index.Fill( 5 );
  transform->GetCoefficientImages()[0]->SetPixel( index, 5.0 );

  TransformType::InputPointType point;
  point.Fill( 50.0 );

  TransformType::OutputPointType outputPointBeforeAdapt = transform->TransformPoint( point );


  /**
   * Instantiate the adaptor
   *   we keep the transform domain definition the same except we increase
   *   the mesh size which increases the number of control points in the grid.
   */

  TransformType::MeshSizeType requiredMeshSize;
  for( unsigned int d = 0; d < SpaceDimension; d++ )
    {
    requiredMeshSize[d] = ( d + 1 ) * meshSize[d];
    }

  TransformType::SizeType gridSizeBefore =
    transform->GetCoefficientImages()[0]->GetLargestPossibleRegion().GetSize();

  typedef itk::BSplineTransformParametersAdaptor<TransformType> AdaptorType;
  AdaptorType::Pointer adaptor = AdaptorType::New();
  adaptor->SetTransform( transform );
  adaptor->SetRequiredTransformDomainMeshSize( requiredMeshSize );
  adaptor->SetRequiredTransformDomainOrigin( transform->GetTransformDomainOrigin() );
  adaptor->SetRequiredTransformDomainDirection( transform->GetTransformDomainDirection() );
  adaptor->SetRequiredTransformDomainPhysicalDimensions( transform->GetTransformDomainPhysicalDimensions() );
  try
    {
    adaptor->AdaptTransformParameters();
    }
  catch(...)
    {
    std::cerr << "Error in adapting transform." << std::endl;
    return EXIT_FAILURE;
    }

  ParametersType fixedParameters = adaptor->GetRequiredFixedParameters();
  std::cout << "Fixed parameters: " << fixedParameters << std::endl;
  adaptor->SetRequiredFixedParameters( fixedParameters );

  if( adaptor->GetRequiredTransformDomainMeshSize() != transform->GetTransformDomainMeshSize() )
    {
    std::cerr << "required transform domain mesh size conversion is incorrect." << std::endl;
    return EXIT_FAILURE;
    }
  if( adaptor->GetRequiredTransformDomainOrigin() != transform->GetTransformDomainOrigin() )
    {
    std::cerr << "required transform domain origin conversion is incorrect." << std::endl;
    return EXIT_FAILURE;
    }
  if( adaptor->GetRequiredTransformDomainDirection() != transform->GetTransformDomainDirection() )
    {
    std::cerr << "required transform domain direction conversion is incorrect." << std::endl;
    return EXIT_FAILURE;
    }
  if( adaptor->GetRequiredTransformDomainPhysicalDimensions() != transform->GetTransformDomainPhysicalDimensions() )
    {
    std::cerr << "required transform domain physical dimensions conversion is incorrect." << std::endl;
    return EXIT_FAILURE;
    }

  TransformType::SizeType gridSizeAfter =
    transform->GetCoefficientImages()[0]->GetLargestPossibleRegion().GetSize();

  TransformType::OutputPointType outputPointAfterAdapt = transform->TransformPoint( point );

  std::cout << "Grid size before: " << gridSizeBefore << std::endl;
  std::cout << "Grid size after: " << gridSizeAfter << std::endl;
  std::cout << point << " to (before) " << outputPointBeforeAdapt << std::endl;
  std::cout << point << " to (after) " << outputPointAfterAdapt << std::endl;

  if( outputPointBeforeAdapt.EuclideanDistanceTo( outputPointAfterAdapt ) > 1e-6 )
    {
    std::cerr << "output points don't match up before and after adapt call." << std::endl;
    return EXIT_FAILURE;
    }

  adaptor->Print( std::cout, 5 );

  return EXIT_SUCCESS;
}
