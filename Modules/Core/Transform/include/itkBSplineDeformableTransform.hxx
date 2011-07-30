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
#ifndef __itkBSplineDeformableTransform_hxx
#define __itkBSplineDeformableTransform_hxx

#include "itkBSplineDeformableTransform.h"
#include "itkContinuousIndex.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

// Constructor with default arguments
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::BSplineDeformableTransform() : Superclass( SpaceDimension, 0 )
{
  // Initialize images
  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    this->m_CoefficientImages[j] = ImageType::New();
    this->m_JacobianImages[j] = JacobianImageType::New();
    }

  this->m_InternalParametersBuffer = ParametersType( 0 );
  // Make sure the parameters pointer is not NULL after construction.
  this->m_InputParametersPointer = &( this->m_InternalParametersBuffer );

  // Instantiate a weights function
  this->m_WeightsFunction = WeightsFunctionType::New();

  /** Fixed Parameters store the following information:
   *     transform domain size
   *     transform domain origin
   *     transform domain spacing
   *     transform domain direction
   *     transform domain mesh size
   *     spline order
   *  The size of these is equal to the  NInputDimensions
   */
  //For example 3D image has FixedParameters of:
  //[size[0],size[1],size[2],
  //origin[0],origin[1],origin[2],
  //spacing[0],spacing[1],spacing[2],
  //dir[0][0],dir[1][0],dir[2][0],
  //dir[0][1],dir[1][1],dir[2][1],
  //dir[0][2],dir[1][2],dir[2][2]]

  this->m_TransformDomainOrigin.Fill( 0.0 );
  this->m_TransformDomainPhysicalDimensions.Fill( 1.0 );
  this->m_TransformDomainDirection.SetIdentity();
  this->m_TransformDomainDirectionInverse.SetIdentity();
  this->m_TransformDomainMeshSize.Fill( 0 );

  SizeType meshSize;
  meshSize.Fill( 1 );

  this->SetTransformDomainMeshSize( meshSize );

  this->SetFixedParametersFromTransformDomainInformation();

  this->SetCoefficientImageInformationFromFixedParameters();

  this->m_LastJacobianIndex.Fill( 0 );
}

// Destructor
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::~BSplineDeformableTransform()
{}

// Get the number of parameters
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
unsigned int
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::GetNumberOfParameters() const
{
  // The number of parameters equal SpaceDimension * number of
  // of pixels in the grid region.
  return ( SpaceDimension * this->GetNumberOfParametersPerDimension() );
}

// Get the number of parameters per dimension
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
unsigned int
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::GetNumberOfParametersPerDimension() const
{
  // The number of parameters per dimension equal number of
  // of pixels in the grid region.
  unsigned int numberOfParametersPerDimension = 1;
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    numberOfParametersPerDimension *= ( this->m_TransformDomainMeshSize[i] +
      SplineOrder );
    }
  return numberOfParametersPerDimension;
}

// Set the transform origin
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetTransformDomainOrigin( const OriginType &origin )
{
  if( this->m_TransformDomainOrigin != origin )
    {
    this->m_TransformDomainOrigin = origin;
    this->SetFixedParametersFromTransformDomainInformation();
    this->SetCoefficientImageInformationFromFixedParameters();

    this->Modified();
    }
}

// Set the transform dimensions
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetTransformDomainPhysicalDimensions( const PhysicalDimensionsType &dims )
{
  if( this->m_TransformDomainPhysicalDimensions != dims )
    {
    this->m_TransformDomainPhysicalDimensions = dims;
    this->SetFixedParametersFromTransformDomainInformation();
    this->SetCoefficientImageInformationFromFixedParameters();

    this->Modified();
    }
}

// Set the transform
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetTransformDomainDirection( const DirectionType &direction )
{
  if( this->m_TransformDomainDirection != direction )
    {
    this->m_TransformDomainDirection = direction;
    this->m_TransformDomainDirectionInverse = direction.GetInverse();
    this->SetFixedParametersFromTransformDomainInformation();
    this->SetCoefficientImageInformationFromFixedParameters();

    this->Modified();
    }
}

// Set the transform domain mesh size
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetTransformDomainMeshSize( const MeshSizeType &meshSize )
{
  if( this->m_TransformDomainMeshSize != meshSize )
    {
    this->m_TransformDomainMeshSize = meshSize;

    this->m_Jacobian.SetSize( SpaceDimension, this->GetNumberOfParameters() );

    // Input parameters point to internal buffer => using default parameters.
    if( this->m_InputParametersPointer == &( this->m_InternalParametersBuffer ) )
      {
      // Check if we need to resize the default parameter buffer.
      if( this->m_InternalParametersBuffer.GetSize() !=
        this->GetNumberOfParameters() )
        {
        this->m_InternalParametersBuffer.SetSize(
          this->GetNumberOfParameters() );
        // Fill with zeros for identity.
        this->m_InternalParametersBuffer.Fill( 0 );
        }
      }
    this->SetFixedParametersFromTransformDomainInformation();
    this->SetCoefficientImageInformationFromFixedParameters();

    this->Modified();
    }
}

// Set the parameters
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetIdentity()
{
  if( this->m_InputParametersPointer == &( this->m_InternalParametersBuffer ) )
    {
    //If this->m_InternalParametersBuffer is the this->m_InputParametersPointer
    this->m_InternalParametersBuffer.Fill( 0.0 );
    }
  else
    {
    //Should not be allowed to modify a const parameter set, so
    //make an internal representation that is an identity mapping
    this->m_InternalParametersBuffer.SetSize( this->GetNumberOfParameters() );
    this->m_InternalParametersBuffer.Fill( 0.0 );
    }
  this->SetParameters( this->m_InternalParametersBuffer );

  this->Modified();
}

// Set the parameters
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetParameters( const ParametersType &parameters )
{
  // check if the number of parameters match the
  // expected number of parameters

  if( parameters.Size() != this->GetNumberOfParameters() )
    {
    itkExceptionMacro( "Mismatch between parameters size "
      << parameters.Size() << " and expected number of parameters "
      << this->GetNumberOfParameters()
      << ( this->m_CoefficientImages[0]->
      GetLargestPossibleRegion().GetNumberOfPixels() == 0 ?
      ". \nSince the size of the grid region is 0, perhaps you forgot to "
      "SetGridRegion or SetFixedParameters before setting the Parameters."
      : "" ) );
    }

  if( &parameters != &( this->m_InternalParametersBuffer ) )
    {
    // Clean up this->m_InternalParametersBuffer becasue we will
    // use an externally supplied set of parameters as the buffer
    this->m_InternalParametersBuffer = ParametersType( 0 );
    }

  // Keep a reference to the input parameters
  // directly from the calling environment.
  // this requires that the parameters persist
  // in the calling evironment while being used
  // here.
  this->m_InputParametersPointer = &parameters;

  // Wrap flat array as images of coefficients
  this->WrapAsImages();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();
}

// Set the parameters by value
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetParametersByValue( const ParametersType &parameters )
{
  // check if the number of parameters match the
  // expected number of parameters
  if( parameters.Size() != this->GetNumberOfParameters() )
    {
    itkExceptionMacro( "Mismatched between parameters size "
      << parameters.size() << " and region size "
      << this->GetNumberOfParameters() );
    }

  // copy parameters to this->m_InternalParametersBuffer
  this->m_InternalParametersBuffer = parameters;
  this->SetParameters( this->m_InternalParametersBuffer );
}

template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetCoefficientImageInformationFromFixedParameters()
{

  // Fixed Parameters store the following information:
  //  grid size
  //  grid origin
  //  grid spacing
  //  grid direction
  //  The size of these is equal to the  NInputDimensions

  // Set the grid size parameters

  SizeType gridSize;
  MeshSizeType meshSize;
  for( unsigned int i = 0; i < NDimensions; i++ )
    {
    gridSize[i] = static_cast<int>( this->m_FixedParameters[i] );
    meshSize[i] = gridSize[i] - SplineOrder;
    }
  this->m_CoefficientImages[0]->SetRegions( gridSize );
  this->m_JacobianImages[0]->SetRegions( gridSize );

  this->SetTransformDomainMeshSize( meshSize );

  // Set the origin parameters

  OriginType origin;
  for( unsigned int i = 0; i < NDimensions; i++ )
    {
    origin[i] = this->m_FixedParameters[NDimensions + i];
    }
  this->m_CoefficientImages[0]->SetOrigin( origin );
  this->m_JacobianImages[0]->SetOrigin( origin );

  // Set the spacing parameters

  SpacingType spacing;
  for( unsigned int i = 0; i < NDimensions; i++ )
    {
    spacing[i] = this->m_FixedParameters[2 * NDimensions + i];
    }
  this->m_CoefficientImages[0]->SetSpacing( spacing );
  this->m_JacobianImages[0]->SetSpacing( spacing );

  // Set the direction parameters

  DirectionType direction;
  for( unsigned int di = 0; di < NDimensions; di++ )
    {
    for( unsigned int dj = 0; dj < NDimensions; dj++ )
      {
      direction[di][dj] =
        this->m_FixedParameters[3 * NDimensions + ( di * NDimensions + dj )];
      }
    }
  this->m_CoefficientImages[0]->SetDirection( direction );
  this->m_JacobianImages[0]->SetDirection( direction );

  // Copy the information to the rest of the images

  for( unsigned int i = 1; i < SpaceDimension; i++ )
    {
    this->m_CoefficientImages[i]->CopyInformation(
      this->m_CoefficientImages[0] );
    this->m_CoefficientImages[i]->SetRegions(
      this->m_CoefficientImages[0]->GetLargestPossibleRegion() );
    this->m_JacobianImages[i]->CopyInformation( this->m_JacobianImages[0] );
    this->m_JacobianImages[i]->SetRegions(
      this->m_JacobianImages[0]->GetLargestPossibleRegion() );
    }
}

template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetFixedParametersGridSizeFromTransformDomainInformation() const
{
  // Set the grid size parameters

  for( unsigned int i = 0; i < NDimensions; i++ )
    {
    this->m_FixedParameters[i] = static_cast<ParametersValueType>(
      this->m_TransformDomainMeshSize[i] + SplineOrder );
    }
}


template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetFixedParametersGridOriginFromTransformDomainInformation() const
{
  // Set the origin parameters

  typedef typename ImageType::PointType PointType;
  PointType origin;
  origin.Fill( 0.0 );

  for( unsigned int i = 0; i < NDimensions; i++ )
    {
    ScalarType gridSpacing = this->m_TransformDomainPhysicalDimensions[i] /
      static_cast<ScalarType>( this->m_TransformDomainMeshSize[i] );

    origin[i] = -0.5 * gridSpacing * ( SplineOrder - 1 );
    }

  origin = this->m_TransformDomainDirection * origin;

  for( unsigned int i = 0; i < NDimensions; i++ )
    {
    this->m_FixedParameters[NDimensions + i] = static_cast<ParametersValueType>(
      origin[i] + this->m_TransformDomainOrigin[i] );
    }
}

template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetFixedParametersGridSpacingFromTransformDomainInformation() const
{
  // Set the spacing parameters

  for( unsigned int i = 0; i < NDimensions; i++ )
    {
    ScalarType gridSpacing = this->m_TransformDomainPhysicalDimensions[i] /
      static_cast<ScalarType>( this->m_TransformDomainMeshSize[i] );

    this->m_FixedParameters[2 * NDimensions + i] =
      static_cast<ParametersValueType>( gridSpacing );
    }
}

template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetFixedParametersGridDirectionFromTransformDomainInformation() const
{
  /** Set the direction parameters */

  for( unsigned int di = 0; di < NDimensions; di++ )
    {
    for( unsigned int dj = 0; dj < NDimensions; dj++ )
      {
      this->m_FixedParameters[3 * NDimensions + ( di * NDimensions + dj )] =
        static_cast<ParametersValueType>(
        this->m_TransformDomainDirection[di][dj] );
      }
    }
}

template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetFixedParametersFromTransformDomainInformation() const
{
  this->m_FixedParameters.SetSize ( NDimensions * ( NDimensions + 3 ) );

  //  Fixed Parameters store the following information:
  //  Grid Size
  //  Grid Origin
  //  Grid Spacing
  //  Grid Direction
  //  The size of each of these is equal to NDimensions

  this->SetFixedParametersGridSizeFromTransformDomainInformation();
  this->SetFixedParametersGridOriginFromTransformDomainInformation();
  this->SetFixedParametersGridSpacingFromTransformDomainInformation();
  this->SetFixedParametersGridDirectionFromTransformDomainInformation();

  this->Modified();
}

// Set the Fixed Parameters
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetFixedParameters( const ParametersType &passedParameters )
{
  // check if the number of passedParameters match the
  // expected number of this->m_FixedParameters
  if( passedParameters.Size() == this->m_FixedParameters.Size() )
    {
    for( unsigned int i = 0; i < NDimensions * ( 3 + NDimensions ); ++i )
      {
      this->m_FixedParameters[i] = passedParameters[i];
      }
    }
  else
    {
    itkExceptionMacro( "Mismatched between parameters size "
      << passedParameters.size()
      << " and the required number of fixed parameters "
      << this->m_FixedParameters.Size() );
    }
  this->SetCoefficientImageInformationFromFixedParameters();
}

// Wrap flat parameters as images
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::WrapAsImages()
{
  /**
   * Wrap flat parameters array into SpaceDimension number of ITK images
   * NOTE: For efficiency, parameters are not copied locally. The parameters
   * are assumed to be maintained by the caller.
   */

  PixelType *dataPointer = const_cast<PixelType *>(
    this->m_InputParametersPointer->data_block() );
  unsigned int numberOfPixels = this->GetNumberOfParametersPerDimension();

  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    this->m_CoefficientImages[j]->GetPixelContainer()->
      SetImportPointer( dataPointer + j * numberOfPixels, numberOfPixels );
    }

  /**
   * Allocate memory for Jacobian and wrap into SpaceDimension number
   * of ITK images
   */

  this->m_Jacobian.set_size( SpaceDimension, this->GetNumberOfParameters() );
  this->m_Jacobian.Fill(NumericTraits< JacobianPixelType >::Zero);
  JacobianPixelType *jacobianDataPointer = this->m_Jacobian.data_block();

  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    this->m_JacobianImages[j]->GetPixelContainer()->
      SetImportPointer( jacobianDataPointer + j * (
      this->GetNumberOfParameters() + numberOfPixels ), numberOfPixels );
    }

}


// Get the parameters
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
const
typename BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::ParametersType &
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::GetParameters() const
{
  /** NOTE: For efficiency, this class does not keep a copy of the parameters -
   * it just keeps pointer to input parameters.
   */
  if( this->m_InputParametersPointer == NULL )
    {
    itkExceptionMacro(
      "Cannot GetParameters() because this->m_InputParametersPointer is NULL." );
    }
  return ( *( this->m_InputParametersPointer ) );
}

// Get the parameters
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
const
typename BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::ParametersType &
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::GetFixedParameters() const
{
  //HACK:  This should not be necessary if the
  //       class is kept in a consistent state
  //  this->SetFixedParametersFromCoefficientImageInformation();
  return ( this->m_FixedParameters );
}

// Set the B-Spline coefficients using input images
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::SetCoefficientImages( const CoefficientImageArray &images )
{
  bool validArrayOfImages = true;
  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    validArrayOfImages &= ( images[0].IsNotNull() );
    }

  if( validArrayOfImages )
    {
    typedef typename ImageType::PointType PointType;
    PointType origin;
    origin.Fill( 0.0 );

    for( unsigned int i = 0; i < SpaceDimension; i++ )
      {
      this->m_TransformDomainMeshSize[i] =
        images[0]->GetLargestPossibleRegion().GetSize()[i] - SplineOrder;

      this->m_TransformDomainPhysicalDimensions[i] = static_cast<ScalarType>(
        this->m_TransformDomainMeshSize[i] ) * images[0]->GetSpacing()[i];

      origin[i] += ( images[0]->GetSpacing()[i] * 0.5 * ( SplineOrder - 1 ) );
      }

    origin = this->m_TransformDomainDirection * origin;

    const size_t totalParameters=this->GetNumberOfParameters();
    this->m_InternalParametersBuffer.SetSize( totalParameters );

    for( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      this->m_TransformDomainOrigin[j] = images[0]->GetOrigin()[j] + origin[j];

      const size_t numberOfPixels =
        images[j]->GetLargestPossibleRegion().GetNumberOfPixels();
      if( numberOfPixels * SpaceDimension != totalParameters )
        {
        itkExceptionMacro( "SetCoefficientImage() has array of images that are "
          << "not the correct size. "
          << numberOfPixels * SpaceDimension << " != " << totalParameters
          << " for image at index " << j << "  \n" << images[j]
          );
        }
      const ParametersValueType * const baseImagePointer =
        images[j]->GetBufferPointer();

      ParametersValueType *dataPointer =
        this->m_InternalParametersBuffer.data_block();

      memcpy( dataPointer + j * numberOfPixels,
        baseImagePointer, sizeof( ParametersValueType ) * numberOfPixels );

      this->m_CoefficientImages[j]->CopyInformation( images[j] );
      this->m_CoefficientImages[j]->SetRegions(
        images[j]->GetLargestPossibleRegion() );
      }

    this->SetParameters( this->m_InternalParametersBuffer );
    }
  else
    {
    itkExceptionMacro("SetCoefficientImage() requires that an array of "
      << "correctly sized images be supplied.");
    }
}

// Print self
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::PrintSelf( std::ostream &os, Indent indent ) const
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "TransformDomainOrigin: "
    << this->m_TransformDomainOrigin << std::endl;
  os << indent << "TransformDomainPhysicalDimensions: "
    << this->m_TransformDomainPhysicalDimensions << std::endl;
  os << indent << "TransformDomainDirection: "
    << this->m_TransformDomainDirection << std::endl;

  os << indent << "GridSize: "
    << this->m_CoefficientImages[0]->GetLargestPossibleRegion().GetSize()
    << std::endl;
  os << indent << "GridOrigin: "
    << this->m_CoefficientImages[0]->GetOrigin() << std::endl;
  os << indent << "GridSpacing: "
    << this->m_CoefficientImages[0]->GetSpacing() << std::endl;
  os << indent << "GridDirection: "
    << this->m_CoefficientImages[0]->GetDirection() << std::endl;

  os << indent << "CoefficientImage: [ ";
  for( unsigned int j = 0; j < SpaceDimension - 1; j++ )
    {
    os << this->m_CoefficientImages[j].GetPointer() << ", ";
    }
  os << this->m_CoefficientImages[SpaceDimension - 1].GetPointer()
    << " ]" << std::endl;

  os << indent << "InputParametersPointer: "
     << this->m_InputParametersPointer << std::endl;

}

template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
bool
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::InsideValidRegion( ContinuousIndexType &index ) const
{
  bool inside = true;

  SizeType gridSize =
    this->m_CoefficientImages[0]->GetLargestPossibleRegion().GetSize();

  ScalarType minLimit = 0.5 * static_cast<ScalarType>( SplineOrder - 1 );

  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    ScalarType maxLimit = static_cast<ScalarType>( gridSize[j] ) - 0.5 *
      static_cast<ScalarType>( SplineOrder - 1 ) - 1.0;
    if( index[j] == maxLimit  )
      {
      index[j] -= 1e-6;
      }
    else if( index[j] >= maxLimit )
      {
      inside = false;
      break;
      }
    else if( index[j] < minLimit )
      {
      inside = false;
      break;
      }
    }

  return inside;
}

template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::TransformPoint( const InputPointType &point, OutputPointType &outputPoint,
  WeightsType &weights, ParameterIndexArrayType &indices, bool &inside ) const
{
  inside = true;

  if( this->m_CoefficientImages[0]->GetBufferPointer() )
    {
    ContinuousIndexType index;
    this->m_CoefficientImages[0]->
      TransformPhysicalPointToContinuousIndex( point, index );

    // NOTE: if the support region does not lie totally within the grid
    // we assume zero displacement and return the input point
    inside = this->InsideValidRegion( index );
    if( !inside )
      {
      outputPoint = point;
      return;
      }

    IndexType    supportIndex;
    // Compute interpolation weights
    this->m_WeightsFunction->Evaluate( index, weights, supportIndex );

    // For each dimension, correlate coefficient with weights
    RegionType supportRegion;
    SizeType supportSize;
    supportSize.Fill( SplineOrder + 1 );
    supportRegion.SetSize(supportSize);
    supportRegion.SetIndex(supportIndex);

    outputPoint.Fill( NumericTraits<ScalarType>::Zero );

    typedef ImageRegionConstIterator<ImageType> IteratorType;
    IteratorType coeffIterator[SpaceDimension];
    unsigned long counter = 0;
    const ParametersValueType *basePointer =
      this->m_CoefficientImages[0]->GetBufferPointer();

    for( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      coeffIterator[j] =
        IteratorType( this->m_CoefficientImages[j], supportRegion );
      }

    while( !coeffIterator[0].IsAtEnd() )
      {
      // multiply weigth with coefficient
      for( unsigned int j = 0; j < SpaceDimension; j++ )
        {
        outputPoint[j] += static_cast<ScalarType>(
          weights[counter] * coeffIterator[j].Get() );
        }

      // populate the indices array
      indices[counter] = &( coeffIterator[0].Value() ) - basePointer;

      // go to next coefficient in the support region
      ++counter;
      for( unsigned int j = 0; j < SpaceDimension; j++ )
        {
        ++( coeffIterator[j] );
        }
      }

    // return results
    for( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      outputPoint[j] += point[j];
      }
    }
  else
    {
    itkWarningMacro(<< "B-spline coefficients have not been set");
    for( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      outputPoint[j] = point[j];
      }
    }
}

// Transform a point
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
typename BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::OutputPointType
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::TransformPoint(const InputPointType &point) const
{
  WeightsType weights( this->m_WeightsFunction->GetNumberOfWeights() );
  ParameterIndexArrayType indices(
    this->m_WeightsFunction->GetNumberOfWeights() );
  OutputPointType outputPoint;
  bool inside;

  this->TransformPoint( point, outputPoint, weights, indices, inside );

  return outputPoint;
}

// Compute the Jacobian in one position
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
const
typename BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::JacobianType &
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::GetJacobian( const InputPointType &point ) const
{
  // Zero all components of jacobian
  // NOTE: for efficiency, we only need to zero out the coefficients
  // that got fill last time this method was called.
  typedef ImageRegionIterator< JacobianImageType > IteratorType;
  IteratorType jacobianIterators[SpaceDimension];
  RegionType supportRegion;
  SizeType supportSize;
  supportSize.Fill( SplineOrder + 1 );

  supportRegion.SetSize( supportSize );
    {
    supportRegion.SetIndex( this->m_LastJacobianIndex );

    //Make an array of jacobian image iterators
    for( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      jacobianIterators[j] = IteratorType(
        this->m_JacobianImages[j], supportRegion );
      while( !jacobianIterators[j].IsAtEnd() )
        {
        // zero out jacobian elements
        jacobianIterators[j].Set( NumericTraits<JacobianPixelType>::Zero );
        ++( jacobianIterators[j] );
        }
      }
    }

  ContinuousIndexType index;
  this->m_CoefficientImages[0]->
    TransformPhysicalPointToContinuousIndex( point, index );

  // NOTE: if the support region does not lie totally within the grid we assume
  // zero displacement and do no computations beyond zeroing out the value
  // return the input point
  if( !this->InsideValidRegion( index ) )
    {
    return this->m_Jacobian;
    }

  // Compute interpolation weights
  WeightsType weights( this->m_WeightsFunction->GetNumberOfWeights() );

    {
    IndexType   supportIndex;
    this->m_WeightsFunction->Evaluate( index, weights, supportIndex );
    this->m_LastJacobianIndex = supportIndex;
    // For each dimension, copy the weight to the support region
    supportRegion.SetIndex(supportIndex);

    //Reset iterators now that the support region has changed
    for( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      jacobianIterators[j] = IteratorType( this->m_JacobianImages[j],
        supportRegion );
      }
    }

  unsigned long counter = 0;
  while( !jacobianIterators[0].IsAtEnd() )
    {
    // copy weight to jacobian image
    for( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      jacobianIterators[j].Set( static_cast<JacobianPixelType>(
        weights[counter] ) );
      }
    // go to next coefficient in the support region
    ++counter;
    for( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      ++( jacobianIterators[j] );
      }
    }

  // Return the results
  return this->m_Jacobian;
}

// Compute the Jacobian in one position
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions, VSplineOrder>
::GetJacobian( const InputPointType &point, WeightsType &weights,
  ParameterIndexArrayType &indexes ) const
{
  ContinuousIndexType index;
  this->m_CoefficientImages[0]->
    TransformPhysicalPointToContinuousIndex( point, index );

  // NOTE: if the support region does not lie totally within the grid
  // we assume zero displacement and return the input point
  if( !this->InsideValidRegion( index ) )
    {
    weights.Fill( 0.0 );
    indexes.Fill( 0 );
    return;
    }

  // Compute interpolation weights
  IndexType supportIndex;
  this->m_WeightsFunction->Evaluate(index, weights, supportIndex);

  // For each dimension, copy the weight to the support region
  RegionType supportRegion;
  SizeType supportSize;
  supportSize.Fill( SplineOrder + 1 );
  supportRegion.SetSize( supportSize );
  supportRegion.SetIndex( supportIndex );
  unsigned long counter = 0;

  typedef ImageRegionIterator< JacobianImageType > IteratorType;

  IteratorType coeffIterator = IteratorType(
    this->m_CoefficientImages[0], supportRegion );
  const ParametersValueType *basePointer =
    this->m_CoefficientImages[0]->GetBufferPointer();
  while( !coeffIterator.IsAtEnd() )
    {
    indexes[counter] = &( coeffIterator.Value() ) - basePointer;

    // go to next coefficient in the support region
    ++counter;
    ++coeffIterator;
    }
}

template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
unsigned int
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::GetNumberOfAffectedWeights() const
{
  return this->m_WeightsFunction->GetNumberOfWeights();
}
} // namespace

#endif
