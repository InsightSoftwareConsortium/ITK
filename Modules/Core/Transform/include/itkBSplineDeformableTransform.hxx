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
#include "itkIdentityTransform.h"

namespace itk
{

// This helper class is used to work around a race condition where the dynamically
// generated images must exist before the references to the sub-sections are created.
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
typename BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >::CoefficientImageArray
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::ArrayOfImagePointerGeneratorHelper(void) const
{
  CoefficientImageArray tempArrayOfPointers;
  for ( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    tempArrayOfPointers[j] = ImageType::New();
    }
  return tempArrayOfPointers;
}

// Constructor with default arguments
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::BSplineDeformableTransform():Superclass(SpaceDimension, 0),
  m_CoefficientImage(this->ArrayOfImagePointerGeneratorHelper()),
  m_GridRegion(this->m_CoefficientImage[0]->GetLargestPossibleRegion()),
  m_GridOrigin(this->m_CoefficientImage[0]->GetOrigin()),
  m_GridSpacing(this->m_CoefficientImage[0]->GetSpacing()),
  m_GridDirection(this->m_CoefficientImage[0]->GetDirection())
{
  // Instantiate a weights function
  this->m_WeightsFunction = WeightsFunctionType::New();
  this->m_SupportSize = this->m_WeightsFunction->GetSupportSize();

  // Instantiate an identity transform
  typedef IdentityTransform< ScalarType, SpaceDimension > IdentityTransformType;
  typename IdentityTransformType::Pointer id = IdentityTransformType::New();
  this->m_BulkTransform = id;

  this->m_InternalParametersBuffer = ParametersType(0);
  // Make sure the parameters pointer is not NULL after construction.
  this->m_InputParametersPointer = &(this->m_InternalParametersBuffer);

  // Setup variables for computing interpolation
  this->m_Offset = SplineOrder / 2;
  if ( SplineOrder % 2 )
    {
    this->m_SplineOrderOdd = true;
    }
  else
    {
    this->m_SplineOrderOdd = false;
    }
  this->m_ValidRegion = this->m_GridRegion; //HACK:  Perhaps this->m_ValidRegion is redundant also.

  /** Fixed Parameters store the following information:
   *     Grid Size
   *     Grid Origin
   *     Grid Spacing
   *     Grid Direction
   *  The size of these is equal to the  NInputDimensions
   */
  //ForExmplae 3D image has FixedParameters of:
  //[size[0],size[1],size[2],
  //origin[0],origin[1],origin[2],
  //spacing[0],spacing[1],spacing[2],
  //dir[0][0],dir[1][0],dir[2][0],
  //dir[0][1],dir[1][1],dir[2][1],
  //dir[0][2],dir[1][2],dir[2][2]]

  this->SetFixedParametersFromCoefficientImageInformation();

  // Initialize jacobian images
  for ( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    this->m_JacobianImage[j]=JacobianImageType::New();
    this->m_JacobianImage[j]->SetRegions(this->m_GridRegion);
    this->m_JacobianImage[j]->SetOrigin( this->m_GridOrigin );
    this->m_JacobianImage[j]->SetSpacing( this->m_GridSpacing );
    this->m_JacobianImage[j]->SetDirection(this->m_GridDirection);
    }
  this->m_LastJacobianIndex = this->m_GridRegion.GetIndex();
  this->UpdateValidGridRegion();
}

// Destructor
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::~BSplineDeformableTransform()
{}

// Explicit New() method, used here because we need to split the itkNewMacro()
// in order to overload the CreateAnother() method.
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
typename BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >::Pointer
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::New(void)
{
  Pointer smartPtr = ::itk::ObjectFactory< Self >::Create();

  if ( smartPtr.IsNull() )
    {
    smartPtr = static_cast< Pointer >( new Self );
    }
  smartPtr->UnRegister();
  return smartPtr;
}

// Explicit New() method, used here because we need to split the itkNewMacro()
// in order to overload the CreateAnother() method.
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
::itk::LightObject::Pointer
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::CreateAnother(void) const
{
  ::itk::LightObject::Pointer smartPtr;
  Pointer copyPtr = Self::New().GetPointer();

  copyPtr->m_BulkTransform =  this->GetBulkTransform();

  smartPtr = static_cast< Pointer >( copyPtr );

  return smartPtr;
}

// Get the number of parameters
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
unsigned int
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::GetNumberOfParameters(void) const
{
  // The number of parameters equal SpaceDimension * number of
  // of pixels in the grid region.
  return ( static_cast< unsigned int >( SpaceDimension )
           * static_cast< unsigned int >( this->m_GridRegion.GetNumberOfPixels() ) );
}

// Get the number of parameters per dimension
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
unsigned int
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::GetNumberOfParametersPerDimension(void) const
{
  // The number of parameters per dimension equal number of
  // of pixels in the grid region.
  return ( static_cast< unsigned int >( this->m_GridRegion.GetNumberOfPixels() ) );
}

// Set the grid region
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::UpdateValidGridRegion()
{
    // Set the valid region
    // If the grid spans the interval [start, last].
    // The valid interval for evaluation is [start+offset, last-offset]
    // when spline order is even.
    // The valid interval for evaluation is [start+offset, last-offset)
    // when spline order is odd.
    // Where offset = floor(spline / 2 ).
    // Note that the last pixel is not included in the valid region
    // with odd spline orders.
    typename RegionType::SizeType size = this->m_GridRegion.GetSize();
    typename RegionType::IndexType index = this->m_GridRegion.GetIndex();
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      index[j] += static_cast< typename RegionType::IndexValueType >( this->m_Offset );
      size[j] -= static_cast< typename RegionType::SizeValueType >( 2 * this->m_Offset );
      this->m_ValidRegionFirst[j] = index[j];
      this->m_ValidRegionLast[j] = index[j] + static_cast< typename RegionType::IndexValueType >( size[j] ) - 1;
      }
    this->m_ValidRegion.SetSize(size);
    this->m_ValidRegion.SetIndex(index);
}

// Set the grid region
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetGridRegion(const RegionType & region)
{
  if ( this->m_GridRegion != region )
    {
    this->m_CoefficientImage[0]->SetRegions(region);
    // set regions for each coefficient image
    for ( unsigned int j = 1; j < SpaceDimension; j++ )
      {
      this->m_CoefficientImage[j]->SetRegions(region);
      }
    // set regions for each jacobian image
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      this->m_JacobianImage[j]->SetRegions(region);
      }

    this->UpdateValidGridRegion();
    //
    // If we are using the default parameters, update their size and set to
    // identity.
    //
    // Input parameters point to internal buffer => using default parameters.
    if ( this->m_InputParametersPointer == &(this->m_InternalParametersBuffer) )
      {
      // Check if we need to resize the default parameter buffer.
      if ( this->m_InternalParametersBuffer.GetSize() != this->GetNumberOfParameters() )
        {
        this->m_InternalParametersBuffer.SetSize( this->GetNumberOfParameters() );
        // Fill with zeros for identity.
        this->m_InternalParametersBuffer.Fill(0);
        }
      }
    this->SetFixedParametersRegionFromCoefficientImageInformation();
    this->Modified();
    }
}

// Set the grid spacing
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetGridSpacing(const SpacingType & spacing)
{
  if ( this->m_GridSpacing != spacing )
    {
    this->m_CoefficientImage[0]->SetSpacing(spacing);
    // set spacing for each coefficient image
    for ( unsigned int j = 1; j < SpaceDimension; j++ )
      {
      this->m_CoefficientImage[j]->SetSpacing( spacing );
      }
    // set spacing for each jacobian image
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      this->m_JacobianImage[j]->SetSpacing( spacing );
      }
    this->SetFixedParametersSpacingFromCoefficientImageInformation();
    this->Modified();
    }
}

// Set the grid direction
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetGridDirection(const DirectionType & direction)
{
  if ( this->m_GridDirection != direction )
    {
    this->m_CoefficientImage[0]->SetDirection(direction);

    // set direction for each coefficient image
    for ( unsigned int j = 1; j < SpaceDimension; j++ )
      {
      this->m_CoefficientImage[j]->SetDirection(direction);
      }
    // set direction for each jacobian image
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      this->m_JacobianImage[j]->SetDirection(direction);
      }
    this->SetFixedParametersDirectionFromCoefficientImageInformation();
    this->Modified();
    }
}

// Set the grid origin
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetGridOrigin(const OriginType & origin)
{
  if ( this->m_GridOrigin != origin )
    {
    this->m_CoefficientImage[0]->SetOrigin( origin );

    // set spacing for each coefficient image
    for ( unsigned int j = 1; j < SpaceDimension; j++ )
      {
      this->m_CoefficientImage[j]->SetOrigin( origin );
      }
    // set spacing for each jacobian image
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      this->m_JacobianImage[j]->SetOrigin( origin );
      }
    this->SetFixedParametersOriginFromCoefficientImageInformation();
    this->Modified();
    }
}

// Set the parameters
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetIdentity()
{
  if ( this->m_InputParametersPointer == &(this->m_InternalParametersBuffer) )
    {
    //If this->m_InternalParametersBuffer is the this->m_InputParametersPointer
    this->m_InternalParametersBuffer.Fill(0.0);
    }
  else
    {
    //Should not be allowed to modify a const parameter set, so
    //make an internal representation that is an identity mapping
    this->m_InternalParametersBuffer.SetSize(this->GetNumberOfParameters());
    this->m_InternalParametersBuffer.Fill(0.0);
    }
  this->SetParameters(this->m_InternalParametersBuffer);
  this->Modified();
}

// Set the parameters
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetParameters(const ParametersType & parameters)
{
  // check if the number of parameters match the
  // expected number of parameters
  if ( parameters.Size() != this->GetNumberOfParameters() )
    {
    itkExceptionMacro( << "Mismatch between parameters size "
                       << parameters.Size()
                       << " and expected number of parameters "
                       << this->GetNumberOfParameters()
                       << ( this->m_GridRegion.GetNumberOfPixels() == 0 ?
                            ". \nSince the size of the grid region is 0, perhaps you forgot to SetGridRegion or SetFixedParameters before setting the Parameters."
                            : "" ) );
    }

  if ( &parameters != &(this->m_InternalParametersBuffer) )
    {
    // Clean up this->m_InternalParametersBuffer becasue we will
    // use an externally supplied set of parameters as the buffer
    this->m_InternalParametersBuffer = ParametersType(0);
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
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetParametersByValue(const ParametersType & parameters)
{
  // check if the number of parameters match the
  // expected number of parameters
  if ( parameters.Size() != this->GetNumberOfParameters() )
    {
    itkExceptionMacro( << "Mismatched between parameters size "
                       << parameters.size()
                       << " and region size "
                       << this->m_GridRegion.GetNumberOfPixels() );
    }

  // copy parameters to this->m_InternalParametersBuffer
  this->m_InternalParametersBuffer = parameters;
  this->SetParameters(this->m_InternalParametersBuffer);
}

template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetCoefficientImageInformationFromFixedParameters()
{
  /*********************************************************
    Fixed Parameters store the following information:
    Grid Size
    Grid Origin
    Grid Spacing
    Grid Direction
    The size of these is equal to the  NInputDimensions
   *********************************************************/
    {
    /** Set the Grid Parameters */
    SizeType gridSize;
    for ( unsigned int i = 0; i < NDimensions; i++ )
      {
      gridSize[i] = static_cast< int >( this->m_FixedParameters[i] );
      }
    RegionType bsplineRegion;
    bsplineRegion.SetSize(gridSize);
    this->SetGridRegion(bsplineRegion);
    }

    {
    /** Set the Origin Parameters */
    OriginType origin;
    for ( unsigned int i = 0; i < NDimensions; i++ )
      {
      origin[i] = this->m_FixedParameters[NDimensions + i];
      }
    this->SetGridOrigin(origin);
    }

    {
    /** Set the Spacing Parameters */
    SpacingType spacing;
    for ( unsigned int i = 0; i < NDimensions; i++ )
      {
      spacing[i] = this->m_FixedParameters[2 * NDimensions + i];
      }
    this->SetGridSpacing(spacing);
    }

    {
    /** Set the Direction Parameters */
    DirectionType direction;
    for ( unsigned int di = 0; di < NDimensions; di++ )
      {
      for ( unsigned int dj = 0; dj < NDimensions; dj++ )
        {
        direction[di][dj] = this->m_FixedParameters[3 * NDimensions + ( di * NDimensions + dj )];
        }
      }
    this->SetGridDirection(direction);
    }
}


template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetFixedParametersRegionFromCoefficientImageInformation() const
{
  /** Set the Grid Parameters */
  const SizeType &gridSize=this->m_CoefficientImage[0]->GetLargestPossibleRegion().GetSize();
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    this->m_FixedParameters[i] = static_cast<ParametersValueType>(gridSize[i]);
    }
}


template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetFixedParametersOriginFromCoefficientImageInformation() const
{
  /** Set the Origin Parameters */
  const OriginType &origin=this->m_CoefficientImage[0]->GetOrigin();
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    this->m_FixedParameters[NDimensions + i]=static_cast<ParametersValueType>(origin[i]);
    }
}

template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetFixedParametersSpacingFromCoefficientImageInformation() const
{
  /** Set the Spacing Parameters */
  const SpacingType &spacing=this->m_CoefficientImage[0]->GetSpacing();
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    this->m_FixedParameters[2 * NDimensions + i]=static_cast<ParametersValueType>(spacing[i]);
    }
}

template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetFixedParametersDirectionFromCoefficientImageInformation() const
{
  /** Set the Direction Parameters */
  const DirectionType &direction=this->m_CoefficientImage[0]->GetDirection();
  for ( unsigned int di = 0; di < NDimensions; di++ )
    {
    for ( unsigned int dj = 0; dj < NDimensions; dj++ )
      {
      this->m_FixedParameters[3 * NDimensions + ( di * NDimensions + dj )]=static_cast<ParametersValueType>(direction[di][dj]);
      }
    }
}

template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetFixedParametersFromCoefficientImageInformation() const
{
  this->m_FixedParameters.SetSize ( NDimensions * ( NDimensions + 3 ) );
  /*********************************************************
    Fixed Parameters store the following information:
    Grid Size
    Grid Origin
    Grid Spacing
    Grid Direction
    The size of these is equal to the  NInputDimensions
   *********************************************************/
  this->SetFixedParametersRegionFromCoefficientImageInformation();
  this->SetFixedParametersOriginFromCoefficientImageInformation();
  this->SetFixedParametersSpacingFromCoefficientImageInformation();
  this->SetFixedParametersDirectionFromCoefficientImageInformation();
  this->Modified();
}

// Set the Fixed Parameters
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetFixedParameters(const ParametersType & passedParameters)
{
    // check if the number of passedParameters match the
    // expected number of this->m_FixedParameters
    if ( passedParameters.Size() == this->m_FixedParameters.Size() )
      {
      for ( unsigned int i = 0; i < NDimensions * ( 3 + NDimensions ); i++ )
        {
        this->m_FixedParameters[i] = passedParameters[i];
        }
      }
    else if ( passedParameters.Size() == NDimensions * 3 )
      {
      //This option was originally valid for backwards compatibilty
      //with BSplines saved to disk from before image orientation was used.
      //Those transforms would no longer be valid with respect to images
      //with explicit directions.
      itkExceptionMacro( << "Mismatched between parameters size "
        << passedParameters.size()
        << " and required number of fixed parameters "
        << this->m_FixedParameters.Size() <<
        ".  Implicit setting of identity direction is no longer supported." );
      }
    else
      {
      itkExceptionMacro( << "Mismatched between parameters size "
        << passedParameters.size()
        << " and the required number of fixed parameters "
        << this->m_FixedParameters.Size() );
      }
    this->SetCoefficientImageInformationFromFixedParameters();
}

// Wrap flat parameters as images
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::WrapAsImages()
{
  /**
   * Wrap flat parameters array into SpaceDimension number of ITK images
   * NOTE: For efficiency, parameters are not copied locally. The parameters
   * are assumed to be maintained by the caller.
   */
  ParametersValueType *dataPointer =
    const_cast< ParametersValueType * >( ( this->m_InputParametersPointer->data_block() ) );
  unsigned int numberOfPixels = this->m_GridRegion.GetNumberOfPixels();

  for ( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    this->m_CoefficientImage[j]->GetPixelContainer()->
    SetImportPointer(dataPointer, numberOfPixels);
    dataPointer += numberOfPixels;
    }

  /**
   * Allocate memory for Jacobian and wrap into SpaceDimension number
   * of ITK images
   */
  this->m_Jacobian.set_size( SpaceDimension, this->GetNumberOfParameters() );
  this->m_Jacobian.Fill(NumericTraits< JacobianPixelType >::Zero);
  this->m_LastJacobianIndex = this->m_ValidRegion.GetIndex();
  JacobianPixelType *jacobianDataPointer = this->m_Jacobian.data_block();

  for ( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    this->m_JacobianImage[j]->GetPixelContainer()->
    SetImportPointer(jacobianDataPointer, numberOfPixels);
    jacobianDataPointer += this->GetNumberOfParameters() + numberOfPixels;
    }
}


// Get the parameters
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
const
typename BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::ParametersType &
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::GetParameters(void) const
{
  /** NOTE: For efficiency, this class does not keep a copy of the parameters -
   * it just keeps pointer to input parameters.
   */
  if ( this->m_InputParametersPointer == NULL )
    {
    itkExceptionMacro(
      <<
      "Cannot GetParameters() because this->m_InputParametersPointer is NULL." );
    }
  return ( *(this->m_InputParametersPointer) );
}

// Get the parameters
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
const
typename BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::ParametersType &
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::GetFixedParameters(void) const
{
  //HACK:  This should not be necessary if the
  //       class is kept in a consistent state
  //  this->SetFixedParametersFromCoefficientImageInformation();
  return ( this->m_FixedParameters );
}

// Set the B-Spline coefficients using input images
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::SetCoefficientImage(const CoefficientImageArray & images)
{

  bool validArrayOfImage=true;
  for ( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    validArrayOfImage &= (images[0].IsNotNull());
    }

  if ( validArrayOfImage )
    {
    //The BufferedRegion MUST equal the LargestPossibleRegion.
    this->SetGridRegion( images[0]->GetLargestPossibleRegion() );
    this->SetGridOrigin( images[0]->GetOrigin() );
    this->SetGridSpacing( images[0]->GetSpacing() );
    this->SetGridDirection( images[0]->GetDirection() );

    const SizeValueType totalParameters=this->GetNumberOfParameters();
    this->m_InternalParametersBuffer.SetSize(totalParameters);

    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      const SizeValueType numberOfPixels = images[j]->GetLargestPossibleRegion().GetNumberOfPixels();
      if(numberOfPixels*SpaceDimension != totalParameters)
        {
        itkExceptionMacro( <<
          "SetCoefficientImage() has array of images that are not the correct size. "
          << numberOfPixels * SpaceDimension << " != " << totalParameters << " for image at index "
          << j << "  \n" << images[j]
          );
        }
    const ParametersValueType * const baseImagePointer = images[j]->GetBufferPointer();
    ParametersValueType *dataPointer = this->m_InternalParametersBuffer.data_block();
      ::memcpy(dataPointer,
        baseImagePointer,
        sizeof(ParametersValueType)*numberOfPixels);
      }
    this->SetParameters(this->m_InternalParametersBuffer);
    }
  else
    {
    itkExceptionMacro( <<
      "SetCoefficientImage() requires that an array of correctly sized images be supplied.");
    }
}

// Print self
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "GridRegion: " << this->m_GridRegion << std::endl;
  os << indent << "GridOrigin: " << this->m_GridOrigin << std::endl;
  os << indent << "GridSpacing: " << this->m_GridSpacing << std::endl;
  os << indent << "GridDirection: " << this->m_GridDirection << std::endl;

  os << indent << "CoefficientImage: [ ";
  for ( unsigned int j = 0; j < SpaceDimension - 1; j++ )
    {
    os << this->m_CoefficientImage[j].GetPointer() << ", ";
    }
  os << this->m_CoefficientImage[SpaceDimension - 1].GetPointer() << " ]" << std::endl;

  os << indent << "InputParametersPointer: "
     << this->m_InputParametersPointer << std::endl;
  os << indent << "ValidRegion: " << this->m_ValidRegion << std::endl;
  os << indent << "LastJacobianIndex: " << this->m_LastJacobianIndex << std::endl;
  os << indent << "BulkTransform: ";
  os << this->m_BulkTransform.GetPointer() << std::endl;
  os << indent << "WeightsFunction: ";
  os << this->m_WeightsFunction.GetPointer() << std::endl;

  if ( this->m_BulkTransform )
    {
    os << indent << "BulkTransformType: "
       << this->m_BulkTransform->GetNameOfClass() << std::endl;
    }
}

// Transform a point
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
bool
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::InsideValidRegion(
  const ContinuousIndexType & index) const
{
  bool inside = true;

  if ( inside && this->m_SplineOrderOdd )
    {
    typedef typename ContinuousIndexType::ValueType ValueType;
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      if ( index[j] >= static_cast< ValueType >( this->m_ValidRegionLast[j] ) )
        {
        inside = false;
        break;
        }
      if ( index[j] < static_cast< ValueType >( this->m_ValidRegionFirst[j] ) )
        {
        inside = false;
        break;
        }
      }
    }

  return inside;
}

// Transform a point
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::TransformPoint(
  const InputPointType & point,
  OutputPointType & outputPoint,
  WeightsType & weights,
  ParameterIndexArrayType & indices,
  bool & inside) const
{
  inside = true;

  InputPointType transformedPoint;
  if ( this->m_BulkTransform )
    {
    transformedPoint = this->m_BulkTransform->TransformPoint(point);
    }
  else
    {
    transformedPoint = point;
    }

  if ( this->m_CoefficientImage[0] )
    {
    ContinuousIndexType index;
    this->m_CoefficientImage[0]->TransformPhysicalPointToContinuousIndex(point,index);

    // NOTE: if the support region does not lie totally within the grid
    // we assume zero displacement and return the input point
    inside = this->InsideValidRegion(index);
    if ( !inside )
      {
      outputPoint = transformedPoint;
      return;
      }

    IndexType    supportIndex;
    // Compute interpolation weights
    this->m_WeightsFunction->Evaluate(index, weights, supportIndex);

    // For each dimension, correlate coefficient with weights
    RegionType supportRegion;
    supportRegion.SetSize(this->m_SupportSize);
    supportRegion.SetIndex(supportIndex);

    outputPoint.Fill(NumericTraits< ScalarType >::Zero);

    typedef ImageRegionConstIterator< ImageType > IteratorType;
    IteratorType     coeffIterator[SpaceDimension];
    unsigned long    counter = 0;
    const ParametersValueType *basePointer = this->m_CoefficientImage[0]->GetBufferPointer();

    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      coeffIterator[j] = IteratorType(this->m_CoefficientImage[j], supportRegion);
      }

    while ( !coeffIterator[0].IsAtEnd() )
      {
      // multiply weigth with coefficient
      for ( unsigned int j = 0; j < SpaceDimension; j++ )
        {
        outputPoint[j] += static_cast< ScalarType >(
          weights[counter] * coeffIterator[j].Get() );
        }

      // populate the indices array
      indices[counter] = &( coeffIterator[0].Value() ) - basePointer;

      // go to next coefficient in the support region
      ++counter;
      for ( unsigned int j = 0; j < SpaceDimension; j++ )
        {
        ++( coeffIterator[j] );
        }
      }

    // return results
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      outputPoint[j] += transformedPoint[j];
      }
    }
  else
    {
    itkWarningMacro(<< "B-spline coefficients have not been set");
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      outputPoint[j] = transformedPoint[j];
      }
    }
}

// Transform a point
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
typename BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::OutputPointType
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::TransformPoint(const InputPointType & point) const
{
  WeightsType             weights( this->m_WeightsFunction->GetNumberOfWeights() );
  ParameterIndexArrayType indices( this->m_WeightsFunction->GetNumberOfWeights() );
  OutputPointType         outputPoint;
  bool                    inside;

  this->TransformPoint(point, outputPoint, weights, indices, inside);

  return outputPoint;
}

// Compute the Jacobian in one position
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
const
typename BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::JacobianType &
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::GetJacobian(const InputPointType & point) const
{
  // Zero all components of jacobian
  // NOTE: for efficiency, we only need to zero out the coefficients
  // that got fill last time this method was called.
  typedef ImageRegionIterator< JacobianImageType > IteratorType;
  IteratorType jacobianIterators[SpaceDimension];
  RegionType supportRegion;
  supportRegion.SetSize(this->m_SupportSize);
    {
    supportRegion.SetIndex(this->m_LastJacobianIndex);

    //Make an array of jacobian image iterators
    for (unsigned int j = 0; j < SpaceDimension; j++ )
      {
      jacobianIterators[j] = IteratorType(this->m_JacobianImage[j], supportRegion);
      while ( !jacobianIterators[j].IsAtEnd() )
        {
        // zero out jacobian elements
        jacobianIterators[j].Set(NumericTraits< JacobianPixelType >::Zero);
        ++( jacobianIterators[j] );
        }
      }
    }

  ContinuousIndexType index;
  this->m_CoefficientImage[0]->TransformPhysicalPointToContinuousIndex(point,index);

  // NOTE: if the support region does not lie totally within the grid
  // we assume zero displacement and do no computations beyond zeroing out the value
  // return the input point
  if ( !this->InsideValidRegion(index) )
    {
    return this->m_Jacobian;
    }

  // Compute interpolation weights
  WeightsType weights( this->m_WeightsFunction->GetNumberOfWeights() );

    {
    IndexType   supportIndex;
    this->m_WeightsFunction->Evaluate(index, weights, supportIndex);
    this->m_LastJacobianIndex = supportIndex;
    // For each dimension, copy the weight to the support region
    supportRegion.SetIndex(supportIndex);

    //Reset iterators now that the support region has changed
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      jacobianIterators[j] = IteratorType(this->m_JacobianImage[j], supportRegion);
      }
    }

  unsigned long counter = 0;
  while ( !jacobianIterators[0].IsAtEnd() )
    {
    // copy weight to jacobian image
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      jacobianIterators[j].Set( static_cast< JacobianPixelType >( weights[counter] ) );
      }
    // go to next coefficient in the support region
    ++counter;
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      ++( jacobianIterators[j] );
      }
    }

  // Return the results
  return this->m_Jacobian;
}

// Compute the Jacobian in one position
template< class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder >
void
BSplineDeformableTransform< TScalarType, NDimensions, VSplineOrder >
::GetJacobian(const InputPointType & point, WeightsType & weights, ParameterIndexArrayType & indexes) const
{
  ContinuousIndexType index;
  this->m_CoefficientImage[0]->TransformPhysicalPointToContinuousIndex(point,index);

  // NOTE: if the support region does not lie totally within the grid
  // we assume zero displacement and return the input point
  if ( !this->InsideValidRegion(index) )
    {
    weights.Fill(0.0);
    indexes.Fill(0);
    return;
    }

  // Compute interpolation weights
  IndexType supportIndex;
  this->m_WeightsFunction->Evaluate(index, weights, supportIndex);

  // For each dimension, copy the weight to the support region
  RegionType supportRegion;
  supportRegion.SetSize(this->m_SupportSize);
  supportRegion.SetIndex(supportIndex);
  unsigned long counter = 0;

  typedef ImageRegionIterator< JacobianImageType > IteratorType;

  IteratorType coeffIterator = IteratorType(this->m_CoefficientImage[0], supportRegion);
  const ParametersValueType *basePointer = this->m_CoefficientImage[0]->GetBufferPointer();
  while ( !coeffIterator.IsAtEnd() )
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
