/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineDeformableTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBSplineDeformableTransform_txx
#define _itkBSplineDeformableTransform_txx

#include "itkBSplineDeformableTransform.h"
#include "itkContinuousIndex.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkIdentityTransform.h"

namespace itk
{

// Constructor with default arguments
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::BSplineDeformableTransform():Superclass(SpaceDimension,0)
{

  // Instantiate a weights function
  m_WeightsFunction = WeightsFunctionType::New();
  m_SupportSize = m_WeightsFunction->GetSupportSize();

  // Instantiate an identity transform
  typedef IdentityTransform<ScalarType,SpaceDimension> IdentityTransformType;
  typename IdentityTransformType::Pointer id = IdentityTransformType::New();
  m_BulkTransform = id;

  // Default grid size is zero
  typename RegionType::SizeType size;
  typename RegionType::IndexType index;
  size.Fill( 0 );
  index.Fill( 0 );
  m_GridRegion.SetSize( size );
  m_GridRegion.SetIndex( index );

  m_GridOrigin.Fill( 0.0 );  // default origin is all zeros
  m_GridSpacing.Fill( 1.0 ); // default spacing is all ones

  m_InputParametersPointer = NULL;

  // Initialize coeffient images
  for ( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    m_CoefficientImage[j] = ImageType::New();
    m_CoefficientImage[j]->SetRegions( m_GridRegion );
    m_CoefficientImage[j]->SetOrigin( m_GridOrigin.GetDataPointer() );
    m_CoefficientImage[j]->SetSpacing( m_GridSpacing.GetDataPointer() );
    }

  // Setup variables for computing interpolation
  m_Offset = ( SplineOrder + 1 ) / 2;
  m_ValidRegion = m_GridRegion;

  // Initialize jacobian images
  for ( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    m_JacobianImage[j] = ImageType::New();
    m_JacobianImage[j]->SetRegions( m_GridRegion );
    m_JacobianImage[j]->SetOrigin( m_GridOrigin.GetDataPointer() );
    m_JacobianImage[j]->SetSpacing( m_GridSpacing.GetDataPointer() );
    }

  m_LastJacobianIndex = m_ValidRegion.GetIndex();
  
}
    

// Destructor
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::~BSplineDeformableTransform()
{

}


// Get the number of parameters
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
unsigned int
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::GetNumberOfParameters(void) const
{

  // The number of parameters equal SpaceDimension * number of
  // of pixels in the grid region.
  return ( static_cast<unsigned int>( SpaceDimension ) *
    static_cast<unsigned int>( m_GridRegion.GetNumberOfPixels() ) );

}


// Get the number of parameters per dimension
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
unsigned int
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::GetNumberOfParametersPerDimension(void) const
{
  // The number of parameters per dimension equal number of
  // of pixels in the grid region.
  return ( static_cast<unsigned int>( m_GridRegion.GetNumberOfPixels() ) );

}


// Set the grid region
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::SetGridRegion( const RegionType& region )
{
  if ( m_GridRegion != region )
    {

    m_GridRegion = region;

    // set regions for each coefficient and jacobian image
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      m_CoefficientImage[j]->SetRegions( m_GridRegion );
      m_JacobianImage[j]->SetRegions( m_GridRegion );
      }

    // Set the valid region
    typename RegionType::SizeType size = m_GridRegion.GetSize();
    typename RegionType::IndexType index = m_GridRegion.GetIndex();
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      index[j] += 
        static_cast< typename RegionType::IndexValueType >( m_Offset );
      size[j] -= 
        static_cast< typename RegionType::SizeValueType> ( 2 * m_Offset );
      }
    m_ValidRegion.SetSize( size );
    m_ValidRegion.SetIndex( index );

    this->Modified();
    }

}


// Set the grid spacing
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::SetGridSpacing( const SpacingType& spacing )
{
  if ( m_GridSpacing != spacing )
    {
    m_GridSpacing = spacing;

    // set spacing for each coefficient and jacobian image
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      m_CoefficientImage[j]->SetSpacing( m_GridSpacing.GetDataPointer() );
      m_JacobianImage[j]->SetSpacing( m_GridSpacing.GetDataPointer() );
      }

    this->Modified();
    }

}


// Set the grid origin
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::SetGridOrigin( const OriginType& origin )
{
  if ( m_GridOrigin != origin )
    {
    m_GridOrigin = origin;

    // set spacing for each coefficient and jacobianimage
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      m_CoefficientImage[j]->SetOrigin( m_GridOrigin.GetDataPointer() );
      m_JacobianImage[j]->SetOrigin( m_GridOrigin.GetDataPointer() );
      }

    this->Modified();
    }

}


// Set the parameters
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::SetParameters( const ParametersType & parameters )
{

  // check if the number of parameters match the
  // expected number of parameters
  if ( parameters.Size() != this->GetNumberOfParameters() )
    {
    itkExceptionMacro(<<"Mismatched between parameters size and region size");
    }

  // Keep a reference to the input parameters
  m_InputParametersPointer = &parameters;

  /**
   * Wrap flat parameters array into SpaceDimension number of ITK images
   * NOTE: For efficiency, parameters are not copied locally. The parameters
   * are assumed to be maintained by the caller.
   */
  PixelType * dataPointer = (PixelType *)( parameters.data_block() );
  unsigned int numberOfPixels = m_GridRegion.GetNumberOfPixels();

  for ( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    m_CoefficientImage[j]->GetPixelContainer()->
      SetImportPointer( dataPointer, numberOfPixels );
    dataPointer += numberOfPixels;
    }

  /**
   * Allocate memory for Jacobian and wrap into SpaceDimension number
   * of ITK images
   */
  m_Jacobian.resize( SpaceDimension, this->GetNumberOfParameters() );
  m_Jacobian.Fill( NumericTraits<JacobianPixelType>::Zero );
  m_LastJacobianIndex = m_ValidRegion.GetIndex();
  JacobianPixelType * jacobianDataPointer = m_Jacobian.data_block();

  for ( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    m_JacobianImage[j]->GetPixelContainer()->
      SetImportPointer( jacobianDataPointer, numberOfPixels );
    jacobianDataPointer += this->GetNumberOfParameters() + numberOfPixels;
    }

}

// Get the parameters
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
const 
typename BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::ParametersType &
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::GetParameters( void ) const
{
  /** NOTE: For efficiency, this class does not keep a copy of the parameters - 
   * it just keeps pointer to input parameters. 
   */
   return (*m_InputParametersPointer);
}


// Print self
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::PrintSelf(std::ostream &os, Indent indent) const
{

  this->Superclass::PrintSelf(os,indent);

  os << indent << "GridRegion: " << m_GridRegion << std::endl;
  os << indent << "GridOrigin: " << m_GridOrigin << std::endl;
  os << indent << "GridSpacing: " << m_GridSpacing << std::endl;
 
  os << indent << "InputParametersPointer: " 
     << m_InputParametersPointer << std::endl;
  os << indent << "ValidRegion: " << m_ValidRegion << std::endl;
  os << indent << "LastJacobianIndex: " << m_LastJacobianIndex << std::endl;
}


// Transform a point
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void 
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::TransformPoint( 
const InputPointType & point, 
OutputPointType & outputPoint, 
WeightsType & weights, 
ParameterIndexArrayType & indices,
bool& inside ) const
{

  unsigned int j;
  IndexType supportIndex;

  InputPointType transformedPoint;
  if ( m_BulkTransform )
    {
    transformedPoint = m_BulkTransform->TransformPoint( point );
    }
  else
    {
    transformedPoint = point;
    }


  ContinuousIndexType index;
  for ( j = 0; j < SpaceDimension; j++ )
    {
    index[j] = ( point[j] - m_GridOrigin[j] ) / m_GridSpacing[j];
    }

  // NOTE: if the support region does not lie totally within the grid
  // we assume zero displacement and return the input point

  inside = true;
  if ( !m_ValidRegion.IsInside( index ) )
    {
    outputPoint = transformedPoint;
    inside = false;
    return;
    }

  // Compute interpolation weights
  m_WeightsFunction->Evaluate( index, weights, supportIndex );

  // For each dimension, correlate coefficient with weights
  RegionType supportRegion;
  supportRegion.SetSize( m_SupportSize );
  supportRegion.SetIndex( supportIndex );

  outputPoint.Fill( NumericTraits<ScalarType>::Zero );

  typedef ImageRegionConstIterator<ImageType> IteratorType;
  IteratorType m_Iterator[ SpaceDimension ];
  unsigned long counter = 0;
  PixelType * basePointer = m_CoefficientImage[0]->GetBufferPointer();

  for ( j = 0; j < SpaceDimension; j++ )
    {
    m_Iterator[j] = IteratorType( m_CoefficientImage[j], supportRegion );
    }

  while ( ! m_Iterator[0].IsAtEnd() )
    {

    // multiply weigth with coefficient
    for ( j = 0; j < SpaceDimension; j++ )
      {
      outputPoint[j] += static_cast<ScalarType>( 
        weights[counter] * m_Iterator[j].Get());
      }

    // populate the indices array
    indices[counter] = &(m_Iterator[0].Value()) - basePointer;

    // go to next coefficient in the support region
    ++ counter;
    for ( j = 0; j < SpaceDimension; j++ )
      {
      ++( m_Iterator[j] );
      }
    }
  
  // return results
  for ( j = 0; j < SpaceDimension; j++ )
    {
    outputPoint[j] += transformedPoint[j];
    }


}



// Transform a point
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
typename BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::OutputPointType
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::TransformPoint(const InputPointType &point) const 
{
  
  WeightsType weights( m_WeightsFunction->GetNumberOfWeights() );
  ParameterIndexArrayType indices( m_WeightsFunction->GetNumberOfWeights() );
  OutputPointType outputPoint;
  bool inside;

  this->TransformPoint( point, outputPoint, weights, indices, inside );

  return outputPoint;

}

 
// Compute the Jacobian in one position 
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
const 
typename BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::JacobianType & 
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::GetJacobian( const InputPointType & point ) const
{

  // Zero all components of jacobian
  // NOTE: for efficiency, we only need to zero out the coefficients
  // that got fill last time this method was called.
  RegionType supportRegion;
  supportRegion.SetSize( m_SupportSize );
  supportRegion.SetIndex( m_LastJacobianIndex );

  typedef ImageRegionIterator<JacobianImageType> IteratorType;
  IteratorType m_Iterator[ SpaceDimension ];
  unsigned int j;

  for ( j = 0; j < SpaceDimension; j++ )
    {
    m_Iterator[j] = IteratorType( m_JacobianImage[j], supportRegion );
    }

  while ( ! m_Iterator[0].IsAtEnd() )
    {

    // zero out jacobian elements
    for ( j = 0; j < SpaceDimension; j++ )
      {
      m_Iterator[j].Set( NumericTraits<JacobianPixelType>::Zero );
      }

    for ( j = 0; j < SpaceDimension; j++ )
      {
      ++( m_Iterator[j] );
      }
    }

 
  ContinuousIndexType index;
  for ( j = 0; j < SpaceDimension; j++ )
    {
    index[j] = ( point[j] - m_GridOrigin[j] ) / m_GridSpacing[j];
    }

  // NOTE: if the support region does not lie totally within the grid
  // we assume zero displacement and return the input point
  if ( !m_ValidRegion.IsInside( index ) )
    {
    return m_Jacobian;
    }

  // Compute interpolation weights
  WeightsType weights( m_WeightsFunction->GetNumberOfWeights() );
  IndexType supportIndex;

  m_WeightsFunction->Evaluate( index, weights, supportIndex );
  m_LastJacobianIndex = supportIndex;

  // For each dimension, copy the weight to the support region
  supportRegion.SetIndex( supportIndex );
  unsigned long counter = 0;

  for ( j = 0; j < SpaceDimension; j++ )
    {
    m_Iterator[j] = IteratorType( m_JacobianImage[j], supportRegion );
    }

  while ( ! m_Iterator[0].IsAtEnd() )
    {

    // copy weight to jacobian image
    for ( j = 0; j < SpaceDimension; j++ )
      {
      m_Iterator[j].Set( static_cast<JacobianPixelType>( weights[counter] ) );
      }

    // go to next coefficient in the support region
    ++ counter;
    for ( j = 0; j < SpaceDimension; j++ )
      {
      ++( m_Iterator[j] );
      }
    }


  // Return the results
  return m_Jacobian;

}

 
  
} // namespace

#endif
