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

  // Instantiate a kernel
  m_Kernel = KernelType::New();

  // Instantiate an identity transform
  typedef IdentityTransform<ScalarType,SpaceDimension> IdentityTransformType;
  IdentityTransformType::Pointer id = IdentityTransformType::New();
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
  for ( int j = 0; j < SpaceDimension; j++ )
    {
    m_CoefficientImage[j] = ImageType::New();
    m_CoefficientImage[j]->SetRegions( m_GridRegion );
    m_CoefficientImage[j]->SetOrigin( m_GridOrigin.GetDataPointer() );
    m_CoefficientImage[j]->SetSpacing( m_GridSpacing.GetDataPointer() );
    }

  // Setup variables for computing interpolation
  m_Offset = ( SplineOrder + 1 ) / 2;
  m_SupportSize.Fill( SplineOrder + 1 );
  m_ValidRegion = m_GridRegion;

  // Setup the support offset to index table
  RegionType supportRegion;
  supportRegion.SetSize( m_SupportSize );

  m_SupportOffsetToIndexTable.resize( supportRegion.GetNumberOfPixels(),
    SpaceDimension );

  typedef Image<char,SpaceDimension> CharImageType;
  typename CharImageType::Pointer tempImage = CharImageType::New();
  tempImage->SetRegions( supportRegion );
  tempImage->Allocate();
  tempImage->FillBuffer( 0 );
  
  typedef ImageRegionConstIteratorWithIndex<CharImageType> IteratorType;
  IteratorType iterator( tempImage, supportRegion );
  unsigned long counter = 0;

  while ( !iterator.IsAtEnd() )
    {
    for( int j = 0; j < SpaceDimension; j++ )
      {
      m_SupportOffsetToIndexTable[counter][j] = iterator.GetIndex()[j];
      }
    ++counter;
    ++iterator;
    }  

  // Initialize jacobian images
  for ( int j = 0; j < SpaceDimension; j++ )
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
    for ( int j = 0; j < SpaceDimension; j++ )
      {
      m_CoefficientImage[j]->SetRegions( m_GridRegion );
      m_JacobianImage[j]->SetRegions( m_GridRegion );
      }

    // Set the valid region
    typename RegionType::SizeType size = m_GridRegion.GetSize();
    typename RegionType::IndexType index = m_GridRegion.GetIndex();
    for ( int j = 0; j < SpaceDimension; j++ )
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
    for ( int j = 0; j < SpaceDimension; j++ )
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
    for ( int j = 0; j < SpaceDimension; j++ )
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

  for ( int j = 0; j < SpaceDimension; j++ )
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

  for ( int j = 0; j < SpaceDimension; j++ )
    {
    m_JacobianImage[j]->GetPixelContainer()->
      SetImportPointer( jacobianDataPointer, numberOfPixels );
    jacobianDataPointer += this->GetNumberOfParameters() + numberOfPixels;
    }

}

// Get the parameters
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
const 
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
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
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::OutputPointType
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::TransformPoint(const InputPointType &point) const 
{

  InputPointType transformedPoint;
  if ( m_BulkTransform )
    {
    transformedPoint = m_BulkTransform->TransformPoint( point );
    }
  else
    {
    transformedPoint = point;
    }

  WeightsType weights;
  IndexType supportIndex;
  bool valid;

  // Compute interpolation weights
  this->ComputeInterpolationWeights( point, weights, supportIndex, valid );

  // NOTE: if the support region does not lie totally within the grid
  // we assume zero displacement and return the input point
  if ( !valid )
    {
    return transformedPoint;
    }

  // For each dimension, correlate coefficient with weights
  RegionType supportRegion;
  supportRegion.SetSize( m_SupportSize );
  supportRegion.SetIndex( supportIndex );

  OutputPointType outputPoint;
  outputPoint.Fill( NumericTraits<ScalarType>::Zero );

  typedef ImageRegionConstIterator<ImageType> IteratorType;
  IteratorType m_Iterator[ SpaceDimension ];
  unsigned int j;
  unsigned long counter = 0;

  for ( j = 0; j < SpaceDimension; j++ )
    {
    m_Iterator[j] = IteratorType( m_CoefficientImage[j], supportRegion );
    }

  while ( ! m_Iterator[0].IsAtEnd() )
    {

    // compute total weight
    double w = 1.0;
    for ( j = 0; j < SpaceDimension; j++ )
      {
      w *= weights[j][ m_SupportOffsetToIndexTable[counter][j] ];
      }

    // multiply weigth with coefficient
    for ( j = 0; j < SpaceDimension; j++ )
      {
      outputPoint[j] += static_cast<ScalarType>( w * m_Iterator[j].Get());
      }

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

  return outputPoint;

}

 
// Compute the Jacobian in one position 
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
const 
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
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

 
  // Compute interpolation weights
  WeightsType weights;
  IndexType supportIndex;
  bool valid;

  this->ComputeInterpolationWeights( point, weights, supportIndex, valid );

  // NOTE: if the support region does not lie totally within the grid
  // we assume zero displacement and we return a zero Jacobian
  if ( !valid )
    {
    return m_Jacobian;
    }

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

    // compute total weight
    double w = 1.0;
    for ( j = 0; j < SpaceDimension; j++ )
      {
      w *= weights[j][ m_SupportOffsetToIndexTable[counter][j] ];
      }

    // copy weight to jacobian image
    for ( j = 0; j < SpaceDimension; j++ )
      {
      m_Iterator[j].Set( static_cast<JacobianPixelType>( w ) );
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



// Compute the interpolation weights.
// This method also returns the support region start index
// and checks whether or not the support region is wholly inside
// the grid
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineDeformableTransform<TScalarType, NDimensions,VSplineOrder>
::ComputeInterpolationWeights(
const InputPointType &point,
WeightsType & weights,
IndexType & startIndex,
bool & valid ) const 
{

  typedef ContinuousIndex<ScalarType,SpaceDimension> ContinuousIndexType;
  ContinuousIndexType index;
  unsigned int j, k;

  // Convert point to continuous index
  for ( j = 0; j < SpaceDimension; j++ )
    {
    index[j] = ( point[j] - m_GridOrigin[j] ) / m_GridSpacing[j];
    }

  // Check if point is within the valid grid region.
  // If not, assume zero displacement
  valid = m_ValidRegion.IsInside( index );

  if ( !valid ) 
    {
    return;
    }
  
  // Find the starting index of the support region
  for ( j = 0; j < SpaceDimension; j++ )
    {
    startIndex[j] = static_cast<typename RegionType::IndexValueType>(
      ceil( index[j] - 0.5 * static_cast<ScalarType>( SplineOrder + 1 ) ) );
    }

  itkDebugMacro( << std::endl << "Start Index: " 
    << startIndex << std::endl );

  
  // Compute the weights
  for ( j = 0; j < SpaceDimension; j++ )
    {

    ScalarType x = index[j] - static_cast<ScalarType>( startIndex[j] );

    for ( k = 0; k <= SplineOrder; k++ )
      {
      weights[j][k] = m_Kernel->Evaluate( x );
      x -= 1.0;
      }

    }

  itkDebugMacro( << std::endl << "Weights: " << std:: endl 
    << weights << std::endl );
 
}
 
  
} // namespace

#endif
