/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetCurvatureFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#include "vnl/vnl_math.h"

namespace itk
{

/**
 *
 */
template<class TInputImage>
void
LevelSetCurvatureFunction<TInputImage>
::SetInputImage( InputImageType * ptr )
{
  this->Superclass::SetInputImage( ptr );

  m_ImageSize = 
    this->GetInputImage()->GetLargestPossibleRegion().GetSize();

  m_ImageSizeOK = true;
  for( int j = 0; j < ImageDimension; j++ )
    {
    if( m_ImageSize[j] < 5 )
      {
      m_ImageSizeOK = false;
      break;
      }

    }

  m_Curvature = 0.0;
  m_Magnitude = 0.0;

  m_EpsilonMagnitude = 1e-9;

  for( int j = 0; j < ImageDimension; j++ )
    {
    for( int k = 0; k < ImageDimension; k++ )
      {
      m_Variable[j][k] = j + k;
      if( m_Variable[j][k] >= ImageDimension )
        {
        m_Variable[j][k] = m_Variable[j][k] - ImageDimension;
        }
      }
    }
  
}

/**
 *
 */
template<class TInputImage>
void
LevelSetCurvatureFunction<TInputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "calculate mean curvature" << std::endl;
}


/**
 *
 */
template<class TInputImage>
double
LevelSetCurvatureFunction<TInputImage>
::Evaluate(
const IndexType& index )
{ 
  if( !m_Image )
    {
     throw ExceptionObject();
    }

  if( !m_ImageSizeOK )
    {
    return 0.0;
    }

  this->CalculateDerivatives( index );
  this->CalculateCurvature();
  return( m_Curvature );
}


/**
 *
 */
template<class TInputImage>
void
LevelSetCurvatureFunction<TInputImage>
::CalculateDerivatives( 
const IndexType& index )
{

  typename InputImageType::Pointer image = this->GetInputImage();

  m_Magnitude = 0.0;
  m_BorderPixel = false;

  // get the center value
  m_CenterValue = (double) 
    ScalarTraits<PixelType>::GetScalar( image->GetPixel( index ) );

  m_NeighIndex = index;

  for( int j = 0; j < ImageDimension; j++ )
    {
    if( index[j] <= 1 || index[j] >= m_ImageSize[j] - 3 ) 
      { 
      m_BorderPixel = true; 
      break; 
      }
    
    // calculate the first order derivative
    m_NeighIndex[j] = index[j] + 1;
    m_DiffValue = (double) 
      ScalarTraits<PixelType>::GetScalar( image->GetPixel( m_NeighIndex ) );

    m_NeighIndex[j] = index[j] - 1;
    m_DiffValue -= (double) 
      ScalarTraits<PixelType>::GetScalar( image->GetPixel( m_NeighIndex ) );

    m_FirstDerivative[j] = m_DiffValue / 2.0;
    m_Magnitude += vnl_math_sqr( m_FirstDerivative[j] );

    // calculate the second order derivatives
    m_NeighIndex[j] = index[j] + 2;
    m_DiffValue = (double) 
      ScalarTraits<PixelType>::GetScalar( image->GetPixel( m_NeighIndex ) );

    m_NeighIndex[j] = index[j] - 2;
    m_DiffValue += (double) 
      ScalarTraits<PixelType>::GetScalar( image->GetPixel( m_NeighIndex ) );

    m_SecondDerivative[j][j] = ( m_DiffValue - m_CenterValue * 2.0 ) / 4.0;

    // reset m_NeighIndex
    m_NeighIndex[j] = index[j];

    }

  // take the sqrt of the magnitude
  m_Magnitude = vnl_math_sqrt( m_Magnitude );

  // return zero curvature if magnitude is too small
  // or if this is a border pixel
  if( m_Magnitude < m_EpsilonMagnitude || m_BorderPixel ) 
    {
    m_Magnitude = 0.0;
    return;
    }

  m_LeftIndex = index;
  m_RightIndex = index;

  for( int j = 0; j < ImageDimension - 1; j++ )
    {
    // calculate the mixed derivatives
    m_RightIndex[j] = index[j] + 1;
    m_LeftIndex[j] = index[j] - 1;

    for( int k = j + 1; k < ImageDimension; k++ )
      {
      m_RightIndex[k] = index[k] + 1;
      m_LeftIndex[k] = index[k] + 1;

      m_DiffValue = (double) 
        ScalarTraits<PixelType>::GetScalar( image->GetPixel( m_RightIndex ) );

      m_DiffValue -= (double) 
        ScalarTraits<PixelType>::GetScalar( image->GetPixel( m_LeftIndex ) );

      m_RightIndex[k] = index[k] - 1;
      m_LeftIndex[k] = index[k] - 1;

      m_DiffValue -= (double) 
        ScalarTraits<PixelType>::GetScalar( image->GetPixel( m_RightIndex ) );

      m_DiffValue += (double) 
        ScalarTraits<PixelType>::GetScalar( image->GetPixel( m_LeftIndex ) );

      m_SecondDerivative[j][k] = m_DiffValue / 4.0;
      m_SecondDerivative[k][j] = m_SecondDerivative[j][k];

      // reset the indices
      m_LeftIndex[k] = index[k];
      m_RightIndex[k] = index[k];

      }

    // reset the indices
    m_RightIndex[j] = index[j];
    m_LeftIndex[j] = index[j];

    }

}


/**
 *
 */
template<class TInputImage>
void
LevelSetCurvatureFunction<TInputImage>
::CalculateCurvature( )
{

  switch( ImageDimension )
    {
    case 2:
      this->CalculateCurvature2D();
      break;
    case 3:
      this->CalculateCurvature3D();
      break;
    default:
      // can only handle 2D and 3D for now
      m_Curvature = 0.0;
    }

}


/**
 *
 */
template<class TInputImage>
void
LevelSetCurvatureFunction<TInputImage>
::CalculateCurvature2D( )
{

  m_Curvature = 0.0;

  if( m_BorderPixel || m_Magnitude < m_EpsilonMagnitude )
    {
    return;
    }

  for( int s = 0; s < ImageDimension; s++ )
    {
    m_Curvature += m_SecondDerivative[ m_Variable[0][s] ][ m_Variable[0][s] ] * 
       vnl_math_sqr( m_FirstDerivative[ m_Variable[1][s] ] );
    }
  m_Curvature -= 2.0 * m_FirstDerivative[0] * m_FirstDerivative[1] * m_SecondDerivative[0][1];

  m_Curvature /= m_Magnitude * m_Magnitude * m_Magnitude;

}


/**
 *
 */
template<class TInputImage>
void
LevelSetCurvatureFunction<TInputImage>
::CalculateCurvature3D( )
{

  m_Curvature = 0;
  
  if( m_BorderPixel )
    {
    return;
    }

  for( int s = 0; s < ImageDimension; s++ )
    {
    m_Curvature += ( 
      m_SecondDerivative[ m_Variable[0][s] ][ m_Variable[0][s] ] + 
      m_SecondDerivative[ m_Variable[1][s] ][ m_Variable[1][s] ] ) * 
      vnl_math_sqr( m_FirstDerivative[ m_Variable[2][s] ] );

    m_Curvature -= 2.0 * 
      m_FirstDerivative[ m_Variable[0][s] ] * 
      m_FirstDerivative[ m_Variable[1][s] ] * 
      m_SecondDerivative[ m_Variable[0][s] ][ m_Variable[1][s] ];
    }

  m_Curvature /= m_Magnitude * m_Magnitude * m_Magnitude;


}




} // namespace itk
