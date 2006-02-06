/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineInterpolationWeightFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBSplineInterpolationWeightFunction_txx
#define __itkBSplineInterpolationWeightFunction_txx

#include "itkBSplineInterpolationWeightFunction.h"
#include "itkImage.h"
#include "itkMatrix.h"
#include "itkImageRegionConstIteratorWithIndex.h"

// anonymous namespace
namespace
{
//--------------------------------------------------------------------------
// The 'floor' function on x86 and mips is many times slower than these
// and is used a lot in this code, optimize for different CPU architectures
inline int BSplineFloor(double x)
{
#if defined mips || defined sparc || defined __ppc__
  return (int)((unsigned int)(x + 2147483648.0) - 2147483648U);
#elif defined i386 || defined _M_IX86
  union { unsigned int hilo[2]; double d; } u;  
  u.d = x + 103079215104.0;
  return (int)((u.hilo[1]<<16)|(u.hilo[0]>>16));  
#else
  return int(floor(x));
#endif
}

}

namespace itk
{

/**
 * Constructor
 */
template <class TCoordRep, unsigned int VSpaceDimension, unsigned int VSplineOrder>
BSplineInterpolationWeightFunction<TCoordRep, VSpaceDimension, VSplineOrder>
::BSplineInterpolationWeightFunction()
{

  // Initialize the number of weights;
  m_NumberOfWeights = 
    static_cast<unsigned long>( pow( static_cast<double>(SplineOrder + 1),
                                     static_cast<double>(SpaceDimension) ) );

  // Initialize support region is a hypercube of length SplineOrder + 1
  m_SupportSize.Fill( SplineOrder + 1 );

  // Initialize offset to index lookup table
  m_OffsetToIndexTable.set_size( m_NumberOfWeights, SpaceDimension );

  typedef Image<char,SpaceDimension> CharImageType;
  typename CharImageType::Pointer tempImage = CharImageType::New();
  tempImage->SetRegions( m_SupportSize );
  tempImage->Allocate();
  tempImage->FillBuffer( 0 );


  typedef ImageRegionConstIteratorWithIndex<CharImageType> IteratorType;
  IteratorType iterator( tempImage, tempImage->GetBufferedRegion() );
  unsigned long counter = 0;

  while ( !iterator.IsAtEnd() )
    {
    for(unsigned  int j = 0; j < SpaceDimension; j++ )
      {
      m_OffsetToIndexTable[counter][j] = iterator.GetIndex()[j];
      }
    ++counter;
    ++iterator;
    }  


  // Initialize the interpolation kernel
  m_Kernel = KernelType::New();

}


/**
 * Standard "PrintSelf" method
 */
template <class TCoordRep, unsigned int VSpaceDimension, unsigned int VSplineOrder>
void
BSplineInterpolationWeightFunction<TCoordRep, VSpaceDimension, VSplineOrder>
::PrintSelf(
  std::ostream& os, 
  Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  
  os << indent << "NumberOfWeights: " << m_NumberOfWeights << std::endl;
  os << indent << "SupportSize: " << m_SupportSize << std::endl;
}



/**
 * Compute weights for interpolation at continous index position
 */
template <class TCoordRep, unsigned int VSpaceDimension, unsigned int VSplineOrder>
typename BSplineInterpolationWeightFunction<TCoordRep, VSpaceDimension, VSplineOrder>
::WeightsType
BSplineInterpolationWeightFunction<TCoordRep, VSpaceDimension, VSplineOrder>
::Evaluate(
  const ContinuousIndexType& index ) const
{

  WeightsType weights( m_NumberOfWeights );
  IndexType startIndex;

  this->Evaluate( index, weights, startIndex );

  return weights;
}



/**
 * Compute weights for interpolation at continous index position
 */
template <class TCoordRep, unsigned int VSpaceDimension, unsigned int VSplineOrder>
void BSplineInterpolationWeightFunction<TCoordRep, VSpaceDimension, VSplineOrder>
::Evaluate(
  const ContinuousIndexType & index,
  WeightsType & weights, 
  IndexType & startIndex ) const
{

  unsigned int j, k;

  // Find the starting index of the support region
  for ( j = 0; j < SpaceDimension; j++ )
    {
    startIndex[j] = static_cast<typename IndexType::IndexValueType>(
      BSplineFloor( index[j] - static_cast<double>( SplineOrder / 2 ) ) );
    }

  // Compute the weights
  Matrix<double,SpaceDimension,SplineOrder + 1> weights1D;
  for ( j = 0; j < SpaceDimension; j++ )
    {
    double x = index[j] - static_cast<double>( startIndex[j] );

    for( k = 0; k <= SplineOrder; k++ )
      {
      weights1D[j][k] = m_Kernel->Evaluate( x );
      x -= 1.0;
      }
    }

  for ( k = 0; k < m_NumberOfWeights; k++ )
    {
    
    weights[k] = 1.0;
    
    for ( j = 0; j < SpaceDimension; j++ )
      {
      weights[k] *= weights1D[j][ m_OffsetToIndexTable[k][j] ];
      }

    }

}


} // end namespace itk

#endif

