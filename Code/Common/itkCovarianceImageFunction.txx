/*=========================================================================

    Program:   Insight Segmentation & Registration Toolkit
    Module:    itkCovarianceImageFunction.txx
    Language:  C++
    Date:      $Date$
    Version:   $Revision$

    Copyright (c) 2002 Insight Consortium. All rights reserved.
    See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

       This software is distributed WITHOUT ANY WARRANTY; without even 
       the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
       PURPOSE.  See the above copyright notices for more information.

  =========================================================================*/
#ifndef _itkCovarianceImageFunction_txx
#define _itkCovarianceImageFunction_txx
#include "itkCovarianceImageFunction.h"

#include "itkNumericTraits.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{

/**
   * Constructor
   */
template <class TInputImage, class TCoordRep>
CovarianceImageFunction<TInputImage,TCoordRep>
::CovarianceImageFunction()
{
  m_NeighborhoodRadius = 1;
}


/**
   *
   */
template <class TInputImage, class TCoordRep>
void
CovarianceImageFunction<TInputImage,TCoordRep>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "NeighborhoodRadius: "  << m_NeighborhoodRadius << std::endl;
}


/**
 *
 */
template <class TInputImage, class TCoordRep>
typename CovarianceImageFunction<TInputImage,TCoordRep>
::RealType
CovarianceImageFunction<TInputImage,TCoordRep>
::EvaluateAtIndex(const IndexType& index) const
{
  RealType covariance;
  typedef  typename TInputImage::PixelType::ValueType  PixelComponentType;

  typedef  typename NumericTraits< PixelComponentType >::RealType PixelComponentRealType;
  
  const unsigned int VectorDimension = 
      ::itk::GetVectorDimension<typename TInputImage::PixelType>::VectorDimension;

  covariance = vnl_matrix< PixelComponentRealType >( VectorDimension, VectorDimension );
  covariance.fill( NumericTraits< PixelComponentRealType >::Zero );
  
  if( !m_Image )
    {
    covariance.fill( NumericTraits< PixelComponentRealType >::max() );
    return covariance;
    }
  
  if ( !this->IsInsideBuffer( index ) )
    {
    covariance.fill( NumericTraits< PixelComponentRealType >::max() );
    return covariance;
    }

  // Create an N-d neighborhood kernel, using a zeroflux boundary condition
  typename InputImageType::SizeType kernelSize;
  kernelSize.Fill( m_NeighborhoodRadius );
  
  ConstNeighborhoodIterator<InputImageType>
    it(kernelSize, m_Image, m_Image->GetBufferedRegion());

  // Set the iterator at the desired location
  it.SetLocation(index);

  // Walk the neighborhood
  const unsigned int size = it.Size();
  for (unsigned int i = 0; i < size; ++i)
    {
    for(unsigned int dimx=0; dimx<VectorDimension; dimx++)
      {
      for(unsigned int dimy=0; dimy<VectorDimension; dimy++)
        {
        covariance[dimx][dimy] += 
            static_cast<PixelComponentRealType>( it.GetPixel(i)[dimx] ) *
            static_cast<PixelComponentRealType>( it.GetPixel(i)[dimy] );
        }
      }
    }
  for(unsigned int dimx=0; dimx<VectorDimension; dimx++)
    {
    for(unsigned int dimy=0; dimy<VectorDimension; dimy++)
      {
      covariance[dimx][dimy] /= double(it.Size());
      }
    }
             
  return ( covariance );
}


} // namespace itk

#endif
