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
#ifndef itkCovarianceImageFunction_hxx
#define itkCovarianceImageFunction_hxx

#include "itkCovarianceImageFunction.h"
#include "itkMatrix.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{

template< typename TInputImage, typename TCoordRep >
CovarianceImageFunction< TInputImage, TCoordRep >
::CovarianceImageFunction() :
  m_NeighborhoodRadius( 1 )
{
}

template< typename TInputImage, typename TCoordRep >
typename CovarianceImageFunction< TInputImage, TCoordRep >
::RealType
CovarianceImageFunction< TInputImage, TCoordRep >
::EvaluateAtIndex(const IndexType & index) const
{
  typedef  typename TInputImage::PixelType PixelType;
  typedef  typename PixelType::ValueType   PixelComponentType;

  typedef  typename NumericTraits< PixelComponentType >::RealType PixelComponentRealType;


  if ( !this->GetInputImage() )
    {
    itkExceptionMacro(<< "No image connected to CovarianceImageFunction");
    }

  const unsigned int VectorDimension = this->GetInputImage()->GetNumberOfComponentsPerPixel();
  RealType covariance = RealType(VectorDimension, VectorDimension);

  if ( !this->IsInsideBuffer(index) )
    {
    covariance.fill( NumericTraits< PixelComponentRealType >::max() );
    return covariance;
    }


  covariance.fill(NumericTraits< PixelComponentRealType >::ZeroValue());

  typedef vnl_vector< PixelComponentRealType > MeanVectorType;
  MeanVectorType mean = MeanVectorType(VectorDimension);
  mean.fill(NumericTraits< PixelComponentRealType >::ZeroValue());

  // Create an N-d neighborhood kernel, using a zeroflux boundary condition
  typename InputImageType::SizeType kernelSize;
  kernelSize.Fill(m_NeighborhoodRadius);

  ConstNeighborhoodIterator< InputImageType >
  it( kernelSize, this->GetInputImage(), this->GetInputImage()->GetBufferedRegion() );

  // Set the iterator at the desired location
  it.SetLocation(index);

  // Walk the neighborhood
  const unsigned int size = it.Size();
  for ( unsigned int i = 0; i < size; ++i )
    {
    const PixelType pixel = it.GetPixel(i);

    for ( unsigned int dimx = 0; dimx < VectorDimension; dimx++ )
      {
      mean[dimx] += pixel[dimx];
      for ( unsigned int dimy = 0; dimy < VectorDimension; dimy++ )
        {
        covariance[dimx][dimy] +=
          static_cast< PixelComponentRealType >( pixel[dimx] )
          * static_cast< PixelComponentRealType >( pixel[dimy] );
        }
      }
    }

  const PixelComponentRealType rsize =
    static_cast< PixelComponentRealType >( size );

  mean /= rsize;

  for ( unsigned int dimx = 0; dimx < VectorDimension; dimx++ )
    {
    for ( unsigned int dimy = 0; dimy < VectorDimension; dimy++ )
      {
      covariance[dimx][dimy] /= rsize;
      covariance[dimx][dimy] -= mean[dimx] * mean[dimy];
      }
    }

  return ( covariance );
}

template< typename TInputImage, typename TCoordRep >
void
CovarianceImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NeighborhoodRadius: " << m_NeighborhoodRadius << std::endl;
}
} // end namespace itk

#endif
