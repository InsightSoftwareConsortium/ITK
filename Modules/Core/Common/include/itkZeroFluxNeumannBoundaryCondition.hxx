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
#ifndef itkZeroFluxNeumannBoundaryCondition_hxx
#define itkZeroFluxNeumannBoundaryCondition_hxx

#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
typename ZeroFluxNeumannBoundaryCondition< TInputImage, TOutputImage >::OutputPixelType
ZeroFluxNeumannBoundaryCondition< TInputImage, TOutputImage >
::operator()(const OffsetType & point_index, const OffsetType & boundary_offset,
             const NeighborhoodType *data) const
{
  int linear_index = 0;

  // Return the value of the pixel at the closest boundary point.
  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    linear_index += ( point_index[i] + boundary_offset[i] ) * data->GetStride(i);
    }

  // The reinterpret_cast is necessary, cause we will have a warning if we
  // do not do this. (In fact this function exists for legacy
  // reasons. The overloaded function below should be (and is) used instead).
  // See any of the neighborhood iterators.
  //
  // (data->operator[](linear_index)) is guaranteed to be a pointer to
  // TImage::PixelType except for VectorImage, in which case, it will be a
  // pointer to TImage::InternalPixelType.
  //
  // A typical neighborhood iterator working on an image will use the boundary
  // condition in the following manner:
  //
  // \code
  // // Initialize the functor typically in the constructor.
  // m_NeighborhoodAccessorFunctor = image->GetNeighborhoodAccessor();
  // m_NeighborhoodAccessorFunctor->SetBegin( image->GetBufferPointer() );
  //
  // m_NeighborhoodAccessorFunctor.BoundaryCondition(
  //    point_index, boundary_offset, data, m_ChosenBoundaryCondition );
  // \endcode
  //
  return static_cast< OutputPixelType >
    (*( reinterpret_cast< PixelType * >( ( data->operator[](linear_index) ) ) ) );
}

template< typename TInputImage, typename TOutputImage >
typename ZeroFluxNeumannBoundaryCondition< TInputImage, TOutputImage >::OutputPixelType
ZeroFluxNeumannBoundaryCondition< TInputImage, TOutputImage >
::operator()(const OffsetType & point_index, const OffsetType & boundary_offset,
             const NeighborhoodType *data,
             const NeighborhoodAccessorFunctorType & neighborhoodAccessorFunctor) const
{
  int linear_index = 0;

  // Return the value of the pixel at the closest boundary point.
  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    linear_index += ( point_index[i] + boundary_offset[i] ) * data->GetStride(i);
    }

  return static_cast< OutputPixelType >
    ( neighborhoodAccessorFunctor.Get( data->operator[](linear_index) ) );
}


template< typename TInputImage, typename TOutputImage >
typename ZeroFluxNeumannBoundaryCondition< TInputImage, TOutputImage >::RegionType
ZeroFluxNeumannBoundaryCondition< TInputImage, TOutputImage >
::GetInputRequestedRegion( const RegionType & inputLargestPossibleRegion,
                           const RegionType & outputRequestedRegion ) const
{
  IndexType inputIndex = inputLargestPossibleRegion.GetIndex();
  SizeType  inputSize  = inputLargestPossibleRegion.GetSize();

  IndexType outputIndex = outputRequestedRegion.GetIndex();
  SizeType  outputSize  = outputRequestedRegion.GetSize();

  IndexType  requestIndex;
  SizeType   requestSize;
  RegionType requestRegion;

  for ( unsigned int i = 0; i < ImageDimension; i++)
    {
    // Check if the output region is entirely below the low index of
    // the image region.
    if ( outputIndex[i] + static_cast< OffsetValueType >( outputSize[i] ) <= inputIndex[i] )
      {
      // Include an image layer one pixel thick closest to the outputRequestedRegion
      requestIndex[i] = inputIndex[i];
      requestSize[i]  = 1;
      }
    // Check if the output is entirely above the high index of the
    // image region.
    else if (  outputIndex[i] >= inputIndex[i] + static_cast< OffsetValueType >( inputSize[i] ) )
      {
      // Include an image layer one pixel thick closest to the outputRequestedRegion
      requestIndex[i] = inputIndex[i] + static_cast< OffsetValueType >( inputSize[i] ) - 1;
      requestSize[i]  = 1;
      }
    // The output region intersects the image region.
    else
      {
      requestIndex[i] = inputIndex[i];
      requestSize[i]  = inputSize[i];

      // First check the start index
      if ( requestIndex[i] < outputIndex[i] )
        {
        // How much do we need to adjust
        OffsetValueType crop = outputIndex[i] - requestIndex[i];

        // Adjust the start index and the size of the current region
        requestIndex[i] += crop;
        requestSize[i] -= static_cast< SizeValueType >( crop );
        }
      // Now check the final size
      if ( requestIndex[i] + static_cast< OffsetValueType >( requestSize[i] ) >
           outputIndex[i] + static_cast< OffsetValueType >( outputSize[i] ) )
        {
        // How much do we need to adjust
        OffsetValueType crop = requestIndex[i] + static_cast< OffsetValueType >( requestSize[i] )
          - outputIndex[i] - static_cast< OffsetValueType >( outputSize[i] );

        // Adjust the size
        requestSize[i] -= static_cast< SizeValueType >( crop );
        }
      }
    }

  RegionType inputRequestedRegion( requestIndex, requestSize );

  return inputRequestedRegion;
}


template< typename TInputImage, typename TOutputImage >
typename ZeroFluxNeumannBoundaryCondition< TInputImage, TOutputImage >::OutputPixelType
ZeroFluxNeumannBoundaryCondition< TInputImage, TOutputImage >
::GetPixel( const IndexType & index, const TInputImage * image ) const
{
  RegionType imageRegion = image->GetLargestPossibleRegion();
  IndexType  imageIndex  = imageRegion.GetIndex();
  SizeType   imageSize   = imageRegion.GetSize();

  IndexType lookupIndex;

  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    IndexValueType lowerIndex = imageIndex[i];
    IndexValueType upperIndex = imageIndex[i] + static_cast< IndexValueType >( imageSize[i] ) - 1;
    if ( index[i] < lowerIndex )
      {
      lookupIndex[i] = lowerIndex;
      }
    else if ( index[i] > upperIndex )
      {
      lookupIndex[i] = upperIndex;
      }
    else // in bounds
      {
      lookupIndex[i] = index[i];
      }
    }

  return static_cast< OutputPixelType >( image->GetPixel( lookupIndex ) );
}

} // end namespace itk

#endif
