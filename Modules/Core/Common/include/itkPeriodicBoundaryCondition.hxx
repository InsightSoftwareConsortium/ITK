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
#ifndef itkPeriodicBoundaryCondition_hxx
#define itkPeriodicBoundaryCondition_hxx

#include "itkConstNeighborhoodIterator.h"
#include "itkPeriodicBoundaryCondition.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
typename PeriodicBoundaryCondition< TInputImage, TOutputImage >::OutputPixelType
PeriodicBoundaryCondition< TInputImage, TOutputImage >
::operator()(const OffsetType & point_index, const OffsetType & boundary_offset,
             const NeighborhoodType *data) const
{
  const ConstNeighborhoodIterator< TInputImage > *iterator =
    dynamic_cast< const ConstNeighborhoodIterator< TInputImage > * >( data );
  typename TInputImage::PixelType * ptr;
  int          linear_index = 0;
  unsigned int i;

  // Find the pointer of the closest boundary pixel

  // Return the value of the pixel at the closest boundary point.
  for ( i = 0; i < ImageDimension; ++i )
    {
    linear_index += ( point_index[i] + boundary_offset[i] ) * data->GetStride(i);
    }
  // (data->operator[](linear_index)) is guaranteed to be a pointer to
  // TInputImage::PixelType except for VectorImage, in which case, it will be a
  // pointer to TInputImage::InternalPixelType.
  ptr = reinterpret_cast< PixelType * >( ( data->operator[](linear_index) ) );

  // Wrap the pointer around the image in the necessary dimensions.  If we have
  // reached this point, we can assume that we are on the edge of the BUFFERED
  // region of the image.  Boundary conditions are only invoked if touching the
  // actual memory boundary.

  // These are the step sizes for increments in each dimension of the image.
  const typename TInputImage::OffsetValueType * offset_table =
    iterator->GetImagePointer()->GetOffsetTable();

  for ( i = 0; i < ImageDimension; ++i )
    {
    if ( boundary_offset[i] != 0 )
      { // If the neighborhood overlaps on the low edge, then wrap from the
        // high edge of the image.
      if ( point_index[i] < static_cast< OffsetValueType >( iterator->GetRadius(i) ) )
        {
        ptr += iterator->GetImagePointer()->GetBufferedRegion().GetSize()[i]
               * offset_table[i] - boundary_offset[i] * offset_table[i];
        }
      else // wrap from the low side of the image
        {
        ptr -= iterator->GetImagePointer()->GetBufferedRegion().GetSize()[i]
               * offset_table[i] + boundary_offset[i] * offset_table[i];
        }
      }
    }

  return static_cast< OutputPixelType >( *ptr );
}

template< typename TInputImage, typename TOutputImage >
typename PeriodicBoundaryCondition< TInputImage, TOutputImage >::OutputPixelType
PeriodicBoundaryCondition< TInputImage, TOutputImage >
::operator()(const OffsetType & point_index, const OffsetType & boundary_offset,
             const NeighborhoodType *data,
             const NeighborhoodAccessorFunctorType & neighborhoodAccessorFunctor) const
{
  const ConstNeighborhoodIterator< TInputImage > *iterator =
    dynamic_cast< const ConstNeighborhoodIterator< TInputImage > * >( data );
  typename TInputImage::InternalPixelType * ptr;
  int          linear_index = 0;
  unsigned int i;

  // Find the pointer of the closest boundary pixel
  //  std::cout << "Boundary offset = " << boundary_offset << std::endl;
  // std::cout << "point index = " << point_index << std::endl;

  // Return the value of the pixel at the closest boundary point.
  for ( i = 0; i < ImageDimension; ++i )
    {
    linear_index += ( point_index[i] + boundary_offset[i] ) * data->GetStride(i);
    }
  ptr = data->operator[](linear_index);

  // Wrap the pointer around the image in the necessary dimensions.  If we have
  // reached this point, we can assume that we are on the edge of the BUFFERED
  // region of the image.  Boundary conditions are only invoked if touching the
  // actual memory boundary.

  // These are the step sizes for increments in each dimension of the image.
  const typename TInputImage::OffsetValueType * offset_table =
    iterator->GetImagePointer()->GetOffsetTable();

  for ( i = 0; i < ImageDimension; ++i )
    {
    if ( boundary_offset[i] != 0 )
      { // If the neighborhood overlaps on the low edge, then wrap from the
        // high edge of the image.
      if ( point_index[i] < static_cast< OffsetValueType >( iterator->GetRadius(i) ) )
        {
        ptr += iterator->GetImagePointer()->GetBufferedRegion().GetSize()[i]
               * offset_table[i] - boundary_offset[i] * offset_table[i];
        }
      else // wrap from the low side of the image
        {
        ptr -= iterator->GetImagePointer()->GetBufferedRegion().GetSize()[i]
               * offset_table[i] + boundary_offset[i] * offset_table[i];
        }
      }
    }

  return static_cast< OutputPixelType >( neighborhoodAccessorFunctor.Get(ptr) );
}


template< typename TInputImage, typename TOutputImage >
typename PeriodicBoundaryCondition< TInputImage, TOutputImage >::RegionType
PeriodicBoundaryCondition< TInputImage, TOutputImage >
::GetInputRequestedRegion( const RegionType & inputLargestPossibleRegion,
                           const RegionType & outputRequestedRegion ) const
{
  IndexType  imageIndex = inputLargestPossibleRegion.GetIndex();
  SizeType   imageSize  = inputLargestPossibleRegion.GetSize();

  IndexType outputIndex = outputRequestedRegion.GetIndex();
  SizeType  outputSize  = outputRequestedRegion.GetSize();

  IndexType inputRequestedIndex;
  SizeType  inputRequestedSize;

  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    // Check for image boundary overlap in the requested region
    IndexValueType lowIndex =
      ( ( outputIndex[i] - imageIndex[i] ) % static_cast< IndexValueType >( imageSize[i] ) );
    if ( lowIndex < 0 )
      {
      lowIndex += static_cast< IndexValueType >( imageSize[i] );
      }
    IndexValueType highIndex = lowIndex + static_cast< IndexValueType >( outputSize[i] );

    bool overlap = ( highIndex >= static_cast< IndexValueType >( imageSize[i] ) );

    if ( overlap )
      {
      // Request the totality of the image in this dimension
      inputRequestedIndex[i] = imageIndex[i];
      inputRequestedSize[i]  = imageSize[i];
      }
    else
      {
      // Remap the requested portion in this dimension into the image region.
      inputRequestedIndex[i] = lowIndex;
      inputRequestedSize[i]  = outputSize[i];
      }
    }
  RegionType inputRequestedRegion( inputRequestedIndex, inputRequestedSize );

  return inputRequestedRegion;
}


template< typename TInputImage, typename TOutputImage >
typename PeriodicBoundaryCondition< TInputImage, TOutputImage >::OutputPixelType
PeriodicBoundaryCondition< TInputImage, TOutputImage >
::GetPixel( const IndexType & index, const TInputImage * image ) const
{
  RegionType imageRegion = image->GetLargestPossibleRegion();
  IndexType  imageIndex  = imageRegion.GetIndex();
  SizeType   imageSize   = imageRegion.GetSize();

  IndexType lookupIndex;

  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    IndexValueType modIndex = ( ( index[i] - imageIndex[i] ) %
                                static_cast< IndexValueType >( imageSize[i] ) );

    if ( modIndex < 0 )
      {
      modIndex += static_cast< IndexValueType >( imageSize[i] );
      }

    lookupIndex[i] = modIndex + imageIndex[i];
    }

  return static_cast< OutputPixelType >( image->GetPixel( lookupIndex ) );
}

} // end namespace itk

#endif
