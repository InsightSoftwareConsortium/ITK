/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageSingleOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#include "itkNeighborhoodAlgorithm.h"
#include "itkRegionNeighborhoodIterator.h"
#include "vnl_math.h"
namespace itk
{
template< class TPixel, unsigned int VDimension>
void
FilterImageSingleOperator<TPixel, VDimension>
::GenerateInputRequestedRegion()
{
  long left;
  unsigned long right;
  const typename OutputImage::Size outputRegionSize
    = this->GetOutput()->GetRequestedRegion().GetSize();
  const typename OutputImage::Index outputRegionStartIndex
    = this->GetOutput()->GetRequestedRegion().GetIndex();
  const typename InputImage::Size inputBufferSize
    = this->GetInput()->GetBufferedRegion().GetSize();
  const typename InputImage::Index inputBufferStartIndex
    = this->GetInput()->GetBufferedRegion().GetIndex();;

  Size<VDimension> operatorRadius = m_Operator.GetRadius();
  
  ImageRegion<VDimension> requestedRegion;
  Size<VDimension> requestedSize;
  Index<VDimension> requestedIndex;
  
  // Call superclass' implementation of this method.
  Superclass::GenerateInputRequestedRegion();

  // Set the input region as close as possible to the ideal size.
  // Calculate the distance we can safely expand the input region up
  // to the ideal size.
  m_CheckBounds = false;
  for (int i = 0; i < InputImage::ImageDimension; ++i)
    {
      left = outputRegionStartIndex[i] - inputBufferStartIndex[i];
      requestedIndex[i] = outputRegionStartIndex[i]
        - ::vnl_math_min((long)left,
                         (long)operatorRadius[i]);
      if (left < operatorRadius[i]) m_CheckBounds = true;
      
      right = inputBufferStartIndex[i] + inputBufferSize[i] -
        outputRegionStartIndex[i] + outputRegionSize[i];
      requestedSize[i] = outputRegionSize[i]
                            + ::vnl_math_min((long)right,
                                             (long)operatorRadius[i]); 
      if (right < operatorRadius[i]) m_CheckBounds = true;
    }

  requestedRegion.SetSize( requestedSize );
  requestedRegion.SetIndex( requestedIndex);
  
  this->GetInput()->SetRequestedRegion( requestedRegion );
}

template< class TPixel, unsigned int VDimension>
void
FilterImageSingleOperator<TPixel, VDimension>
::GenerateData()
{
  // Allocate output
  Image::Pointer output = this->GetOutput();
  Image::Pointer input  = this->GetInput();

  // Need to allocate output buffer memory.
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();
  
  // Filter
  if (m_CheckBounds)
    {
      // A two-pass filtering algorithm is required, since we have to
      // accommodate buffer boundaries.
      NeighborhoodAlgorithm::DoUnsynchedInnerProduct<TPixel, VDimension>
        ( input, output, m_Operator );
    }
  else
    {
      // Set up a Neighborhood Iterator on the input image
      // according to the region size of the output image.
      // (input and output region sizes are not the same!)
      RegionNeighborhoodIterator<TPixel, VDimension> rni
        (
         m_Operator.GetRadius(),
         input,
         output->GetRequestedRegion());
      
      // One-pass filtering algorithm.
      NeighborhoodAlgorithm::DoUnsynchedInnerProduct<
        RegionNeighborhoodIterator<TPixel,VDimension>,
        TPixel *,
        Neighborhood<TPixel, VDimension>  >
        ( rni, output->GetBufferPointer(), m_Operator );
    }

}

} // end namespace itk
