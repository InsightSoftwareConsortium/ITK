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

namespace itk
{
template< class TPixel, unsigned int VDimension>
void
FilterImageSingleOperator<TPixel, VDimension>
::GenerateInputRequestedRegion()
{
  long left;
  unsigned long right;
  unsigned long inputRequestedRegionSize[InputImage::ImageDimension];
  InputImage::Index inputRequestedStartIndex;
  const unsigned long *outputRegionSize = outputPtr->GetRegionSize();
  const OutputImage::Index outputRegionStartIndex
    = outputPtr->GetRegionStartIndex();
  const unsigned long *inputBufferSize = inputPtr->GetBufferSize();
  const InputImage::Index inputBufferStartIndex =
    inputPtr->GetBufferStartIndex();
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr= this->GetOutput();

  // Call superclass' implementation of this method.
  Superclass::GenerateInputRequestedRegion();

  // Set the input region as close as possible to the ideal size.
  // Calculate the distance we can safely expand the input region up
  // to the ideal size. [ASSUMES INPUT AND OUTPUT IMAGES ARE EQUAL
  // SIZES AND REGION IS CONTAINED IN BUFFER]
  m_CheckBounds = false;
  for (int i = 0; i < InputImage::ImageDimension; ++i)
    {
      left = outputRegionStartIndex[i] - inputBufferStartIndex[i];
      inputRequestedRegionStartIndex[i] =
        outputRegionStartIndex[i] - std::min(left, operatorRadius[i]);
      if (left < operatorRadius[i]) m_CheckBounds = true;
      
      right = inputBufferStartIndex[i] + inputBufferSize[i] -
        outputRegionStartIndex[i] + outputRegionSize[i];
      inputRequestedRegionSize[i] =
        outputRegionSize[i] + std::min(right, operatorRadius[i]);
      if (right < operatorRadius[i]) m_CheckBounds = true;
    }

  inputPtr->SetRegionSize( inputRequestedRegionSize );
  inputPtr->SetRegionStartIndex( inputRequestedRegionStartIndex );
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
  output->SetBufferSize(output->GetRegionSize());
  output->SetBufferStartIndex(outputPtr->GetRegionStartIndex());
  output->Allocate();
  
  // Filter
  if (m_CheckBounds)
    {
      // A two-pass filtering algorithm is required, since we have to
      // accommodate buffer boundaries.
      NeighborhoodAlgorithm::DoUnsynchedInnerProduct<TPixel, VDimension>
        ( input, output, *m_Operator );
    }
  else
    {
      // Set up a Neighborhood Iterator on the input image
      // according to the region size of the output image.
      // (input and output region sizes are not the same!)
      RegionNeighborhoodIterator<TPixel, VDimension> rni
        (
         m_Operator->GetRadius(),
         input,
         output->GetStartIndex(),
         output->GetRegionSize()
         );
      
      // One-pass filtering algorithm.
      NeighborhoodAlgorithm::DoUnsynchedInnerProduct<
        RegionNeighborhoodIterator<TPixel,VDimension>,
        TPixel *,
        NeighborhoodOperator<TPixel, VDimension>
        >
        ( rni, output->GetBufferPointer(), *m_Operator );
    }

}

} // end namespace itk
