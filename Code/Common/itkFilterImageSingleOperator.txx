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
#include "itkNeighborhoodOperator.h"
#include "itkRegionNeighborhoodIterator.h"
#include <vnl/vnl_math.h>
namespace itk
{

template< class TPixel, unsigned int VDimension>
void
FilterImageSingleOperator<TPixel, VDimension>
::GenerateData()
{
  // Allocate output
  ImageType::Pointer output = this->GetOutput();
  ImageType::Pointer input  = this->GetInput();
 
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
