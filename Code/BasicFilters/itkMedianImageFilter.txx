/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptMedianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMedianImageFilter_txx
#define __itkMedianImageFilter_txx
#include "itkMedianImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"

#include <vector>
#include <algorithm>

namespace itk
{
template< class TInputImage, class TOutputImage >
MedianImageFilter< TInputImage, TOutputImage >
::MedianImageFilter()
{}

template< class TInputImage, class TOutputImage >
void
MedianImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       int threadId)
{
  // Allocate output
  typename OutputImageType::Pointer output = this->GetOutput();
  typename  InputImageType::ConstPointer input  = this->GetInput();

  // Find the data-set boundary "faces"
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType > bC;
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >::FaceListType
  faceList = bC( input, outputRegionForThread, this->GetRadius() );

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // All of our neighborhoods have an odd number of pixels, so there is
  // always a median index (if there where an even number of pixels
  // in the neighborhood we have to average the middle two values).

  ZeroFluxNeumannBoundaryCondition< InputImageType > nbc;
  std::vector< InputPixelType >                      pixels;
  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for ( typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >::FaceListType::iterator
        fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    ImageRegionIterator< OutputImageType > it = ImageRegionIterator< OutputImageType >(output, *fit);

    ConstNeighborhoodIterator< InputImageType > bit =
      ConstNeighborhoodIterator< InputImageType >(this->GetRadius(), input, *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();
    const unsigned int neighborhoodSize = bit.Size();
    const unsigned int medianPosition = neighborhoodSize / 2;
    while ( !bit.IsAtEnd() )
      {
      // collect all the pixels in the neighborhood, note that we use
      // GetPixel on the NeighborhoodIterator to honor the boundary conditions
      pixels.resize(neighborhoodSize);
      for ( unsigned int i = 0; i < neighborhoodSize; ++i )
        {
        pixels[i] = ( bit.GetPixel(i) );
        }

      // get the median value
      const typename std::vector< InputPixelType >::iterator medianIterator = pixels.begin() + medianPosition;
      std::nth_element( pixels.begin(), medianIterator, pixels.end() );
      it.Set( static_cast< typename OutputImageType::PixelType >( *medianIterator ) );

      ++bit;
      ++it;
      progress.CompletedPixel();
      }
    }
}
} // end namespace itk

#endif
