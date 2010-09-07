/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptMeanImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMeanImageFilter_txx
#define __itkMeanImageFilter_txx
#include "itkMeanImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
MeanImageFilter< TInputImage, TOutputImage >
::MeanImageFilter()
{}

template< class TInputImage, class TOutputImage >
void
MeanImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       int threadId)
{
  unsigned int i;

  ZeroFluxNeumannBoundaryCondition< InputImageType > nbc;

  ConstNeighborhoodIterator< InputImageType > bit;
  ImageRegionIterator< OutputImageType >      it;

  // Allocate output
  typename OutputImageType::Pointer output = this->GetOutput();
  typename  InputImageType::ConstPointer input  = this->GetInput();

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType > bC;
  faceList = bC( input, outputRegionForThread, this->GetRadius() );

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >::FaceListType::iterator fit;

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  InputRealType sum;

  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    bit = ConstNeighborhoodIterator< InputImageType >(this->GetRadius(),
                                                      input, *fit);
    unsigned int neighborhoodSize = bit.Size();
    it = ImageRegionIterator< OutputImageType >(output, *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    while ( !bit.IsAtEnd() )
      {
      sum = NumericTraits< InputRealType >::Zero;
      for ( i = 0; i < neighborhoodSize; ++i )
        {
        sum += static_cast< InputRealType >( bit.GetPixel(i) );
        }

      // get the mean value
      it.Set( static_cast< OutputPixelType >( sum / double(neighborhoodSize) ) );

      ++bit;
      ++it;
      progress.CompletedPixel();
      }
    }
}
} // end namespace itk

#endif
