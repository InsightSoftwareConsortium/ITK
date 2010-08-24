/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoxMeanImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBoxMeanImageFilter_txx
#define __itkBoxMeanImageFilter_txx

#include "itkImage.h"
#include "itkBoxMeanImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkOffset.h"
#include "itkProgressAccumulator.h"
#include "itkNumericTraits.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkBoxUtilities.h"

// #include "ioutils.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
BoxMeanImageFilter< TInputImage, TOutputImage >
::BoxMeanImageFilter()
{}

template< class TInputImage, class TOutputImage >
void
BoxMeanImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, int threadId)
{
  // Accumulate type is too small
  typedef typename NumericTraits< PixelType >::RealType    AccPixType;
  typedef Image< AccPixType, TInputImage::ImageDimension > AccumImageType;

  typename TInputImage::SizeType internalRadius;
  for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
    {
    internalRadius[i] = this->GetRadius()[i] + 1;
    }

  const InputImageType *inputImage = this->GetInput();
  OutputImageType *     outputImage = this->GetOutput();
  RegionType            accumRegion = outputRegionForThread;
  accumRegion.PadByRadius(internalRadius);
  accumRegion.Crop( inputImage->GetRequestedRegion() );

  ProgressReporter progress( this, threadId, 2 * accumRegion.GetNumberOfPixels() );

  typename AccumImageType::Pointer accImage = AccumImageType::New();
  accImage->SetRegions(accumRegion);
  accImage->Allocate();

  BoxAccumulateFunction< TInputImage, AccumImageType >(inputImage, accImage,
                                                       accumRegion,
                                                       accumRegion,
                                                       progress);
  BoxMeanCalculatorFunction< AccumImageType, TOutputImage >(accImage.GetPointer(), outputImage,
                                                            accumRegion,
                                                            outputRegionForThread,
                                                            this->GetRadius(),
                                                            progress);
}
} // end namespace itk
#endif
