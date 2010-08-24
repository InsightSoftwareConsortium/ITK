/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoxSigmaImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBoxSigmaImageFilter_txx
#define __itkBoxSigmaImageFilter_txx

#include "itkImage.h"
#include "itkBoxSigmaImageFilter.h"
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
BoxSigmaImageFilter< TInputImage, TOutputImage >
::BoxSigmaImageFilter()
{}

template< class TInputImage, class TOutputImage >
void
BoxSigmaImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, int threadId)
{
  // Accumulate type is too small
  typedef typename itk::NumericTraits< PixelType >::RealType             AccValueType;
  typedef typename itk::Vector< AccValueType, 2 >                        AccPixType;
  typedef typename itk::Image< AccPixType, TInputImage::ImageDimension > AccumImageType;

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

  BoxSquareAccumulateFunction< TInputImage, AccumImageType >(inputImage, accImage,
                                                             accumRegion,
                                                             accumRegion,
                                                             progress);
  BoxSigmaCalculatorFunction< AccumImageType, TOutputImage >(accImage, outputImage,
                                                             accumRegion,
                                                             outputRegionForThread,
                                                             this->GetRadius(),
                                                             progress);
}
} // end namespace itk
#endif
