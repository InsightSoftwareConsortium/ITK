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
#ifndef itkBoxMeanImageFilter_hxx
#define itkBoxMeanImageFilter_hxx

#include "itkBoxMeanImageFilter.h"
#include "itkProgressAccumulator.h"
#include "itkBoxUtilities.h"


/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://hdl.handle.net/1926/555
 * http://www.insight-journal.org/browse/publication/160
 *
 */

namespace itk
{
template< typename TInputImage, typename TOutputImage >
BoxMeanImageFilter< TInputImage, TOutputImage >
::BoxMeanImageFilter()
{}

template< typename TInputImage, typename TOutputImage >
void
BoxMeanImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId)
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
