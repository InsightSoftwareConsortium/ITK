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

#ifndef itkPhysicalPointImageSource_hxx
#define itkPhysicalPointImageSource_hxx

#include "itkPhysicalPointImageSource.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{

template< typename TOutputImage >
void
PhysicalPointImageSource< TOutputImage >
::GenerateOutputInformation()
{
  // this methods is overloaded so that if the output image is a
  // VectorImage then the correct number of components are set.
  Superclass::GenerateOutputInformation();
  OutputImageType* output = this->GetOutput();

  if ( !output )
    {
    return;
    }
  if ( output->GetNumberOfComponentsPerPixel() != TOutputImage::ImageDimension )
    {
    output->SetNumberOfComponentsPerPixel( TOutputImage::ImageDimension );
    }
}

template< typename TOutputImage >
void
PhysicalPointImageSource< TOutputImage >
::ThreadedGenerateData (const RegionType &outputRegionForThread, ThreadIdType threadId)
{
  // Support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  TOutputImage *image = this->GetOutput(0);

  ImageRegionIteratorWithIndex< TOutputImage > it(image, outputRegionForThread);
  PointType pt;
  PixelType px;
  NumericTraits<PixelType>::SetLength(px, TOutputImage::ImageDimension );

  for (; !it.IsAtEnd(); ++it )
    {
    image->TransformIndexToPhysicalPoint( it.GetIndex(), pt );


    for( unsigned int i = 0; i < TOutputImage::ImageDimension; ++i )
      {
      px[i] = static_cast<typename PixelType::ValueType> (pt[i]);
      }
    it.Set( px );
    progress.CompletedPixel();
    }
}
} // end namespace itk

#endif
