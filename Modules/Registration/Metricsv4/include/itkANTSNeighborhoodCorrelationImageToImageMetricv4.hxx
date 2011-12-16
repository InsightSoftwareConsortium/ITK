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
#ifndef __itkANTSNeighborhoodCorrelationImageToImageMetricv4_hxx
#define __itkANTSNeighborhoodCorrelationImageToImageMetricv4_hxx

#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkNumericTraits.h"

namespace itk
{

template<class TFixedImage, class TMovingImage, class TVirtualImage>
ANTSNeighborhoodCorrelationImageToImageMetricv4<TFixedImage, TMovingImage,TVirtualImage>
::ANTSNeighborhoodCorrelationImageToImageMetricv4()
{
  // initialize radius
  typedef typename RadiusType::SizeValueType RadiusValueType;
  this->m_Radius.Fill( NumericTraits<RadiusValueType>::One );
  // We have our own GetValueAndDerivativeThreader's that we want
  // ImageToImageMetricv4 to use.
  this->m_DenseGetValueAndDerivativeThreader  = ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreaderType::New();
  // not implemented
  //this->m_SparseGetValueAndDerivativeThreader =
}

template<class TFixedImage, class TMovingImage, class TVirtualImage>
ANTSNeighborhoodCorrelationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
::~ANTSNeighborhoodCorrelationImageToImageMetricv4()
{
}

template<class TFixedImage, class TMovingImage, class TVirtualImage>
void
ANTSNeighborhoodCorrelationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
::InitializeScanning( const ImageRegionType &scanRegion, ScanIteratorType &scanIt,
                      ScanMemType & scanMem, ScanParametersType &scanParameters ) const
{
  scanParameters.scanRegion   = scanRegion;
  scanParameters.fixedImage   = this->m_FixedImage;
  scanParameters.movingImage  = this->m_MovingImage;
  scanParameters.virtualImage = this->m_VirtualDomainImage;
  scanParameters.radius       = this->GetRadius();

  OffsetValueType numberOfFillZero = scanParameters.virtualImage->GetBufferedRegion().GetIndex(0)
      - (scanRegion.GetIndex(0) - scanParameters.radius[0]);

  if (numberOfFillZero < NumericTraits<OffsetValueType>::ZeroValue())
    {
    numberOfFillZero = NumericTraits<OffsetValueType>::ZeroValue();
    }

  scanParameters.numberOfFillZero = numberOfFillZero;

  scanIt = ScanIteratorType(scanParameters.radius, scanParameters.fixedImage, scanRegion);
  scanParameters.windowLength = scanIt.Size();
  scanParameters.scanRegionBeginIndexDim0 = scanIt.GetBeginIndex()[0];

  scanMem.fixedA = NumericTraits< QueueRealType >::Zero;
  scanMem.movingA = NumericTraits< QueueRealType >::Zero;
  scanMem.sFixedMoving = NumericTraits< QueueRealType >::Zero;
  scanMem.sFixedFixed = NumericTraits< QueueRealType >::Zero;
  scanMem.sMovingMoving = NumericTraits< QueueRealType >::Zero;

  scanMem.fixedImageGradient.Fill(0.0);
  scanMem.movingImageGradient.Fill(0.0);
  scanMem.mappedMovingPoint.Fill(0.0);
}


template<class TFixedImage, class TMovingImage, class TVirtualImage>
void
ANTSNeighborhoodCorrelationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Correlation window radius: " << m_Radius << std::endl;
}

} // end namespace itk

#endif
