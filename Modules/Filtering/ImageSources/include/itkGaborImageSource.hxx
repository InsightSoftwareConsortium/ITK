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
#ifndef __itkGaborImageSource_hxx
#define __itkGaborImageSource_hxx

#include "itkGaborKernelFunction.h"
#include "itkGaborImageSource.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressReporter.h"
#include "itkObjectFactory.h"

namespace itk
{
template< typename TOutputImage >
GaborImageSource< TOutputImage >
::GaborImageSource()
{
  // Gabor parameters, defined so that the gaussian
  // is centered in the default image
  this->m_Mean.Fill(32.0);
  this->m_Sigma.Fill(16.0);

  this->m_CalculateImaginaryPart = false;
  this->m_Frequency = 0.4;
  this->m_PhaseOffset = 0.0;
}

template< typename TOutputImage >
void
GaborImageSource< TOutputImage >
::GenerateData()
{
  OutputImageType* outputPtr = this->GetOutput();

  // allocate the output buffer
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Create and initialize a new gaussian function
  typedef GaborKernelFunction<double> KernelFunctionType;
  typename KernelFunctionType::Pointer gabor = KernelFunctionType::New();

  gabor->SetSigma(this->m_Sigma[0]);
  gabor->SetFrequency(this->m_Frequency);
  gabor->SetPhaseOffset(this->m_PhaseOffset);
  gabor->SetCalculateImaginaryPart(this->m_CalculateImaginaryPart);

  // Create an iterator that will walk the output region
  ImageRegionIteratorWithIndex< OutputImageType >
  outIt( outputPtr, outputPtr->GetRequestedRegion() );

  // The position at which the function is evaluated
  Point< double, ImageDimension > evalPoint;

  ProgressReporter progress( this, 0,
                             outputPtr->GetRequestedRegion().GetNumberOfPixels() );

  // Walk the output image, evaluating the spatial function at each pixel
  for ( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt )
    {
    typename OutputImageType::IndexType index = outIt.GetIndex();
    outputPtr->TransformIndexToPhysicalPoint(index, evalPoint);
    double sum = 0.0;
    for ( unsigned int i = 1; i < ImageDimension; i++ )
      {
      sum += vnl_math_sqr( ( evalPoint[i] - this->m_Mean[i] ) / this->m_Sigma[i] );
      }
    double value = std::exp(-0.5 * sum) * gabor->Evaluate(evalPoint[0] - this->m_Mean[0]);

    // Set the pixel value to the function value
    outIt.Set( static_cast< PixelType >( value ) );
    progress.CompletedPixel();
    }
}

template< typename TOutputImage >
void
GaborImageSource< TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "  Sigma: " << this->GetSigma() << std::endl;
  os << indent << "  Mean: " << this->GetMean() << std::endl;
  os << indent << "  Frequency: " << this->GetFrequency() << std::endl;
  if ( this->GetCalculateImaginaryPart() )
    {
    os << indent << "  Calculate complex part: true " << std::endl;
    }
  else
    {
    os << indent << "  Calculate complex part: false " << std::endl;
    }
}
} // end namespace itk

#endif
