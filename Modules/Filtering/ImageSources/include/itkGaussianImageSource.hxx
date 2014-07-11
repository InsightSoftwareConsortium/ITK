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
#ifndef __itkGaussianImageSource_hxx
#define __itkGaussianImageSource_hxx

#include "itkGaussianImageSource.h"
#include "itkGaussianSpatialFunction.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"
#include "itkObjectFactory.h"

namespace itk
{
//----------------------------------------------------------------------------
template< typename TOutputImage >
GaussianImageSource< TOutputImage >
::GaussianImageSource()
{
  // Gaussian parameters, defined so that the gaussian
  // is centered in the default image
  m_Mean.Fill(32.0);
  m_Sigma.Fill(16.0);
  m_Scale = 255.0;

  m_Normalized = false;
}

//----------------------------------------------------------------------------
template< typename TOutputImage >
void
GaussianImageSource< TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Gaussian sigma: [";
  for ( unsigned int ii = 0; ii < NDimensions; ++ii )
    {
    os << m_Sigma[ii];
    if( ii != NDimensions - 1 )
      {
      os << ", ";
      }
    }
  os << "]" << std::endl;

  os << indent << "Gaussian mean: [";
  for ( unsigned int ii = 0; ii < NDimensions; ++ii )
    {
    os << m_Mean[ii];
    if( ii != NDimensions - 1 )
      {
      os << ", ";
      }
    }
  os << "]" << std::endl;

  os << indent << "Gaussian scale: " << m_Scale << std::endl;
  os << indent << "Normalized Gaussian?: " << m_Normalized << std::endl;
}

//----------------------------------------------------------------------------
template< typename TOutputImage >
void
GaussianImageSource< TOutputImage >
::SetParameters(const ParametersType & parameters)
{
  ArrayType sigma, mean;
  for ( unsigned int i = 0; i < ArrayType::Length; i++ )
    {
    sigma[i] = parameters[i];
    mean[i]  = parameters[i + ArrayType::Length];
    }
  this->SetSigma( sigma );
  this->SetMean( mean );

  double scale = parameters[2*ArrayType::Length];
  this->SetScale( scale );
}

//----------------------------------------------------------------------------
template< typename TOutputImage >
typename GaussianImageSource< TOutputImage >::ParametersType
GaussianImageSource< TOutputImage >
::GetParameters() const
{
  ParametersType parameters( 2*ArrayType::Length + 1 );
  for ( unsigned int i = 0; i < ArrayType::Length; i++ )
    {
    parameters[i] = m_Sigma[i];
    parameters[i + ArrayType::Length] = m_Mean[i];
    }
  parameters[2*ArrayType::Length] = m_Scale;

  return parameters;
}

//----------------------------------------------------------------------------
template< typename TOutputImage >
unsigned int
GaussianImageSource< TOutputImage >
::GetNumberOfParameters() const
{
  return 2*ArrayType::Length + 1;
}

//----------------------------------------------------------------------------
template< typename TOutputImage >
void
GaussianImageSource< TOutputImage >
::GenerateData()
{
  TOutputImage * outputPtr = this->GetOutput();

  // allocate the output buffer
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Create and initialize a new gaussian function
  typedef itk::GaussianSpatialFunction< double, NDimensions > FunctionType;
  typename FunctionType::Pointer pGaussian = FunctionType::New();

  pGaussian->SetSigma(m_Sigma);
  pGaussian->SetMean(m_Mean);
  pGaussian->SetScale(m_Scale);
  pGaussian->SetNormalized(m_Normalized);

  // Create an iterator that will walk the output region
  typedef ImageRegionIterator< TOutputImage > OutputIterator;
  OutputIterator outIt = OutputIterator( outputPtr,
                                         outputPtr->GetRequestedRegion() );

  // The value produced by the spatial function
  double value;

  // The position at which the function is evaluated
  Point< double, TOutputImage::ImageDimension > evalPoint;

  ProgressReporter progress( this, 0,
                             outputPtr->GetRequestedRegion()
                             .GetNumberOfPixels() );
  // Walk the output image, evaluating the spatial function at each pixel
  for (; !outIt.IsAtEnd(); ++outIt )
    {
    typename TOutputImage::IndexType index = outIt.GetIndex();
    outputPtr->TransformIndexToPhysicalPoint(index, evalPoint);
    value = pGaussian->Evaluate(evalPoint);

    // Set the pixel value to the function value
    outIt.Set( ( typename TOutputImage::PixelType )value );
    progress.CompletedPixel();
    }
}

} // end namespace itk

#endif
