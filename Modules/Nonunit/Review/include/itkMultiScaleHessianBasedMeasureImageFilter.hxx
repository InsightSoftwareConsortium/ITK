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
#ifndef itkMultiScaleHessianBasedMeasureImageFilter_hxx
#define itkMultiScaleHessianBasedMeasureImageFilter_hxx

#include "itkMultiScaleHessianBasedMeasureImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkMath.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://hdl.handle.net/1926/576
 * http://www.insight-journal.org/browse/publication/175
 *
 */

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage,
          typename THessianImage,
          typename TOutputImage >
MultiScaleHessianBasedMeasureImageFilter
< TInputImage, THessianImage, TOutputImage >
::MultiScaleHessianBasedMeasureImageFilter()
{
  m_NonNegativeHessianBasedMeasure = true;

  m_SigmaMinimum = 0.2;
  m_SigmaMaximum = 2.0;

  m_NumberOfSigmaSteps = 10;
  m_SigmaStepMethod = Self::LogarithmicSigmaSteps;

  m_HessianFilter = HessianFilterType::New();
  m_HessianToMeasureFilter = ITK_NULLPTR;

  //Instantiate Update buffer
  m_UpdateBuffer = UpdateBufferType::New();

  m_GenerateScalesOutput = false;
  m_GenerateHessianOutput = false;

  typename ScalesImageType::Pointer scalesImage = ScalesImageType::New();
  typename HessianImageType::Pointer hessianImage = HessianImageType::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(3);
  this->ProcessObject::SetNthOutput( 1, scalesImage.GetPointer() );
  this->ProcessObject::SetNthOutput( 2, hessianImage.GetPointer() );
}

template< typename TInputImage,
          typename THessianImage,
          typename TOutputImage >
void
MultiScaleHessianBasedMeasureImageFilter
< TInputImage, THessianImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  // currently this filter can not stream so we must set the outputs
  // requested region to the largest
  output->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage,
          typename THessianImage,
          typename TOutputImage >
typename MultiScaleHessianBasedMeasureImageFilter
< TInputImage, THessianImage, TOutputImage >::DataObjectPointer
MultiScaleHessianBasedMeasureImageFilter
< TInputImage, THessianImage, TOutputImage >
::MakeOutput(DataObjectPointerArraySizeType idx)
{
  if ( idx == 1 )
    {
    return ScalesImageType::New().GetPointer();
    }
  if ( idx == 2 )
    {
    return HessianImageType::New().GetPointer();
    }
  return Superclass::MakeOutput(idx);
}

template< typename TInputImage,
          typename THessianImage,
          typename TOutputImage >
void
MultiScaleHessianBasedMeasureImageFilter
< TInputImage, THessianImage, TOutputImage >
::AllocateUpdateBuffer()
{
  /* The update buffer looks just like the output and holds the best response
     in the  objectness measure */

  typename TOutputImage::Pointer output = this->GetOutput();

  // this copies meta data describing the output such as origin,
  // spacing and the largest region
  m_UpdateBuffer->CopyInformation(output);

  m_UpdateBuffer->SetRequestedRegion( output->GetRequestedRegion() );
  m_UpdateBuffer->SetBufferedRegion( output->GetBufferedRegion() );
  m_UpdateBuffer->Allocate();

  // Update buffer is used for > comparisons so make it really really small,
  // just to be sure. Thanks to Hauke Heibel.
  if ( m_NonNegativeHessianBasedMeasure )
    {
    m_UpdateBuffer->FillBuffer(itk::NumericTraits< BufferValueType >::ZeroValue());
    }
  else
    {
    m_UpdateBuffer->FillBuffer( itk::NumericTraits< BufferValueType >::NonpositiveMin() );
    }
}

template< typename TInputImage,
          typename THessianImage,
          typename TOutputImage >
void
MultiScaleHessianBasedMeasureImageFilter
< TInputImage, THessianImage, TOutputImage >
::GenerateData()
{
  // TODO: Move the allocation to a derived AllocateOutputs method
  // Allocate the output
  this->GetOutput()->SetBufferedRegion( this->GetOutput()->GetRequestedRegion() );
  this->GetOutput()->Allocate();

  if ( m_HessianToMeasureFilter.IsNull() )
    {
    itkExceptionMacro(" HessianToMeasure filter is not set. Use SetHessianToMeasureFilter() ");
    }

  if ( m_GenerateScalesOutput )
    {
    typename ScalesImageType::Pointer scalesImage =
      dynamic_cast< ScalesImageType * >( this->ProcessObject::GetOutput(1) );

    scalesImage->SetBufferedRegion( scalesImage->GetRequestedRegion() );
    scalesImage->Allocate(true); // initialize
                                                        // buffer to zero
    }

  if ( m_GenerateHessianOutput )
    {
    typename HessianImageType::Pointer hessianImage =
      dynamic_cast< HessianImageType * >( this->ProcessObject::GetOutput(2) );

    hessianImage->SetBufferedRegion( hessianImage->GetRequestedRegion() );
    hessianImage->Allocate();
    // SymmetricSecondRankTensor is already filled with zero elements at
    // construction.
    // No strict need of filling the buffer, but we do it explicitly here to
    // make sure.
    typename HessianImageType::PixelType zeroTensor(0.0);
    hessianImage->FillBuffer(zeroTensor);
    }

  // Allocate the buffer
  AllocateUpdateBuffer();

  typename InputImageType::ConstPointer input = this->GetInput();

  this->m_HessianFilter->SetInput(input);

  this->m_HessianFilter->SetNormalizeAcrossScale(true);

  // Create a process accumulator for tracking the progress of this
  // minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // prevent a divide by zero
  if ( m_NumberOfSigmaSteps > 0 )
    {
    progress->RegisterInternalFilter(this->m_HessianFilter, .5 / m_NumberOfSigmaSteps);
    progress->RegisterInternalFilter(this->m_HessianToMeasureFilter, .5 / m_NumberOfSigmaSteps);
    }

  for( unsigned int scaleLevel = 0; scaleLevel < m_NumberOfSigmaSteps; ++scaleLevel )
    {
    const double sigma  = this->ComputeSigmaValue(scaleLevel);

    itkDebugMacro (<< "Computing measure for scale with sigma = " << sigma);

    m_HessianFilter->SetSigma(sigma);

    m_HessianToMeasureFilter->SetInput ( m_HessianFilter->GetOutput() );

    m_HessianToMeasureFilter->Update();

    this->UpdateMaximumResponse(sigma);
    }

  // Write out the best response to the output image
  // we can assume that the meta-data should match between these two
  // image, therefore we iterate over the desired output region
  OutputRegionType                        outputRegion = this->GetOutput()->GetBufferedRegion();
  ImageRegionIterator< UpdateBufferType > it(m_UpdateBuffer, outputRegion);
  it.GoToBegin();

  ImageRegionIterator< TOutputImage > oit(this->GetOutput(), outputRegion);
  oit.GoToBegin();

  while ( !oit.IsAtEnd() )
    {
    oit.Value() = static_cast< OutputPixelType >( it.Get() );
    ++oit;
    ++it;
    }

  // Release data from the update buffer.
  m_UpdateBuffer->ReleaseData();
}

template< typename TInputImage,
          typename THessianImage,
          typename TOutputImage >
void
MultiScaleHessianBasedMeasureImageFilter
< TInputImage, THessianImage, TOutputImage >
::UpdateMaximumResponse(double sigma)
{
  // the meta-data should match between these images, therefore we
  // iterate over the desired output region
  OutputRegionType outputRegion = this->GetOutput()->GetBufferedRegion();

  ImageRegionIterator< UpdateBufferType > oit(m_UpdateBuffer, outputRegion);

  typename ScalesImageType::Pointer scalesImage = static_cast< ScalesImageType * >( this->ProcessObject::GetOutput(1) );
  ImageRegionIterator< ScalesImageType > osit;

  typename HessianImageType::Pointer hessianImage = static_cast< HessianImageType * >( this->ProcessObject::GetOutput(2) );
  ImageRegionIterator< HessianImageType > ohit;

  oit.GoToBegin();
  if ( m_GenerateScalesOutput )
    {
    osit = ImageRegionIterator< ScalesImageType >(scalesImage, outputRegion);
    osit.GoToBegin();
    }
  if ( m_GenerateHessianOutput )
    {
    ohit = ImageRegionIterator< HessianImageType >(hessianImage, outputRegion);
    ohit.GoToBegin();
    }

  typedef typename HessianToMeasureFilterType::OutputImageType HessianToMeasureOutputImageType;

  ImageRegionIterator< HessianToMeasureOutputImageType > it(m_HessianToMeasureFilter->GetOutput(), outputRegion);
  ImageRegionIterator< HessianImageType >                hit(m_HessianFilter->GetOutput(), outputRegion);

  it.GoToBegin();
  hit.GoToBegin();

  while ( !oit.IsAtEnd() )
    {
    if ( oit.Value() < it.Value() )
      {
      oit.Value() = it.Value();
      if ( m_GenerateScalesOutput )
        {
        osit.Value() = static_cast< ScalesPixelType >( sigma );
        }
      if ( m_GenerateHessianOutput )
        {
        ohit.Value() = hit.Value();
        }
      }
    ++oit;
    ++it;
    if ( m_GenerateScalesOutput )
      {
      ++osit;
      }
    if ( m_GenerateHessianOutput )
      {
      ++ohit;
      ++hit;
      }
    }
}

template< typename TInputImage,
          typename THessianImage,
          typename TOutputImage >
double
MultiScaleHessianBasedMeasureImageFilter
< TInputImage, THessianImage, TOutputImage >
::ComputeSigmaValue(int scaleLevel)
{
  double sigmaValue;

  if ( m_NumberOfSigmaSteps < 2 )
    {
    return m_SigmaMinimum;
    }

  switch ( m_SigmaStepMethod )
    {
    case Self::EquispacedSigmaSteps:
      {
      const double stepSize = std::max( 1e-10, ( m_SigmaMaximum - m_SigmaMinimum ) / ( m_NumberOfSigmaSteps - 1 ) );
      sigmaValue = m_SigmaMinimum + stepSize * scaleLevel;
      break;
      }
    case Self::LogarithmicSigmaSteps:
      {
      const double stepSize =
        std::max( 1e-10, ( std::log(m_SigmaMaximum) - std::log(m_SigmaMinimum) ) / ( m_NumberOfSigmaSteps - 1 ) );
      sigmaValue = std::exp(std::log (m_SigmaMinimum) + stepSize * scaleLevel);
      break;
      }
    default:
      throw ExceptionObject(__FILE__, __LINE__, "Invalid SigmaStepMethod.", ITK_LOCATION);
      break;
    }

  return sigmaValue;
}

template< typename TInputImage,
          typename THessianImage,
          typename TOutputImage >
void
MultiScaleHessianBasedMeasureImageFilter
< TInputImage, THessianImage, TOutputImage >
::SetSigmaStepMethodToEquispaced()
{
  this->SetSigmaStepMethod(Self::EquispacedSigmaSteps);
}

template< typename TInputImage,
          typename THessianImage,
          typename TOutputImage >
void
MultiScaleHessianBasedMeasureImageFilter
< TInputImage, THessianImage, TOutputImage >
::SetSigmaStepMethodToLogarithmic()
{
  this->SetSigmaStepMethod(Self::LogarithmicSigmaSteps);
}

/** Get the image containing the Hessian at which each pixel gave the
 * best response */
template< typename TInputImage,
          typename THessianImage,
          typename TOutputImage >
const
typename MultiScaleHessianBasedMeasureImageFilter< TInputImage, THessianImage, TOutputImage >::HessianImageType *
MultiScaleHessianBasedMeasureImageFilter
< TInputImage, THessianImage, TOutputImage >
::GetHessianOutput() const
{
  return static_cast< const HessianImageType * >( this->ProcessObject::GetOutput(2) );
}

/** Get the image containing the scales at which each pixel gave the
 * best response */
template< typename TInputImage,
          typename THessianImage,
          typename TOutputImage >
const
typename MultiScaleHessianBasedMeasureImageFilter< TInputImage, THessianImage, TOutputImage >::ScalesImageType *
MultiScaleHessianBasedMeasureImageFilter
< TInputImage, THessianImage, TOutputImage >
::GetScalesOutput() const
{
  return static_cast< const ScalesImageType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TInputImage,
          typename THessianImage,
          typename TOutputImage >
void
MultiScaleHessianBasedMeasureImageFilter
< TInputImage, THessianImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "SigmaMinimum:  " << m_SigmaMinimum << std::endl;
  os << indent << "SigmaMaximum:  " << m_SigmaMaximum  << std::endl;
  os << indent << "NumberOfSigmaSteps:  " << m_NumberOfSigmaSteps  << std::endl;
  os << indent << "SigmaStepMethod:  " << m_SigmaStepMethod  << std::endl;
  os << indent << "HessianToMeasureFilter: " << m_HessianToMeasureFilter << std::endl;
  os << indent << "NonNegativeHessianBasedMeasure:  " << m_NonNegativeHessianBasedMeasure << std::endl;
  os << indent << "GenerateScalesOutput: " << m_GenerateScalesOutput << std::endl;
  os << indent << "GenerateHessianOutput: " << m_GenerateHessianOutput << std::endl;
}
} // end namespace itk

#endif
