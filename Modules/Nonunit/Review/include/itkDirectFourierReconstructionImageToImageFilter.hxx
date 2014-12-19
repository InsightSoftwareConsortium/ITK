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
#ifndef itkDirectFourierReconstructionImageToImageFilter_hxx
#define itkDirectFourierReconstructionImageToImageFilter_hxx

#include "itkDirectFourierReconstructionImageToImageFilter.h"

namespace itk
{
/**
 * Initialize member variables with meaningful values.
 */
template< typename TInputImage, typename TOutputImage >
DirectFourierReconstructionImageToImageFilter< TInputImage, TOutputImage >::DirectFourierReconstructionImageToImageFilter():
  Superclass()
{
  const double RADIANS = 1.0;

  m_ZeroPadding = 2;
  m_OverSampling = 2;
  m_Cutoff = 1.0;
  m_AlphaRange = 180;

  m_ZDirection = 1;
  m_RDirection = 0;
  m_AlphaDirection = 2;

  m_RadialSplineOrder = 3;

  m_PI = 4 * std::atan(RADIANS);
}

/**
 * Print out class state (member variables)
 */
template< typename TInputImage, typename TOutputImage >
void DirectFourierReconstructionImageToImageFilter< TInputImage, TOutputImage >::PrintSelf(std::ostream & os,
                                                                                                   Indent indent) const
{
  // call the superclass' implementation of this method
  Superclass::PrintSelf(os, indent);

  os << indent << "Zero Padding Factor: "
     << this->GetZeroPadding() << std::endl;
  os << indent << "Fourier Oversampling Factor: "
     << this->GetOverSampling() << std::endl;
  os << indent << "Radial Spline Order: "
     << this->GetRadialSplineOrder() << std::endl;
  os << indent << "Fourier Radial Cutoff Frequency: "
     << this->GetCutoff() << std::endl;
  os << indent << "Alpha Range: "
     << this->GetAlphaRange() << std::endl;
  os << indent << "Z Direction: "
     << this->GetZDirection() << std::endl;
  os << indent << "Alpha Direction: "
     << this->GetAlphaDirection() << std::endl;
  os << indent << "Radial Direction: "
     << this->GetRDirection() << std::endl;
  os << indent << "Input Requested Region: "
     << m_InputRequestedRegion << std::endl;
}

/**
* Calculate image boundaries and define output regions, spacing, origin etc.
*/
template< typename TInputImage, typename TOutputImage >
void DirectFourierReconstructionImageToImageFilter< TInputImage, TOutputImage >::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  ConstInputImagePointer inputImage = this->GetInput();
  OutputImagePointer     outputImage = this->GetOutput();

  if ( !inputImage || !outputImage )
    {
    return;
    }

  RegionType inputRegion = inputImage->GetLargestPossibleRegion();
  IndexType  inputStart = inputRegion.GetIndex();
  SizeType   inputSize = inputRegion.GetSize();

  IndexType outputStart;
  SizeType  outputSize;

  // Conserve axial dimensions
  outputSize[2] = inputSize[m_ZDirection];
  outputStart[2] = inputStart[m_ZDirection];

  // Radial dimensions give in-plane extent
  outputSize[0] = outputSize[1] = inputSize[m_RDirection];
  outputStart[0] = outputStart[1] = 0;

  RegionType outputRegion;
  outputRegion.SetIndex(outputStart);
  outputRegion.SetSize(outputSize);

  outputImage->SetLargestPossibleRegion(outputRegion);

  // Could be elsewhere...
  PointType outputOrigin;
  outputOrigin[0] = outputOrigin[1] = outputOrigin[2] = 0;

  outputImage->SetOrigin(outputOrigin);

  SpacingType inputSpacing = inputImage->GetSpacing();
  SpacingType outputSpacing;
  // Conserve axial spacing
  outputSpacing[2] = inputSpacing[m_ZDirection];
  // in-plane spacing = Radial spacing
  outputSpacing[0] = outputSpacing[1] = inputSpacing[m_RDirection];

  outputImage->SetSpacing(outputSpacing);
}

/**
 * Calculate necessary input image boundaries
 */
template< typename TInputImage, typename TOutputImage >
void DirectFourierReconstructionImageToImageFilter< TInputImage, TOutputImage >::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  OutputImagePointer outputImage = this->GetOutput();
  InputImagePointer  inputImage = const_cast< InputImageType * >( this->GetInput() );
  if ( !inputImage || !outputImage )
    {
    return;
    }

  // request maximum angular and radial information / to be optimized

  SizeType  inputSize = inputImage->GetLargestPossibleRegion().GetSize();
  IndexType inputStart = inputImage->GetLargestPossibleRegion().GetIndex();

  // crop to requested z-axis
  inputSize[m_ZDirection] = outputImage->GetRequestedRegion().GetSize()[2];
  inputStart[m_ZDirection] = outputImage->GetRequestedRegion().GetIndex()[2];

  m_InputRequestedRegion.SetSize(inputSize);
  m_InputRequestedRegion.SetIndex(inputStart);

  m_InputRequestedRegion.Crop( inputImage->GetLargestPossibleRegion() );
  inputImage->SetRequestedRegion(m_InputRequestedRegion);
}

/**
 * Actual computation
 */
template< typename TInputImage, typename TOutputImage >
void DirectFourierReconstructionImageToImageFilter< TInputImage, TOutputImage >::GenerateData()
{
  OutputImagePointer     outputImage = this->GetOutput();
  ConstInputImagePointer inputImage = this->GetInput();

  if ( !inputImage || !outputImage )
    {
    return;
    }

  outputImage->SetBufferedRegion( outputImage->GetRequestedRegion() );
  outputImage->Allocate();

  // Crop angular input image size to 180 degrees
  typename InputImageType::RegionType inputROI = m_InputRequestedRegion;
  typename InputImageType::SizeType inputROISize = inputROI.GetSize();
  typename InputImageType::IndexType inputROIStart = inputROI.GetIndex();

  // the number of projections needed to cover 180 degrees
  const unsigned int alpha_size = Math::Floor< unsigned int >(
    ( 180 * ( inputROISize[m_AlphaDirection] ) ) / m_AlphaRange);
  const double last_alpha_size = 1 + ( 180.0 * ( inputROISize[m_AlphaDirection] ) ) / m_AlphaRange - alpha_size;
  inputROIStart[m_AlphaDirection] += ( inputROISize[m_AlphaDirection] - alpha_size ) / 2;
  inputROISize[m_AlphaDirection] = alpha_size;
  inputROI.SetSize(inputROISize);
  inputROI.SetIndex(inputROIStart);

  // Setup the input ROI iterator
  InputSliceIteratorType inputIt (inputImage, inputROI);
  inputIt.SetFirstDirection(m_RDirection);      // Iterate first along r
  inputIt.SetSecondDirection(m_AlphaDirection); // then alpha (slice), and
                                                // finally z (stack)
  inputIt.GoToBegin();

  // Setup projection line
  ProjectionLineType::Pointer    projectionLine = ProjectionLineType::New();
  ProjectionLineType::RegionType pRegion;
  ProjectionLineType::SizeType   pSize;
  ProjectionLineType::IndexType  pStart;
  pSize[0] = inputROISize[m_RDirection] * m_ZeroPadding;
  pStart[0] = inputROIStart[m_RDirection];
  pRegion.SetSize(pSize);
  pRegion.SetIndex(pStart);
  projectionLine->SetRegions(pRegion);
  projectionLine->Allocate(true); // initialize
                                                         // buffer to zero

  ProjectionLineType::IndexType pIdx;
  const unsigned int            pLineHalfShift = pSize[0] - inputROISize[m_RDirection] / 2;

  // Setup 1D FFT Filter
  FFTLineFilterType::Pointer FFT = FFTLineFilterType::New();
  FFT->SetInput(projectionLine);

  // Setup FFT Line interpolator stack
  FFTLineInterpolatorType::Pointer *FFTLineInterpolator = new FFTLineInterpolatorType::Pointer[alpha_size];
  for ( unsigned int alpha = 0; alpha < alpha_size; alpha++ )
    {
    FFTLineInterpolator[alpha] = FFTLineInterpolatorType::New();
    FFTLineInterpolator[alpha]->SetSplineOrder(m_RadialSplineOrder);
    }

  // Setup cartesian FFTSlice domain
  FFTSliceType::RegionType FFTSliceRegion;
  FFTSliceType::SizeType   FFTSliceSize;
  FFTSliceType::IndexType  FFTSliceStart;

  FFTSliceSize[0] = FFTSliceSize[1] = inputROISize[m_RDirection] * m_ZeroPadding * m_OverSampling;
  FFTSliceStart[0] = FFTSliceStart[1] = 0;
  FFTSliceRegion.SetSize(FFTSliceSize);
  FFTSliceRegion.SetIndex(FFTSliceStart);

  FFTSliceType::Pointer FFTSlice = FFTSliceType::New();
  FFTSlice->SetRegions(FFTSliceRegion);
  FFTSlice->Allocate(true); // initialize
                                                   // buffer to zero

  FFTSliceIteratorType    FFTSliceIt (FFTSlice, FFTSliceRegion);
  FFTSliceType::IndexType sIdx;

  const double r_max = inputROISize[m_RDirection] * m_ZeroPadding * m_Cutoff / 2.0 - 1;
  const double halfR = inputROISize[m_RDirection] * m_ZeroPadding / 2;

  // Only the central frame will be copied to the output
  OutputSliceType::RegionType outputWindow;
  OutputSliceType::SizeType   outputWindowSize;
  OutputSliceType::IndexType  outputWindowStart;

  const unsigned int outputWindowShift = inputROISize[m_RDirection] * ( m_ZeroPadding * m_OverSampling - 1 ) / 2;
  const unsigned int outputWindowHalf = inputROISize[m_RDirection] * m_ZeroPadding * m_OverSampling / 2;

  outputWindowSize[0] = outputImage->GetRequestedRegion().GetSize()[0];
  outputWindowSize[1] = outputImage->GetRequestedRegion().GetSize()[1];
  outputWindowStart[0] = outputImage->GetRequestedRegion().GetIndex()[0] + outputWindowShift;
  outputWindowStart[1] = outputImage->GetRequestedRegion().GetIndex()[1] + outputWindowShift;

  outputWindow.SetSize(outputWindowSize);
  outputWindow.SetIndex(outputWindowStart);

  OutputSliceType::IndexType wIdx;
  typename OutputImageType::IndexType oIdx;

  ProgressReporter progress( this, 0, outputImage->GetRequestedRegion().GetNumberOfPixels() );

  // Start iterating through slices
  while ( !inputIt.IsAtEnd() )
    {
    while ( !inputIt.IsAtEndOfSlice() ) // Start iterating through angles
      {
      while ( !inputIt.IsAtEndOfLine() ) // copy the whole input line
        {
        pIdx[0] = inputIt.GetIndex()[m_RDirection];

        // Shift the pixel to the borders of the image (origin @ 0)
        pIdx[0] += pLineHalfShift;
        pIdx[0] %= pSize[0];

        // Modulate image to shift DC to center of FFT line
        ProjectionLineType::PixelType val =
          static_cast< ProjectionLineType::PixelType >( ( pIdx[0] & 1 ) ? -inputIt.Get() : inputIt.Get() );
        projectionLine->SetPixel(pIdx, val);

        ++inputIt;
        } // while ( !inputIt.IsAtEndOfLine() )

      // Compute FFT
      FFT->Update();

      // link fft line into interpolator stack ...
      FFTLineInterpolator[inputIt.GetIndex()[m_AlphaDirection] - inputROIStart[m_AlphaDirection]]->SetInputImage(
         FFT->GetOutput() );

      // ... and unlink from upstream pipeline
      FFT->GetOutput()->DisconnectPipeline();

      inputIt.NextLine();
      } // while ( !inputIt.IsAtEndOfSlice() )

    double       u, v;
    double       theta, r;
    double       alpha;
    unsigned int a_lo;

    // Resample the cartesian FFT Slice from polar lines
    for ( FFTSliceIt.GoToBegin(); !FFTSliceIt.IsAtEnd(); ++FFTSliceIt )
      {
      sIdx = FFTSliceIt.GetIndex();

      // center on DC
      u = static_cast< double >( sIdx[0] ) - static_cast< double >( FFTSliceSize[0] ) / 2;
      v = static_cast< double >( sIdx[1] ) - static_cast< double >( FFTSliceSize[1] ) / 2;

      // Calculate polar radius
      r = sqrt(u * u + v * v) / m_OverSampling;

      // Radial cutoff frequency
      if ( r >= r_max )
        {
        continue;
        }

      // Get polar angle - and map into [0 PI]
      if ( u == 0.0 && v == 0.0 )
        {
        theta = 0.0;
        }
      else
        {
        theta = std::atan2(v, u);
        }
      if ( theta < 0 )
        {
        theta += m_PI;
        r = -r;
        }

      // Convert into alpha-image indices
      alpha = theta * alpha_size / m_PI;
      if ( alpha >= alpha_size )
        {
        alpha -= alpha_size;
        r = -r;
        }

      FFTLineType::PixelType out;

      // radial BSpline / linear angle interpolation
      a_lo = Math::Floor< unsigned int >(alpha);

      if ( a_lo < alpha_size - 1 ) // no date-line crossing
        {
        // compute angular interpolation weights
        FFTLineType::PixelType w_a_lo( 1 - ( alpha - a_lo ) );
        FFTLineType::PixelType w_a_hi(alpha - a_lo);

        // get radial BSpline interpolations
        FFTLineInterpolatorType::ContinuousIndexType idx;
        idx[0] = r + halfR;
        FFTLineType::PixelType o_lo = FFTLineInterpolator[a_lo]->EvaluateAtContinuousIndex(idx);
        FFTLineType::PixelType o_hi = FFTLineInterpolator[a_lo + 1]->EvaluateAtContinuousIndex(idx);

        out = w_a_lo * o_lo + w_a_hi * o_hi;
        }
      else  // date-line crossing
        {
        // compute angular interpolation weights (bigger last interval)
        FFTLineType::PixelType w_a_lo(1 - ( alpha - a_lo ) / last_alpha_size);
        FFTLineType::PixelType w_a_hi( ( alpha - a_lo ) / last_alpha_size );

        // get radial BSpline interpolations
        FFTLineInterpolatorType::ContinuousIndexType idx;
        idx[0] = r + halfR;
        FFTLineType::PixelType o_lo = FFTLineInterpolator[a_lo]->EvaluateAtContinuousIndex(idx);
        idx[0] = halfR - r;
        FFTLineType::PixelType o_hi = FFTLineInterpolator[0]->EvaluateAtContinuousIndex(idx);

        out = w_a_lo * o_lo + w_a_hi * o_hi;
        }

      FFTSliceIt.Set(out);
      } // for FFTSliceIt

    // Setup inverse 2D FFT Filter
    IFFTSliceFilterType::Pointer IFFT = IFFTSliceFilterType::New();
    IFFT->SetInput(FFTSlice);

    // Calculate the inverse 2D FFT of the slice
    IFFT->Update();

    // Copy desired region into the outputImage

    // Setup output iterator
    OutputSliceIteratorType outputIt(IFFT->GetOutput(), outputWindow);
    // Current z-slice
    oIdx[2] = inputIt.GetIndex()[m_ZDirection];
    for ( outputIt.GoToBegin(); !outputIt.IsAtEnd(); ++outputIt )
      {
      wIdx = outputIt.GetIndex();
      // Write to correct window
      oIdx[0] = wIdx[0] - outputWindowShift;
      oIdx[1] = wIdx[1] - outputWindowShift;
      // Fetch output data from buffer corners
      wIdx[0] += outputWindowHalf;
      wIdx[1] += outputWindowHalf;
      wIdx[0] %= FFTSliceSize[0];
      wIdx[1] %= FFTSliceSize[1];

      // Demodulate the image
      OutputPixelType val = static_cast< OutputPixelType >( IFFT->GetOutput()->GetPixel(wIdx) );
      outputImage->SetPixel(oIdx, ( ( wIdx[0] + wIdx[1] ) & 1 ) ? -val : val);
      progress.CompletedPixel();
      } // for outputIt

    inputIt.NextSlice();
    } // while ( !inputIt.IsAtEnd() )

  delete[] FFTLineInterpolator;
}
} // namespace

#endif
