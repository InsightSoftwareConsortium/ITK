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

#ifndef __itkFFTConvolutionImageFilter_hxx
#define __itkFFTConvolutionImageFilter_hxx

#include "itkFFTConvolutionImageFilter.h"

#include "itkChangeInformationImageFilter.h"
#include "itkConstantPadImageFilter.h"
#include "itkCyclicShiftImageFilter.h"
#include "itkExtractImageFilter.h"
#include "itkHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkImageBase.h"
#include "itkMultiplyImageFilter.h"
#include "itkNormalizeToConstantImageFilter.h"
#include "itkRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkVnlFFTCommon.h"

namespace itk
{

template< class TInputImage, class TKernelImage, class TOutputImage, class TInternalPrecision >
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::FFTConvolutionImageFilter()
{
}

template< class TInputImage, class TKernelImage, class TOutputImage, class TInternalPrecision >
void
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::GenerateInputRequestedRegion()
{
  // Request the largest possible region for both input images.
  if ( this->GetInput() )
    {
    typename InputImageType::Pointer imagePtr =
      const_cast< InputImageType * >( this->GetInput() );
    imagePtr->SetRequestedRegionToLargestPossibleRegion();
    }

  if ( this->GetKernelImage() )
    {
    // Input kernel is an image, cast away the constness so we can set
    // the requested region.
    typename KernelImageType::Pointer kernelPtr =
      const_cast< KernelImageType * >( this->GetKernelImage() );
    kernelPtr->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< class TInputImage, class TKernelImage, class TOutputImage, class TInternalPrecision >
void
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter( this );

  InternalComplexImagePointerType input = NULL;
  InternalComplexImagePointerType kernel = NULL;

  this->PrepareInputs( input, kernel, progress, 0.7 );

  typedef MultiplyImageFilter< InternalComplexImageType,
                               InternalComplexImageType,
                               InternalComplexImageType > MultiplyFilterType;
  typename MultiplyFilterType::Pointer multiplyFilter = MultiplyFilterType::New();
  multiplyFilter->SetInput1( input );
  multiplyFilter->SetInput2( kernel );
  multiplyFilter->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( multiplyFilter, 0.1 );

  // Free up the memory for the prepared inputs
  input = NULL;
  kernel = NULL;

  this->ProduceOutput( multiplyFilter->GetOutput(), progress, 0.2 );
}

template< class TInputImage, class TKernelImage, class TOutputImage, class TInternalPrecision >
void
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::PrepareInputs(InternalComplexImagePointerType & preparedInput,
                InternalComplexImagePointerType & preparedKernel,
                ProgressAccumulator * progress,
                float progressWeight)
{
  const KernelImageType* kernelImage = this->GetKernelImage();
  KernelRegionType kernelRegion = kernelImage->GetLargestPossibleRegion();
  KernelSizeType kernelSize = kernelRegion.GetSize();

  // Calculate the required pad size.
  InputSizeType padSize = this->GetPadSize();
  typename KernelImageType::SizeType kernelUpperBound;
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
    kernelUpperBound[i] = padSize[i] - kernelSize[i];
    }

  InternalImagePointerType paddedKernelImage = NULL;

  if ( this->GetNormalize() )
    {
    typedef NormalizeToConstantImageFilter< KernelImageType, InternalImageType >
      NormalizeFilterType;
    typename NormalizeFilterType::Pointer normalizeFilter = NormalizeFilterType::New();
    normalizeFilter->SetConstant( NumericTraits< TInternalPrecision >::One );
    normalizeFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
    normalizeFilter->SetInput( this->GetKernelImage() );
    normalizeFilter->ReleaseDataFlagOn();
    progress->RegisterInternalFilter( normalizeFilter, 0.05f * progressWeight );
    progressWeight = 0.95f * progressWeight;

    // Pad the kernel image with zeros.
    typedef ConstantPadImageFilter< InternalImageType, InternalImageType > KernelPadType;
    typedef typename KernelPadType::Pointer                                KernelPadPointer;
    KernelPadPointer kernelPadder = KernelPadType::New();
    kernelPadder->SetConstant( NumericTraits< TInternalPrecision >::ZeroValue() );
    kernelPadder->SetPadUpperBound( kernelUpperBound );
    kernelPadder->SetNumberOfThreads( this->GetNumberOfThreads() );
    kernelPadder->SetInput( normalizeFilter->GetOutput() );
    kernelPadder->ReleaseDataFlagOn();
    progress->RegisterInternalFilter( kernelPadder, 0.1f * progressWeight );
    paddedKernelImage = kernelPadder->GetOutput();
    }
  else
    {
    // Pad the kernel image with zeros.
    typedef ConstantPadImageFilter< KernelImageType, InternalImageType > KernelPadType;
    typedef typename KernelPadType::Pointer                              KernelPadPointer;
    KernelPadPointer kernelPadder = KernelPadType::New();
    kernelPadder->SetConstant( NumericTraits< TInternalPrecision >::ZeroValue() );
    kernelPadder->SetPadUpperBound( kernelUpperBound );
    kernelPadder->SetNumberOfThreads( this->GetNumberOfThreads() );
    kernelPadder->SetInput( kernelImage );
    kernelPadder->ReleaseDataFlagOn();
    progress->RegisterInternalFilter( kernelPadder, 0.1f * progressWeight );
    paddedKernelImage = kernelPadder->GetOutput();
    }

  // Shift the padded kernel image.
  typedef CyclicShiftImageFilter< InternalImageType, InternalImageType > KernelShiftFilterType;
  typename KernelShiftFilterType::Pointer kernelShifter = KernelShiftFilterType::New();
  typename KernelShiftFilterType::OffsetType kernelShift;
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
    kernelShift[i] = -(kernelSize[i] / 2);
    }
  kernelShifter->SetShift( kernelShift );
  kernelShifter->SetNumberOfThreads( this->GetNumberOfThreads() );
  kernelShifter->SetInput( paddedKernelImage );
  kernelShifter->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( kernelShifter, 0.1f * progressWeight );

  // Pad the image
  typename InputImageType::Pointer localInput = InputImageType::New();
  localInput->Graft( this->GetInput() );

  InputRegionType inputRegion = localInput->GetLargestPossibleRegion();
  InputSizeType inputSize = inputRegion.GetSize();

  typedef PadImageFilter< InputImageType, InternalImageType > InputPadFilterType;
  typename InputPadFilterType::Pointer inputPadder = InputPadFilterType::New();
  inputPadder->SetBoundaryCondition( this->GetBoundaryCondition() );
  typename InputPadFilterType::SizeType inputLowerBound;
  typename InputPadFilterType::SizeType inputUpperBound;
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
    inputLowerBound[i] = (padSize[i] - inputSize[i]) / 2;
    inputUpperBound[i] = (padSize[i] - inputSize[i]) / 2;
    if ( ( padSize[i] - inputSize[i] ) % 2 == 1 )
      {
      inputUpperBound[i]++;
      }
    }
  inputPadder->SetPadLowerBound( inputLowerBound );
  inputPadder->SetPadUpperBound( inputUpperBound );
  inputPadder->SetNumberOfThreads( this->GetNumberOfThreads() );
  inputPadder->SetInput( localInput );
  inputPadder->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( inputPadder, 0.099f * progressWeight );

  // Set up the forward and inverse FFT minipipeline.
  typedef RealToHalfHermitianForwardFFTImageFilter< InternalImageType,
                                                    InternalComplexImageType >
    FFTFilterType;
  typename FFTFilterType::Pointer imageFFTFilter = FFTFilterType::New();
  imageFFTFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  imageFFTFilter->SetInput( inputPadder->GetOutput() );
  progress->RegisterInternalFilter( imageFFTFilter, 0.3f * progressWeight );

  preparedInput = imageFFTFilter->GetOutput();

  typename FFTFilterType::Pointer kernelFFTFilter = FFTFilterType::New();
  kernelFFTFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  kernelFFTFilter->SetInput( kernelShifter->GetOutput() );
  progress->RegisterInternalFilter( kernelFFTFilter, 0.3f * progressWeight );

  typedef ChangeInformationImageFilter< InternalComplexImageType > InfoFilterType;
  typename InfoFilterType::Pointer kernelInfoFilter = InfoFilterType::New();
  kernelInfoFilter->SetReferenceImage( imageFFTFilter->GetOutput() );
  kernelInfoFilter->UseReferenceImageOn();
  kernelInfoFilter->ChangeAll();
  kernelInfoFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  kernelInfoFilter->SetInput( kernelFFTFilter->GetOutput() );
  progress->RegisterInternalFilter( kernelInfoFilter, 0.001f * progressWeight );

  preparedKernel = kernelInfoFilter->GetOutput();
}

template< class TInputImage, class TKernelImage, class TOutputImage, class TInternalPrecision >
void
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::ProduceOutput(InternalComplexImageType * paddedOutput,
                ProgressAccumulator * progress,
                float progressWeight)
{
  typedef HalfHermitianToRealInverseFFTImageFilter< InternalComplexImageType,
                                                    InternalImageType >
    IFFTFilterType;
  typename IFFTFilterType::Pointer ifftFilter = IFFTFilterType::New();
  ifftFilter->SetActualXDimensionIsOdd( this->GetXDimensionIsOdd() );
  ifftFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  ifftFilter->SetInput( paddedOutput );
  ifftFilter->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( ifftFilter, progressWeight );

  // Allocate the output
  this->AllocateOutputs();

  // Now crop the output to the desired size.
  typedef ExtractImageFilter< InternalImageType, OutputImageType > ExtractFilterType;

  typename ExtractFilterType::Pointer extractFilter = ExtractFilterType::New();
  extractFilter->SetDirectionCollapseToIdentity();
  extractFilter->InPlaceOn();
  extractFilter->GraftOutput( this->GetOutput() );

  // Set up the crop sizes.
  if ( this->GetOutputRegionMode() == Self::SAME )
    {
    InputRegionType sameRegion = this->GetInput()->GetLargestPossibleRegion();
    extractFilter->SetExtractionRegion( sameRegion );
    }
  else // OutputRegionMode == Self::VALID
    {
    extractFilter->SetExtractionRegion( this->GetValidRegion() );
    }

  // Graft the minipipeline output to this filter.
  extractFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  extractFilter->SetInput( ifftFilter->GetOutput() );
  extractFilter->GetOutput()->SetRequestedRegion( this->GetOutput()->GetRequestedRegion() );
  extractFilter->Update();

  // Graft the output of the crop filter back onto this
  // filter's output.
  this->GraftOutput( extractFilter->GetOutput() );
}

template< class TInputImage, class TKernelImage, class TOutputImage, class TInternalPrecision >
typename FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >::InputSizeType
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::GetPadSize() const
{
  typename InputImageType::ConstPointer inputImage = this->GetInput();
  InputSizeType inputSize = inputImage->GetLargestPossibleRegion().GetSize();
  typename KernelImageType::ConstPointer kernelImage = this->GetKernelImage();
  KernelSizeType kernelSize = kernelImage->GetLargestPossibleRegion().GetSize();

  InputSizeType padSize;
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
    padSize[i] = inputSize[i] + kernelSize[i];
    // Use the valid sizes for VNL because they are fast sizes for
    // both VNL and FFTW.
    while ( !VnlFFTCommon::IsDimensionSizeLegal( padSize[i] ) )
      {
      padSize[i]++;
      }
    }

  return padSize;
}

template< class TInputImage, class TKernelImage, class TOutputImage, class TInternalPrecision >
bool
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::GetXDimensionIsOdd() const
{
  InputSizeType padSize = this->GetPadSize();
  return (padSize[0] % 2 != 0);
}
}
#endif
