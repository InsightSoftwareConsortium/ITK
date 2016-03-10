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

#ifndef itkFFTConvolutionImageFilter_hxx
#define itkFFTConvolutionImageFilter_hxx

#include "itkFFTConvolutionImageFilter.h"

#include "itkCastImageFilter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkConstantPadImageFilter.h"
#include "itkCyclicShiftImageFilter.h"
#include "itkExtractImageFilter.h"
#include "itkImageBase.h"
#include "itkMultiplyImageFilter.h"
#include "itkNormalizeToConstantImageFilter.h"
#include "itkMath.h"

namespace itk
{

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::FFTConvolutionImageFilter()
{
  m_SizeGreatestPrimeFactor = FFTFilterType::New()->GetSizeGreatestPrimeFactor();
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
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

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter( this );

  typename InputImageType::Pointer localInput = InputImageType::New();
  localInput->Graft( this->GetInput() );

  const KernelImageType* kernelImage = this->GetKernelImage();

  InternalComplexImagePointerType input = ITK_NULLPTR;
  InternalComplexImagePointerType kernel = ITK_NULLPTR;
  this->PrepareInputs( localInput, kernelImage, input, kernel, progress, 0.7f );

  typedef MultiplyImageFilter< InternalComplexImageType,
                               InternalComplexImageType,
                               InternalComplexImageType > MultiplyFilterType;
  typename MultiplyFilterType::Pointer multiplyFilter = MultiplyFilterType::New();
  multiplyFilter->SetInput1( input );
  multiplyFilter->SetInput2( kernel );
  multiplyFilter->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( multiplyFilter, 0.1 );

  // Free up the memory for the prepared inputs
  input = ITK_NULLPTR;
  kernel = ITK_NULLPTR;

  this->ProduceOutput( multiplyFilter->GetOutput(), progress, 0.2 );
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::PrepareInputs(const InputImageType * input,
                const KernelImageType * kernel,
                InternalComplexImagePointerType & preparedInput,
                InternalComplexImagePointerType & preparedKernel,
                ProgressAccumulator * progress, float progressWeight)
{
  this->PrepareInput( input, preparedInput,  progress, 0.5f * progressWeight );
  this->PrepareKernel( kernel, preparedKernel, progress, 0.5f * progressWeight );
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::PrepareInput(const InputImageType * input,
               InternalComplexImagePointerType & preparedInput,
               ProgressAccumulator * progress,
               float progressWeight)
{
  InternalImagePointerType paddedInput;
  this->PadInput( input, paddedInput, progress, 0.3f * progressWeight );
  this->TransformPaddedInput( paddedInput, preparedInput, progress,
                              0.7f * progressWeight );
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::PadInput(const InputImageType * input,
           InternalImagePointerType & paddedInput,
           ProgressAccumulator * progress, float progressWeight)
{
  // Pad the image
  InputSizeType padSize = this->GetPadSize();
  InputRegionType inputRegion = input->GetLargestPossibleRegion();
  InputSizeType inputSize = inputRegion.GetSize();

  typedef PadImageFilter< InputImageType, InputImageType > InputPadFilterType;
  typename InputPadFilterType::Pointer inputPadder = InputPadFilterType::New();
  inputPadder->SetBoundaryCondition( this->GetBoundaryCondition() );

  InputSizeType inputLowerBound = this->GetPadLowerBound();
  inputPadder->SetPadLowerBound( inputLowerBound );

  InputSizeType inputUpperBound;
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
    inputUpperBound[i] = (padSize[i] - inputSize[i]) / 2;
    if ( ( padSize[i] - inputSize[i] ) % 2 == 1 )
      {
      inputUpperBound[i]++;
      }
    }
  inputPadder->SetPadUpperBound( inputUpperBound );
  inputPadder->SetNumberOfThreads( this->GetNumberOfThreads() );
  inputPadder->SetInput( input );
  inputPadder->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( inputPadder, 0.5f * progressWeight );

  // We could avoid a separate cast here by setting the output type of
  // the padder to the InternalImageType, but doing so complicates the
  // definition of the boundary condition passed into this class and
  // requires the InternalImageType to be exposed publicly.
  typedef CastImageFilter< InputImageType, InternalImageType > InputCastFilterType;
  typename InputCastFilterType::Pointer inputCaster = InputCastFilterType::New();
  // See if we can avoid unnecessary casting and copying of memory
  inputCaster->InPlaceOn();
  inputCaster->SetNumberOfThreads( this->GetNumberOfThreads() );
  inputCaster->SetInput( inputPadder->GetOutput() );
  inputCaster->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( inputCaster, 0.5f * progressWeight );
  inputCaster->Update();

  paddedInput = inputCaster->GetOutput();
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::TransformPaddedInput(const InternalImageType * paddedInput,
                       InternalComplexImagePointerType & transformedInput,
                       ProgressAccumulator * progress, float progressWeight)
{
  // Take the Fourier transform of the padded image.
  typename FFTFilterType::Pointer imageFFTFilter = FFTFilterType::New();
  imageFFTFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  imageFFTFilter->SetInput( paddedInput );
  imageFFTFilter->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( imageFFTFilter, progressWeight );
  imageFFTFilter->Update();

  transformedInput = imageFFTFilter->GetOutput();
  transformedInput->DisconnectPipeline();

  imageFFTFilter->SetInput( ITK_NULLPTR );
  imageFFTFilter = ITK_NULLPTR;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::PrepareKernel(const KernelImageType * kernel,
                InternalComplexImagePointerType & preparedKernel,
                ProgressAccumulator * progress, float progressWeight)
{
  KernelRegionType kernelRegion = kernel->GetLargestPossibleRegion();
  KernelSizeType kernelSize = kernelRegion.GetSize();

  InputSizeType padSize = this->GetPadSize();
  typename KernelImageType::SizeType kernelUpperBound;
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
    kernelUpperBound[i] = padSize[i] - kernelSize[i];
    }

  InternalImagePointerType paddedKernelImage = ITK_NULLPTR;

  float paddingWeight = 0.2f;
  if ( this->GetNormalize() )
    {
    typedef NormalizeToConstantImageFilter< KernelImageType, InternalImageType >
      NormalizeFilterType;
    typename NormalizeFilterType::Pointer normalizeFilter = NormalizeFilterType::New();
    normalizeFilter->SetConstant( NumericTraits< TInternalPrecision >::OneValue() );
    normalizeFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
    normalizeFilter->SetInput( kernel );
    normalizeFilter->ReleaseDataFlagOn();
    progress->RegisterInternalFilter( normalizeFilter,
                                      0.2f * paddingWeight * progressWeight );

    // Pad the kernel image with zeros.
    typedef ConstantPadImageFilter< InternalImageType, InternalImageType > KernelPadType;
    typedef typename KernelPadType::Pointer                                KernelPadPointer;
    KernelPadPointer kernelPadder = KernelPadType::New();
    kernelPadder->SetConstant( NumericTraits< TInternalPrecision >::ZeroValue() );
    kernelPadder->SetPadUpperBound( kernelUpperBound );
    kernelPadder->SetNumberOfThreads( this->GetNumberOfThreads() );
    kernelPadder->SetInput( normalizeFilter->GetOutput() );
    kernelPadder->ReleaseDataFlagOn();
    progress->RegisterInternalFilter( kernelPadder,
                                      0.8f * paddingWeight * progressWeight );
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
    kernelPadder->SetInput( kernel );
    kernelPadder->ReleaseDataFlagOn();
    progress->RegisterInternalFilter( kernelPadder,
                                      paddingWeight * progressWeight );
    paddedKernelImage = kernelPadder->GetOutput();
    }

  // Shift the padded kernel image.
  typedef CyclicShiftImageFilter< InternalImageType, InternalImageType > KernelShiftFilterType;
  typename KernelShiftFilterType::Pointer kernelShifter = KernelShiftFilterType::New();
  typename KernelShiftFilterType::OffsetType kernelShift;
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
    kernelShift[i] = -(static_cast<typename KernelShiftFilterType::OffsetType::OffsetValueType>(kernelSize[i]/2));
    }
  kernelShifter->SetShift( kernelShift );
  kernelShifter->SetNumberOfThreads( this->GetNumberOfThreads() );
  kernelShifter->SetInput( paddedKernelImage );
  kernelShifter->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( kernelShifter, 0.1f * progressWeight );

  typename FFTFilterType::Pointer kernelFFTFilter = FFTFilterType::New();
  kernelFFTFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  kernelFFTFilter->SetInput( kernelShifter->GetOutput() );
  progress->RegisterInternalFilter( kernelFFTFilter, 0.699f * progressWeight );
  kernelFFTFilter->Update();

  typedef ChangeInformationImageFilter< InternalComplexImageType > InfoFilterType;
  typename InfoFilterType::Pointer kernelInfoFilter = InfoFilterType::New();
  kernelInfoFilter->ChangeRegionOn();

  typedef typename InfoFilterType::OutputImageOffsetValueType InfoOffsetValueType;
  const InputSizeType & inputLowerBound = this->GetPadLowerBound();
  const InputIndexType & inputIndex = this->GetInput()->GetLargestPossibleRegion().GetIndex();
  const KernelIndexType & kernelIndex = kernel->GetLargestPossibleRegion().GetIndex();
  InfoOffsetValueType kernelOffset[ImageDimension];
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
    kernelOffset[i] = static_cast< InfoOffsetValueType >( inputIndex[i] - inputLowerBound[i] - kernelIndex[i] );
    }
  kernelInfoFilter->SetOutputOffset( kernelOffset );
  kernelInfoFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  kernelInfoFilter->SetInput( kernelFFTFilter->GetOutput() );
  progress->RegisterInternalFilter( kernelInfoFilter, 0.001f * progressWeight );
  kernelInfoFilter->Update();

  preparedKernel = kernelInfoFilter->GetOutput();
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::ProduceOutput(InternalComplexImageType * paddedOutput,
                ProgressAccumulator * progress,
                float progressWeight)
{
  typename IFFTFilterType::Pointer ifftFilter = IFFTFilterType::New();
  ifftFilter->SetActualXDimensionIsOdd( this->GetXDimensionIsOdd() );
  ifftFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  ifftFilter->SetInput( paddedOutput );
  ifftFilter->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( ifftFilter, 0.6f * progressWeight );

  this->CropOutput( ifftFilter->GetOutput(), progress, 0.4f * progressWeight );
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::CropOutput(InternalImageType * paddedOutput,
             ProgressAccumulator * progress,
             float progressWeight)
{
  // Allocate the output
  this->AllocateOutputs();

  // Now crop the output to the desired size.
  typedef ExtractImageFilter< InternalImageType, OutputImageType > ExtractFilterType;

  typename ExtractFilterType::Pointer extractFilter = ExtractFilterType::New();
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
  extractFilter->SetInput( paddedOutput );
  extractFilter->GetOutput()->SetRequestedRegion( this->GetOutput()->GetRequestedRegion() );
  progress->RegisterInternalFilter( extractFilter, progressWeight );
  extractFilter->Update();

  OutputImageType *extractedImage = extractFilter->GetOutput();
  OutputImageType *output = this->GetOutput();

  // Only manually copy the buffer via the pixel container.
  // The output meta-data of the extract filter is not correct and
  // different that the GenerateOutputInformation method. So just copy
  // the buffer.
  output->SetBufferedRegion(extractedImage->GetBufferedRegion());
  output->SetPixelContainer(extractedImage->GetPixelContainer());

}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
typename FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >::InputSizeType
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::GetPadLowerBound() const
{
  typename InputImageType::ConstPointer inputImage = this->GetInput();
  InputSizeType inputSize = inputImage->GetLargestPossibleRegion().GetSize();
  InputSizeType padSize = this->GetPadSize();

  InputSizeType inputLowerBound;
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
    inputLowerBound[i] = (padSize[i] - inputSize[i]) / 2;
    }

  return inputLowerBound;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
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
    if( m_SizeGreatestPrimeFactor > 1 )
      {
      while ( Math::GreatestPrimeFactor( padSize[i] ) > m_SizeGreatestPrimeFactor )
        {
        padSize[i]++;
        }
      }
    }

  return padSize;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
bool
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::GetXDimensionIsOdd() const
{
  InputSizeType padSize = this->GetPadSize();
  return (padSize[0] % 2 != 0);
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "SizeGreatestPrimeFactor: " << m_SizeGreatestPrimeFactor << std::endl;
}

}
#endif
