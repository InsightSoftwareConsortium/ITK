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

#ifndef __itkConvolutionImageFilter_txx
#define __itkConvolutionImageFilter_txx

#include "itkConvolutionImageFilter.h"

#include "itkConstantPadImageFilter.h"
#include "itkImageBase.h"
#include "itkImageKernelOperator.h"
#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkNormalizeToConstantImageFilter.h"


/*
 *
 * This code was contributed in the Insight Journal paper:
 *
 * "Image Kernel Convolution"
 * by Tustison N., Gee J.
 * http://hdl.handle.net/1926/1323
 * http://www.insight-journal.org/browse/publication/208
 *
 */

namespace itk
{
template< class TInputImage, class TOutputImage >
ConvolutionImageFilter< TInputImage, TOutputImage >
::ConvolutionImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  m_Normalize = false;
}

template< class TInputImage, class TOutputImage >
ConvolutionImageFilter< TInputImage, TOutputImage >
::~ConvolutionImageFilter()
{}

template< class TInputImage, class TOutputImage >
void
ConvolutionImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter( this );

  // Build a mini-pipeline that involves a
  // NeighborhoodOperatorImageFilter to compute the convolution, a
  // normalization filter for the kernel, and a pad filter for making
  // the kernel an odd size.
  if ( m_Normalize )
    {
    typedef typename NumericTraits< InputPixelType >::RealType RealPixelType;
    typedef Image< RealPixelType, ImageDimension >             RealImageType;

    typedef NormalizeToConstantImageFilter< InputImageType, RealImageType > NormalizeFilterType;
    typename NormalizeFilterType::Pointer normalizeFilter = NormalizeFilterType::New();
    normalizeFilter->SetConstant( NumericTraits< RealPixelType >::One );
    normalizeFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
    normalizeFilter->SetInput( this->GetImageKernelInput() );
    normalizeFilter->ReleaseDataFlagOn();
    progress->RegisterInternalFilter( normalizeFilter, 0.1f );
    normalizeFilter->UpdateLargestPossibleRegion();

    this->ComputeConvolution( normalizeFilter->GetOutput(), progress );
    }
  else
    {
    this->ComputeConvolution( this->GetImageKernelInput(), progress );
    }
}

template< class TInputImage, class TOutputImage >
template< class TKernelImage >
void
ConvolutionImageFilter< TInputImage, TOutputImage >
::ComputeConvolution( const TKernelImage * kernelImage,
                      ProgressAccumulator * progress )
{
  typedef typename TKernelImage::PixelType KernelPixelType;
  typedef ImageKernelOperator< KernelPixelType, ImageDimension > KernelOperatorType;
  KernelOperatorType kernelOperator;

  if ( this->GetKernelNeedsPadding() )
    {
    // Pad the kernel if necessary to an odd size in each dimension.
    typedef ConstantPadImageFilter< TKernelImage, TKernelImage >
      PadImageFilterType;
    typename PadImageFilterType::Pointer kernelPadImageFilter =
      PadImageFilterType::New();
    kernelPadImageFilter->SetConstant
      ( NumericTraits< KernelPixelType >::ZeroValue() );
    kernelPadImageFilter->SetPadLowerBound( this->GetKernelPadSize() );
    kernelPadImageFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
    kernelPadImageFilter->SetInput( kernelImage );
    progress->RegisterInternalFilter( kernelPadImageFilter, 0.1f );
    kernelPadImageFilter->UpdateLargestPossibleRegion();

    kernelOperator.SetImageKernel( kernelPadImageFilter->GetOutput() );
    }
  else
    {
    kernelOperator.SetImageKernel( kernelImage );
    }

  // Compute the kernel radius. We can compute this from the input
  // kernel image instead of the padded image because the outcome will
  // be the same.
  typedef typename KernelOperatorType::SizeType SizeType;
  typedef typename SizeType::SizeValueType      SizeValueType;
  SizeType radius;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    radius[i] = kernelImage->GetLargestPossibleRegion().GetSize()[i] / 2;
    }

  kernelOperator.CreateToRadius( radius );

  typedef NeighborhoodOperatorImageFilter< InputImageType, OutputImageType, KernelPixelType >
    ConvolutionFilterType;
  typename ConvolutionFilterType::Pointer convolutionFilter = ConvolutionFilterType::New();
  convolutionFilter->SetOperator( kernelOperator );
  convolutionFilter->SetInput( this->GetInput() );
  convolutionFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter( convolutionFilter, 0.8f );

  // Graft the minipipeline output to this filter.
  convolutionFilter->GraftOutput( this->GetOutput() );
  convolutionFilter->Update();

  // Graft the output of the convolution filter back onto this
  // filter's output.
  this->GraftOutput( convolutionFilter->GetOutput() );
}

template< class TInputImage, class TOutputImage >
bool
ConvolutionImageFilter< TInputImage, TOutputImage >
::GetKernelNeedsPadding() const
{
  const InputImageType *kernel = this->GetImageKernelInput();
  InputRegionType kernelRegion = kernel->GetLargestPossibleRegion();
  InputSizeType kernelSize = kernelRegion.GetSize();

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( kernelSize[i] % 2 == 0 ) // Check if dimension is even
      {
      return true;
      }
    }

  return false;
}

template< class TInputImage, class TOutputImage >
typename ConvolutionImageFilter< TInputImage, TOutputImage >::InputSizeType
ConvolutionImageFilter< TInputImage, TOutputImage >
::GetKernelPadSize() const
{
  const InputImageType *kernel = this->GetImageKernelInput();
  InputRegionType kernelRegion = kernel->GetLargestPossibleRegion();
  InputSizeType kernelSize = kernelRegion.GetSize();
  InputSizeType padSize;

  for ( unsigned int i = 0; i < ImageDimension; i++)
    {
    // Pad by 1 if the size fo the image in this dimension is even.
    padSize[i] = 1 - (kernelSize[i] % 2);
    }

  return padSize;
}

/**
 * ConvolutionImageFilter needs a smaller 2nd input (the image kernel)
 * requested region than output requested region.  As such,  this filter
 * needs to provide an implementation for GenerateInputRequestedRegion() in
 * order to inform the pipeline execution model.
 *
 * \sa ProcessObject::GenerateInputRequestedRegion()
 */
template< class TInputImage, class TOutputImage >
void
ConvolutionImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // Simply copy the GenerateInputRequestedRegion() function and
  // deal with the image kernel as a special case.
  for ( unsigned int idx = 0; idx < 2; ++idx )
    {
    if ( this->GetInput(idx) )
      {
      // Check whether the input is an image of the appropriate
      // dimension (use ProcessObject's version of the GetInput()
      // method since it returns the input as a pointer to a
      // DataObject as opposed to the subclass version which
      // static_casts the input to an TInputImage).
      typedef ImageBase< ImageDimension > ImageBaseType;
      typename ImageBaseType::ConstPointer constInput =
        dynamic_cast< ImageBaseType const * >(
          this->ProcessObject::GetInput(idx) );

      if ( constInput.IsNull() )
        {
        itkExceptionMacro("Input image " << idx
                                         << " not correctly specified.");
        }

      // Input is an image, cast away the constness so we can set
      // the requested region.
      typename InputImageType::Pointer input =
        const_cast< TInputImage * >( this->GetInput(idx) );

      typename InputImageType::RegionType inputRegion;
      if ( idx == 0 )
        {
        Superclass::CallCopyOutputRegionToInputRegion( inputRegion,
                                                       this->GetOutput()->GetRequestedRegion() );
        }
      else  // the input is the image kernel
        {
        typename InputImageType::RegionType::SizeType  inputSize;
        typename InputImageType::RegionType::IndexType inputIndex;
        inputSize = this->GetInput( idx )->GetLargestPossibleRegion().GetSize();
        inputIndex = this->GetInput( idx )->GetLargestPossibleRegion().GetIndex();
        inputRegion.SetSize( inputSize );
        inputRegion.SetIndex( inputIndex );
        }
      input->SetRequestedRegion( inputRegion );
      }
    }
}

template< class TInputImage, class TOutputImage >
void
ConvolutionImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Normalize: "  << m_Normalize << std::endl;
}
}
#endif
