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
#ifndef itkParametricBlindLeastSquaresDeconvolutionImageFilter_hxx
#define itkParametricBlindLeastSquaresDeconvolutionImageFilter_hxx

#include "itkParametricBlindLeastSquaresDeconvolutionImageFilter.h"

#include "itkComplexConjugateImageAdaptor.h"
#include "itkImageDuplicator.h"
#include "itkHalfHermitianToRealInverseFFTImageFilter.h"

namespace itk
{

template< typename TInputImage, typename TKernelImage, typename TOutputImage >
ParametricBlindLeastSquaresDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage >
::ParametricBlindLeastSquaresDeconvolutionImageFilter()
{
  m_Alpha = 0.01;
  m_Beta  = 0.01;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage >
ParametricBlindLeastSquaresDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage >
::~ParametricBlindLeastSquaresDeconvolutionImageFilter()
{
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage >
void
ParametricBlindLeastSquaresDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage >
::SetKernelSource(KernelSourceType * kernelSource)
{
  itkDebugMacro("setting KernelSource to " << kernelSource);
  if ( this->m_KernelSource != kernelSource )
    {
    this->m_KernelSource = kernelSource;
    this->Modified();
    }
  m_KernelSource = kernelSource;

  // The kernel image isn't needed until the Initialize() method.
  // However, it needs to be set here to avoid triggering an exception
  // in the itk::ProcessObject::VerifyPreconditions() method.
  this->SetKernelImage( m_KernelSource->GetOutput() );
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage >
void
ParametricBlindLeastSquaresDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage >
::Initialize(ProgressAccumulator * progress, float progressWeight,
             float iterationProgressWeight)
{
  // Set the kernel, needed by the class to pad the input properly
  m_KernelSource->Update();
  this->SetKernelImage( m_KernelSource->GetOutput() );

  this->Superclass::Initialize( progress, 0.5f * progressWeight,
                                iterationProgressWeight );

  this->PrepareInput( this->GetInput(), m_TransformedInput, progress,
                      0.5f * progressWeight );

  typedef ImageDuplicator< InternalComplexImageType > DuplicatorType;
  typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
  duplicator->SetInputImage( m_TransformedInput );
  duplicator->Update();
  m_TransformedCurrentEstimate = duplicator->GetModifiableOutput();
  m_TransformedCurrentEstimate->DisconnectPipeline();

  // Computes the difference between convolution of estimate with
  // parametric kernel at the current kernel parameters
  m_DifferenceFilter = DifferenceFilterType::New();
  // Transform of current estimate will be set as input 1 and
  // transform of current kernel estimate will be set as input 2 in
  // Iteration()
  m_DifferenceFilter->SetInput3( m_TransformedInput );

  // Computes the updated image estimate
  m_ImageUpdateFilter = ImageUpdateFilterType::New();

}

template< typename TInputImage, typename TKernelImage, typename TOutputImage >
void
ParametricBlindLeastSquaresDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage >
::Iteration(ProgressAccumulator * progress, float /*iterationProgressWeight*/)
{
  // Compute the new padded, shifted, and transformed kernel
  m_KernelSource->UpdateLargestPossibleRegion();
  InternalComplexImagePointerType preparedKernel = ITK_NULLPTR;
  this->PrepareKernel( m_KernelSource->GetOutput(), preparedKernel, progress, 0.0 );

  m_DifferenceFilter->SetInput1( m_TransformedCurrentEstimate );
  m_DifferenceFilter->SetInput2( preparedKernel );
  m_DifferenceFilter->SetInput3( m_TransformedInput );
  m_DifferenceFilter->UpdateLargestPossibleRegion();

  // Compute and apply the update to the current estimate. The update
  // is found by convolving the difference with the flipped
  // kernel. Equivalently, in the Fourier domain, the update is found
  // by multiplying the difference with the conjugate of the current
  // kernel.
  m_ImageUpdateFilter->SetInput1( m_TransformedCurrentEstimate );
  m_ImageUpdateFilter->SetInput2( m_DifferenceFilter->GetOutput() );
  m_ImageUpdateFilter->SetInput3( preparedKernel );
  m_ImageUpdateFilter->GetFunctor().SetAlpha( m_Alpha );
  m_ImageUpdateFilter->UpdateLargestPossibleRegion();

  m_TransformedCurrentEstimate = m_ImageUpdateFilter->GetOutput();
  m_TransformedCurrentEstimate->DisconnectPipeline();

  // Compute the convolution of the difference with the flipped and
  // normalized version of the current image estimate
  typedef HalfHermitianToRealInverseFFTImageFilter< InternalComplexImageType,
                                                    InternalImageType >
    InverseFFTFilterType;
  typename InverseFFTFilterType::Pointer ifft = InverseFFTFilterType::New();
  ifft->SetActualXDimensionIsOdd( this->GetXDimensionIsOdd() );
  ifft->SetInput( m_TransformedCurrentEstimate );
  ifft->UpdateLargestPossibleRegion();

  // Shift the image by negative one half the input size in each
  // dimension
  typedef CyclicShiftImageFilter< InternalImageType > EstimateShiftFilterType;
  typename EstimateShiftFilterType::Pointer estimateShifter =
    EstimateShiftFilterType::New();
  typename EstimateShiftFilterType::OffsetType shift;
  typename InternalImageType::SizeType inputSize =
    this->GetInput()->GetLargestPossibleRegion().GetSize();
  for (unsigned int i = 0; i < InternalImageType::ImageDimension; ++i)
    {
    shift[i] = -(static_cast< typename EstimateShiftFilterType::OffsetValueType >( inputSize[i] ) / 2);
    }

  estimateShifter->SetShift( shift );
  estimateShifter->SetInput( ifft->GetOutput() );

  // Normalize the image so that it sums to 1
  typedef NormalizeToConstantImageFilter< InternalImageType, InternalImageType >
    NormalizeEstimateFilterType;
  typename NormalizeEstimateFilterType::Pointer normalizer =
    NormalizeEstimateFilterType::New();
  normalizer->SetConstant( 1.0 );
  normalizer->SetInput( estimateShifter->GetOutput() );

  // Take the DFT of the shifted image
  typedef RealToHalfHermitianForwardFFTImageFilter< InternalImageType,
                                                    InternalComplexImageType >
    ForwardFFTFilterType;
  typename ForwardFFTFilterType::Pointer fft = ForwardFFTFilterType::New();
  fft->SetInput( normalizer->GetOutput() );
  fft->UpdateLargestPossibleRegion();

  typedef ComplexConjugateImageAdaptor< InternalComplexImageType > ComplexAdaptorType;
  typename ComplexAdaptorType::Pointer complexAdaptor = ComplexAdaptorType::New();
  complexAdaptor->SetImage( fft->GetOutput() );

  // Now we can compute the Jacobian (the derivative of the least-squares
  // objective function with respect to the kernel image intensity
  // changes) in preparation for computing the derivative of the
  // objective function with respect to the parameters of the
  // parametric kernel
  typedef MultiplyImageFilter< InternalComplexImageType, ComplexAdaptorType > MultiplyFilterType;
  typename MultiplyFilterType::Pointer multiplier = MultiplyFilterType::New();
  multiplier->SetInput1( m_DifferenceFilter->GetOutput() );
  multiplier->SetInput2( complexAdaptor );

  // Compute the inverse DFT of the result to get the Jacobian
  typename InverseFFTFilterType::Pointer jacobianIFFT = InverseFFTFilterType::New();
  jacobianIFFT->SetInput( multiplier->GetOutput() );
  jacobianIFFT->SetActualXDimensionIsOdd( this->GetXDimensionIsOdd() );
  jacobianIFFT->UpdateLargestPossibleRegion();

  // Now compute the partial derivative images of the kernel using
  // finite differences.
  typename KernelSourceType::ParametersType parameters =
    m_KernelSource->GetParameters();
  std::vector< double > gradient( parameters.Size() );

  for (unsigned int i = 0; i < parameters.Size(); ++i)
    {
    typedef typename KernelSourceType::OutputImageType InternalKernelImageType;
    typedef typename InternalKernelImageType::Pointer  InternalKernelImagePointer;
    typename KernelSourceType::ParametersValueType theta = parameters[i];
    double deltaTheta = 0.0001;
    double thetaPlus  = theta + deltaTheta;
    double thetaMinus = theta - deltaTheta;

    // Generate the plus image
    parameters[i] = thetaPlus;
    m_KernelSource->SetParameters( parameters );
    m_KernelSource->UpdateLargestPossibleRegion();
    InternalKernelImagePointer plusImage = m_KernelSource->GetOutput();
    plusImage->DisconnectPipeline();

    // Generate the minus image
    parameters[i] = thetaMinus;
    m_KernelSource->SetParameters( parameters );
    m_KernelSource->UpdateLargestPossibleRegion();
    InternalKernelImagePointer minusImage = m_KernelSource->GetOutput();
    minusImage->DisconnectPipeline();

    // Subtract the two and divide by deltaTheta * 2 to get the
    // partial derivative image estimate, then multiply the result by
    // the Jacobian. We'll do this all in one loop to simplify things.
    typename InternalKernelImageType::RegionType region( plusImage->GetLargestPossibleRegion() );
    ImageRegionConstIterator< InternalKernelImageType > plusImageIter( plusImage, region );
    ImageRegionConstIterator< InternalKernelImageType > minusImageIter( minusImage, region );
    ImageRegionConstIterator< InternalImageType > jacobianImageIter( jacobianIFFT->GetOutput(), region );

    double sum = 0.0;
    while ( !plusImageIter.IsAtEnd() )
      {
      double dhdTheta = ( plusImageIter.Get() - minusImageIter.Get() ) / (2.0 * deltaTheta );
      sum += dhdTheta * jacobianImageIter.Get();

      ++plusImageIter;
      ++minusImageIter;
      ++jacobianImageIter;
      }
    gradient[i] = sum;

    parameters[i] = theta;
    }

  for (unsigned int i = 0; i < parameters.Size(); ++i)
    {
    parameters[i] = parameters[i] - m_Beta * gradient[i];
    }

  m_KernelSource->SetParameters( parameters );
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage >
void
ParametricBlindLeastSquaresDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage >
::Finish(ProgressAccumulator * progress, float progressWeight)
{
  // Take the inverse Fourier transform of the current estimate
  typedef HalfHermitianToRealInverseFFTImageFilter< InternalComplexImageType,
                                                    InternalImageType >
    InverseFFTFilterType;
  typename InverseFFTFilterType::Pointer ifft = InverseFFTFilterType::New();
  ifft->SetActualXDimensionIsOdd( this->GetXDimensionIsOdd() );
  ifft->SetInput( m_TransformedCurrentEstimate );
  ifft->UpdateLargestPossibleRegion();
  progress->RegisterInternalFilter( ifft, 0.0 * progressWeight );
  this->m_CurrentEstimate = ifft->GetOutput();
  this->m_CurrentEstimate->DisconnectPipeline();

  this->Superclass::Finish( progress, progressWeight );

  m_TransformedInput = ITK_NULLPTR;
  m_TransformedCurrentEstimate = ITK_NULLPTR;
  m_DifferenceFilter = ITK_NULLPTR;
  m_ImageUpdateFilter = ITK_NULLPTR;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage >
void
ParametricBlindLeastSquaresDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf( os, indent );

  os << indent << "KernelSource: " << m_KernelSource << std::endl;
  os << indent << "Alpha: " << m_Alpha << std::endl;
  os << indent << "Beta: " << m_Beta << std::endl;
}
} // end namespace itk

#endif
