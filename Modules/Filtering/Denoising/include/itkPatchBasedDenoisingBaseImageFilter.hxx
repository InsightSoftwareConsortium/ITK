#ifndef __itkPatchBasedDenoisingBaseImageFilter_hxx
#define __itkPatchBasedDenoisingBaseImageFilter_hxx

#include "itkPatchBasedDenoisingBaseImageFilter.h"
#include "itkExceptionObject.h"
#include "itkEventObject.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
::PatchBasedDenoisingBaseImageFilter()
{
  // Default patch size.
  m_PatchRadius = 4;

  // Default strategy for kernel-bandwidth estimation
  m_DoKernelBandwidthEstimation = true;
  m_KernelBandwidthUpdateFrequency = 3;

  // Default for number of denoising iterations to perform
  m_NumberOfIterations      = 5;
  m_ElapsedIterations       = 0;

  // Default noise model.
  m_NoiseModel = GAUSSIAN;
  // Default weights for the smoothing and fidelity terms.
  m_SmoothingWeight = 1.0;
  m_FidelityWeight  = 0.0;

  // Default to setting component space
  // based on pixel type
  m_AlwaysTreatComponentsAsEuclidean = false;
  m_ComponentSpace                   = EUCLIDEAN;

  m_ManualReinitialization  = false;
  m_State                   = UNINITIALIZED;
}

template <class TInputImage, class TOutputImage>
PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
::~PatchBasedDenoisingBaseImageFilter()
{
}

template <class TInputImage, class TOutputImage>
void
PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
::SetStateToInitialized()
{
  this->SetState(INITIALIZED);
}

template <class TInputImage, class TOutputImage>
void
PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
::SetStateToUninitialized()
{
  this->SetState(UNINITIALIZED);
}

template <class TInputImage, class TOutputImage>
void
PatchBasedDenoisingBaseImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method
  // Copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();
}

template <class TInputImage, class TOutputImage>
void
PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  if (this->GetState() == UNINITIALIZED)
  {
    // Allocate the output image
    this->AllocateOutputs();

    // Copy the input image to the output image.
    // Algorithms will operate directly on the output image and the update buffer.
    this->CopyInputToOutput();

    // Initialize patch weights.
    this->InitializePatchWeights();

    // Perform any other necessary pre-iteration initialization.
    this->Initialize();

    // Allocate the internal update buffer.
    // This takes place entirely within the subclass,
    // since this class cannot define an update buffer type.
    this->AllocateUpdateBuffer();

    this->SetStateToInitialized();
    m_ElapsedIterations = 0;
  }

  // Iterative Patch-Based Denoising Algorithm

  // Any pre-processing of the input can be done here.
  this->PreProcessInput();

  while ( ! this->Halt() )
  {
    // An optional method for precalculating global values,
    // or otherwise setting up for the next iteration.
    this->InitializeIteration();

    if ((m_DoKernelBandwidthEstimation) &&
        (m_ElapsedIterations % m_KernelBandwidthUpdateFrequency == 0))
    {
      // Find the optimal kernel bandwidth parameter.
      this->ComputeKernelBandwidthUpdate();
    }
    {
      std::ostringstream msg;
      msg << "Computing Image Update iteration " << m_ElapsedIterations+1
          << " of " << m_NumberOfIterations << std::endl;
      m_Logger->Write(itk::LoggerBase::INFO, msg.str());
    }

    // Update the image intensities to denoise the image.
    this->ComputeImageUpdate();
    this->ApplyUpdate();

    // Increase iteration count.
    ++m_ElapsedIterations;

    // Invoke the iteration event.
    this->InvokeEvent( IterationEvent() );
    if( this->GetAbortGenerateData() )
    {
      this->InvokeEvent( IterationEvent() );
      this->ResetPipeline();
      throw ProcessAborted(__FILE__,__LINE__);
    }
  }

  if (m_ManualReinitialization == false)
  {
    // Reset the state once execution is completed.
    this->SetStateToUninitialized();
  }

  // Any post-processing of the solution can be done here.
  this->PostProcessOutput();
}

template <class TInputImage, class TOutputImage>
void
PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
::InitializePatchWeights()
{
  // Default patch weights of all unity.
  PatchWeightsType patchWeights;
  // Allocate patch weights.
  patchWeights.SetSize(this->GetPatchLengthInVoxels());
  // Assign default patch weights of all unity => rectangular patch.
  patchWeights.Fill(1.0);
  this->SetPatchWeights(patchWeights);
}

template <class TInputImage, class TOutputImage>
void
PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
::SetPatchWeights(const PatchWeightsType& weights)
{
  itkAssertOrThrowMacro( this->GetPatchLengthInVoxels() == weights.GetSize(),
                         "Unexpected patch size encountered while setting patch weights" );

  // Allocate patch weights.
  m_PatchWeights.SetSize (this->GetPatchLengthInVoxels());

  // Copy weights to m_PatchWeights
  for (unsigned int pos = 0; pos < this->GetPatchLengthInVoxels(); ++pos)
  {
    itkAssertOrThrowMacro( (weights[pos] >= 0.0f) && (weights[pos] <= 1.0f),
                           "Patch weights must be in the range [0,1]" );
    m_PatchWeights[pos] = weights[pos];
  }
}

template <class TInputImage, class TOutputImage>
typename PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>::PatchWeightsType
PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
::GetPatchWeights() const
{
  return m_PatchWeights;
}

template <class TInputImage, class TOutputImage>
typename PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>::PatchRadiusType::SizeValueType
PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
::GetPatchLengthInVoxels() const
{
  const PatchRadiusType diameter = this->GetPatchDiameterInVoxels();
  typename PatchRadiusType::SizeValueType length = 1;
  for (unsigned int dim = 0; dim < ImageDimension; ++dim)
  {
    length *= diameter[dim];
  }
  return length;
}

template <class TInputImage, class TOutputImage>
typename PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>::PatchRadiusType
PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
::GetPatchDiameterInVoxels() const
{
  PatchRadiusType one;
  PatchRadiusType two;
  one.Fill(1);
  two.Fill(2);
  const PatchRadiusType radius = this->GetPatchRadiusInVoxels();
  const PatchRadiusType diameter = two * radius + one;
  return diameter;
}

template <class TInputImage, class TOutputImage>
typename PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>::PatchRadiusType
PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
::GetPatchRadiusInVoxels() const
{
  const typename Self::Pointer thisPtr = const_cast< Self* >(this);
  const typename InputImageType::Pointer inputPtr
    = const_cast< InputImageType* >( thisPtr->GetInput() );
  const typename InputImageType::SpacingType spacing = inputPtr->GetSpacing();

  PatchRadiusType radius;
  radius.Fill(m_PatchRadius);
  for (unsigned int dim = 0; dim < ImageDimension; ++dim)
  {
    radius[dim] = vnl_math_ceil (radius[dim] / spacing[dim]);
  }
  return radius;
}

template <class TInputImage, class TOutputImage>
bool
PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
::Halt()
{
  if (m_NumberOfIterations != 0)
  {
    this->UpdateProgress( static_cast<float>( this->GetElapsedIterations() ) /
                          static_cast<float>( m_NumberOfIterations ) );
  }

  // Check and indicate whether to continue iterations or stop.
  if (this->GetElapsedIterations() >= m_NumberOfIterations)
  {
    return true;
  }
  else
  {
    return false;
  }
}

template <class TInputImage, class TOutputImage>
void
PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ElapsedIterations: "
     << m_ElapsedIterations << std::endl;
  os << indent << "NumberOfIterations: "
     << m_NumberOfIterations << std::endl;
  os << indent << "KernelBandwidthUpdateFrequency: "
     << m_KernelBandwidthUpdateFrequency << std::endl;
  if (m_NoiseModel == Self::GAUSSIAN)
  {
    os << indent << "NoiseModel: GAUSSIAN" << std::endl;
  }
  else if (m_NoiseModel == Self::RICIAN)
  {
    os << indent << "NoiseModel: RICIAN" << std::endl;
  }
  else if (m_NoiseModel == Self::POISSON)
  {
    os << indent << "NoiseModel: POISSON" << std::endl;
  }
  else {}

  if (m_ComponentSpace == Self::EUCLIDEAN)
  {
    os << indent << "ComponentSpace: EUCLIDEAN" << std::endl;
  }
  else if (m_ComponentSpace == Self::RIEMANNIAN)
  {
    os << indent << "ComponentSpace: RIEMANNIAN" << std::endl;
  }
  else {}

  os << indent << "State: "
     << m_State << std::endl;
  os << indent << "ManualReinitialization: "
     << m_ManualReinitialization << std::endl;
  os << std::endl;
}

}// end namespace itk

#endif
