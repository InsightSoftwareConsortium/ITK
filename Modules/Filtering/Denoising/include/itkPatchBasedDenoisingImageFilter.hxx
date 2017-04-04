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
#ifndef itkPatchBasedDenoisingImageFilter_hxx
#define itkPatchBasedDenoisingImageFilter_hxx

#include "itkPatchBasedDenoisingImageFilter.h"
#include "itkNthElementImageAdaptor.h"
#include "itkMinimumMaximumImageFilter.h"
#include "itkResampleImageFilter.h"
#include "itkIdentityTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkImageFileWriter.h"
#include "itkGaussianOperator.h"
#include "itkImageAlgorithm.h"
#include "itkVectorImageToImageAdaptor.h"
#include "itkSpatialNeighborSubsampler.h"
#include "itkMacro.h"
#include "itkMath.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::PatchBasedDenoisingImageFilter() :
  m_UpdateBuffer( OutputImageType::New() ),
  m_NumPixelComponents( 0 ),       // not valid until Initialize()
  m_NumIndependentComponents( 0 ), // not valid until Initialize()
  m_TotalNumberPixels( 0 ),        // not valid until an image is provided
  m_UseSmoothDiscPatchWeights( true ),
  m_UseFastTensorComputations( true ),
  m_KernelBandwidthSigmaIsSet( false ),
  m_ZeroPixel(),                 // not valid until Initialize()
  m_KernelBandwidthFractionPixelsForEstimation( 0.20 ),
  m_ComputeConditionalDerivatives( false ),
  m_MinSigma( NumericTraits<RealValueType>::min() * 100 ), // to avoid divide by zero
  m_MinProbability( NumericTraits<RealValueType>::min() * 100 ), // to avoid divide by zero
  m_SigmaUpdateDecimationFactor( static_cast<unsigned int>
                                ( Math::Round<double>( 1.0 / m_KernelBandwidthFractionPixelsForEstimation ) ) ),
  m_SigmaUpdateConvergenceTolerance( 0.01 ),   // desired accuracy of Newton-Raphson sigma estimation
  m_KernelBandwidthMultiplicationFactor( 1.0 ),
  m_NoiseSigma( 0.0 ),
  m_NoiseSigmaSquared( 0.0 ),
  m_NoiseSigmaIsSet( false ),
  m_SearchSpaceList( ListAdaptorType::New() )
{
  // By default, turn off automatic kernel bandwidth sigma estimation
  this->KernelBandwidthEstimationOff();
}

template <typename TInputImage, typename TOutputImage>
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::~PatchBasedDenoisingImageFilter()
{
  this->EmptyCaches();
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::EmptyCaches()
{
  for( unsigned int threadId = 0; threadId < m_ThreadData.size(); ++threadId )
    {
    m_ThreadData[threadId].eigenValsCache.clear();
    m_ThreadData[threadId].eigenVecsCache.clear();
    }
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::SetThreadData(int threadId, const ThreadDataStruct& data)
{
  if( threadId < static_cast<int>(m_ThreadData.size() ) )
    {
    m_ThreadData[threadId] = data;
    }
  else
    {
    itkExceptionMacro(<< "Invalid thread id " << threadId
                      << " or SetThreadData called before m_ThreadData (size="
                      << m_ThreadData.size()
                      << ") was initialized.");
    }
}

template <typename TInputImage, typename TOutputImage>
typename PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>::ThreadDataStruct
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::GetThreadData(int threadId)
{
  if( threadId < static_cast<int>(m_ThreadData.size() ) )
    {
    return m_ThreadData[threadId];
    }
  else
    {
    itkExceptionMacro(<< "Invalid thread id " << threadId
                      << " or GetThreadData called before m_ThreadData (size="
                      << m_ThreadData.size()
                      << ") was initialized.");
    }
  return ThreadDataStruct(); // keep the compiler happy
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::SetNoiseSigma(const RealType& sigma)
{
  m_NoiseSigma = sigma;
  m_NoiseSigmaSquared = sigma * sigma;
  m_NoiseSigmaIsSet = true;
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::SetKernelBandwidthSigma(const RealArrayType& kernelSigma)
{
  m_KernelBandwidthSigma = kernelSigma;
  m_KernelBandwidthSigmaIsSet = true;
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::CopyInputToOutput()
{
  if( !this->m_InputImage || !this->m_OutputImage )
    {
    itkExceptionMacro(<< "Input or Output image is ITK_NULLPTR.");
    }

  InputImageRegionConstIteratorType inputIt( this->m_InputImage, this->m_InputImage->GetRequestedRegion() );
  OutputImageRegionIteratorType     outputIt( this->m_OutputImage, this->m_OutputImage->GetRequestedRegion() );
  for( inputIt.GoToBegin(), outputIt.GoToBegin();
       !outputIt.IsAtEnd();
       ++inputIt, ++outputIt )
    {
    outputIt.Set(inputIt.Get() );
    }
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // Typical patch-based denoising algorithms would have access to the entire
  // image for sampling
  // patches for entropy calculations and denoising.  But this may not be
  // memory-friendly for
  // extremely large images.  If the user desires, they can limit the size of
  // the requested region
  // and thus the size of the region available for calculations and patch
  // selection.  But this needs
  // to be managed carefully. This class doesn't allow RequestRegion() yet.
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input
  const typename Superclass::InputImagePointer inputPtr =
    const_cast< InputImageType * >( this->GetInput() );

  if( !inputPtr )
    {
    return;
    }

  // Try to set up a buffered region that will accommodate our
  // neighborhood operations.  This may not be possible and we
  // need to be careful not to request a region outside the largest
  // possible region, because the pipeline will give us whatever we
  // ask for.

  const PatchRadiusType voxelNeighborhoodSize = this->GetPatchRadiusInVoxels();

  // Get a copy of the input requested region (should equal the output
  // requested region)
  typename InputImageType::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();
  // Pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( voxelNeighborhoodSize );

  itkDebugMacro( << "Padding inputRequestedRegion by " << voxelNeighborhoodSize << "\n"
                 << "inputRequestedRegion: "           << inputRequestedRegion  << "\n"
                 << "largestPossibleRegion: "          << inputPtr->GetLargestPossibleRegion() );

  // Crop the input requested region at the input's largest possible region
  if( inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion() ) )
    {
    itkDebugMacro( << "Cropped inputRequestedRegion to: " << inputRequestedRegion );
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region). Throw an exception.

    // Store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion( inputRequestedRegion );

    // Build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::AllocateUpdateBuffer()
{
  // The update buffer looks just like the output.
  const typename OutputImageType::Pointer output = this->GetOutput();

  m_UpdateBuffer->CopyInformation(output);
  m_UpdateBuffer->SetRequestedRegion(output->GetRequestedRegion() );
  m_UpdateBuffer->SetBufferedRegion(output->GetBufferedRegion() );
  m_UpdateBuffer->Allocate();
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::Initialize()
{
  typename InputImageType::IndexType requiredIndex;
  requiredIndex.Fill(0);
  const typename InputImageType::RegionType largestRegion =
    this->GetInput()->GetLargestPossibleRegion();
  const PatchRadiusType radius = this->GetPatchRadiusInVoxels();
  PatchRadiusType two;
  two.Fill(2);
  requiredIndex += two * radius;

  if( !(largestRegion.IsInside(requiredIndex) ) )
    {
    // The image needs to be at least as big as a single patch
    // throw an exception when this isn't the case
    itkExceptionMacro( << "Patch is larger than the entire image (in at least one dimension)."
                       << "\nImage region: " << largestRegion
                       << "\nPatch length (2*radius + 1): " << this->GetPatchDiameterInVoxels()
                       << "\nUse a smaller patch for this image.\n"; )
    }

  // Get the number of pixels in the input image.
  m_TotalNumberPixels = largestRegion.GetNumberOfPixels();

  // For automatic sigma estimation, select every 'k'th pixel.
  m_SigmaUpdateDecimationFactor = static_cast<unsigned int> (
    Math::Round<double>( 1.0 / m_KernelBandwidthFractionPixelsForEstimation ) );
  // For automatic sigma estimation, use at least 1% of pixels.
  m_SigmaUpdateDecimationFactor = std::min( m_SigmaUpdateDecimationFactor,
                   static_cast<unsigned int>
                    ( Math::Round<double>(m_TotalNumberPixels / 100.0 ) ) );
  // For automatic sigma estimation, can't use more than 100% of pixels.
  m_SigmaUpdateDecimationFactor = std::max(m_SigmaUpdateDecimationFactor, 1u);

  itkDebugMacro(  <<"m_KernelBandwidthFractionPixelsForEstimation: "
                  << m_KernelBandwidthFractionPixelsForEstimation << ", "
                  << "m_SigmaUpdateDecimationFactor: "  << m_SigmaUpdateDecimationFactor );

  // Get the number of components per pixel in the input image.
  m_NumPixelComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
  NumericTraits<PixelType>::SetLength( m_ZeroPixel, m_NumPixelComponents );
  m_ZeroPixel = NumericTraits<PixelType>::ZeroValue( m_ZeroPixel );

  // Set the number of independent components or channels.
  // This value is dependent on the pixel type.
  // For instance, a DiffusionTensor3D pixel should be treated as one unit
  // instead of 6 independent channels, unless the user has explicitly
  // specified that they should be treated independently.
  if( this->GetAlwaysTreatComponentsAsEuclidean() )
    {
    this->SetComponentSpace( Superclass::EUCLIDEAN );
    }
  else
    {
    this->SetComponentSpace( this->DetermineComponentSpace( m_ZeroPixel ) );
    }

  if( this->GetComponentSpace() == Superclass::EUCLIDEAN )
    {
    m_NumIndependentComponents = m_NumPixelComponents;
    }
  else
    {
    m_NumIndependentComponents = 1;
    }

  this->EmptyCaches();

  // initialize thread data struct
  const unsigned int numThreads = this->GetNumberOfThreads();
  for( unsigned int thread = 0; thread < numThreads; ++thread )
    {
    ThreadDataStruct newStruct;
    newStruct.entropyFirstDerivative.SetSize( m_NumIndependentComponents );
    newStruct.entropySecondDerivative.SetSize( m_NumIndependentComponents );
    newStruct.validDerivatives.SetSize( m_NumIndependentComponents );
    newStruct.validNorms.SetSize( m_NumIndependentComponents );
    newStruct.minNorm.SetSize( m_NumIndependentComponents );
    newStruct.maxNorm.SetSize( m_NumIndependentComponents );
    for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
      {
      newStruct.validDerivatives[ic] = 0;
      newStruct.entropyFirstDerivative[ic] = 0;
      newStruct.entropySecondDerivative[ic] = 0;
      newStruct.validNorms[ic] = 0;
      newStruct.minNorm[ic] = 0;
      newStruct.maxNorm[ic] = 0;
      }
    newStruct.sampler = ITK_NULLPTR;

    m_ThreadData.push_back( newStruct );
    }

  m_IntensityRescaleInvFactor.SetSize( m_NumIndependentComponents );
  m_ImageMin.SetSize( m_NumIndependentComponents );
  m_ImageMax.SetSize( m_NumIndependentComponents );
  this->ComputeMinMax( this->m_InputImage );

  this->EnforceConstraints();

  // For sigma calculation, keep the intensity range to 100 to avoid numerical
  // issues.
  // Compute the inverse factor to do multiplication later instead of division.
  for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
    {
    const PixelValueType max = m_ImageMax[ic];
    const PixelValueType min = m_ImageMin[ic];
    m_IntensityRescaleInvFactor[ic] = 100.0 / (max - min);
    }

  if( !this->m_KernelBandwidthSigmaIsSet )
    {
    // Kernel sigma initialization for automatic sigma estimation
    m_KernelBandwidthSigma.SetSize(m_NumIndependentComponents);
    this->InitializeKernelSigma();
    }
  else
    {
    // Use the kernel sigma that has already been set
    // either by default or explicitly by the user
    for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
      {
      // Check that the set kernel sigma is valid
      if( m_KernelBandwidthSigma[ic] <= m_MinSigma )
        {
        itkExceptionMacro (<< "Gaussian kernel sigma ("
                           << m_KernelBandwidthSigma[ic]
                           << ") must be larger than "
                           << m_MinSigma);
        }
      }
    }

  itkDebugMacro( << "Image Intensity range: ["
                 << m_ImageMin << ","
                 << m_ImageMax << "], "
                 << "IntensityRescaleInvFactor: "
                 << m_IntensityRescaleInvFactor << " , "
                 << "KernelBandwidthMultiplicationFactor: "
                 << m_KernelBandwidthMultiplicationFactor << " , "
                 << "KernelBandwidthSigma initialized to: "
                 << m_KernelBandwidthSigma );

  if( !this->m_NoiseSigmaIsSet )
    {
    NumericTraits<RealType>::SetLength(m_NoiseSigma, m_NumPixelComponents);
    NumericTraits<RealType>::SetLength(m_NoiseSigmaSquared, m_NumPixelComponents);
    for( unsigned int pc = 0; pc < m_NumPixelComponents; ++pc )
      {
      // Initialize to 5% of the intensity range
      RealValueType invFactor;
      if( this->GetComponentSpace() == Superclass::EUCLIDEAN )
        {
        invFactor = m_IntensityRescaleInvFactor[pc];
        }
      else
        {
        invFactor = m_IntensityRescaleInvFactor[0];
        }
      RealValueType sigma = 5.0 / invFactor;
      this->SetComponent(m_NoiseSigma, pc, sigma);
      this->SetComponent(m_NoiseSigmaSquared, pc, sigma * sigma);
      }
    }

  if( m_Sampler.IsNull() )
    {
    typedef itk::Statistics::SpatialNeighborSubsampler< PatchSampleType, InputImageRegionType >
    SamplerType;
    typename SamplerType::Pointer defaultSampler = SamplerType::New();

    defaultSampler->SetRadius( 25 );
    this->SetSampler( defaultSampler );
    }
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::EnforceConstraints()
{
  // Image cannot be constant
  for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
    {
    if( m_ImageMax[ic] <= m_ImageMin[ic] )
      {
      itkExceptionMacro( << "Each image component must be nonconstant.  "
                         << "Component " << ic
                         << " has the constant value " << m_ImageMax[ic]
                         << ".\n"; );
      }
    }

  // For Poisson or Rician noise models, image must be >= 0
  if( this->GetNoiseModel() == Superclass::RICIAN ||
    this->GetNoiseModel() == Superclass::POISSON )
    {
    for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
      {
      if( m_ImageMin[ic] < NumericTraits<PixelValueType>::ZeroValue() )
        {
        itkExceptionMacro( << "When using POISSON or RICIAN noise models, "
                           << "all components of all pixels in the image must "
                           << "be >= 0.  The smallest value for component "
                           << ic << " in the image is "
                           << m_ImageMin[ic] << ".\n"; );
        }
      }
    }

  // Do not know how to compute noise model in RIEMANNIAN case
  // so make sure the computation is disabled
  if( this->GetComponentSpace() == Superclass::RIEMANNIAN )
    {
    if( this->GetNoiseModelFidelityWeight() > 0 )
      {
      itkWarningMacro( << "Noise model is undefined for RIEMANNIAN case, "
                       << "disabling noise model by setting fidelity weight "
                       << "to zero." );
      this->SetNoiseModelFidelityWeight(0.0);
      }
    }
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::InitializePatchWeights()
{
  if( m_UseSmoothDiscPatchWeights )
    {
    // Redefine patch weights to make the patch more isotropic (less
    // rectangular).
    this->InitializePatchWeightsSmoothDisc();
    }
  else
    {
    // Default patch weights of all unity.
    PatchWeightsType patchWeights;
    patchWeights.SetSize(this->GetPatchLengthInVoxels());
    patchWeights.Fill(1);
    this->SetPatchWeights(patchWeights);
    }
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::InitializePatchWeightsSmoothDisc()
{
  typedef itk::Image<float, ImageDimension> WeightsImageType;
  typedef float                             DistanceType;

  const unsigned int patchRadius = this->GetPatchRadius();

  // Patch size in physical space
  const unsigned int physicalDiameter= 2 * patchRadius + 1;

  // Really we just want to handle relative anisotropies,
  // so base resampling on the deviation from the max spacing
  // in the input image.
  typename WeightsImageType::SpacingType physicalSpacing;
  typename WeightsImageType::SpacingValueType maxSpacing;
  physicalSpacing = this->m_InputImage->GetSpacing();
  maxSpacing = physicalSpacing[0];
  for( unsigned int sp = 1; sp < physicalSpacing.Size(); ++sp )
    {
    if( physicalSpacing[sp] > maxSpacing )
      {
      maxSpacing = physicalSpacing[sp];
      }
    }
  physicalSpacing.Fill(maxSpacing);

  // Allocate the patch weights (mask) as an image.
  // Done in physical space.
  typename WeightsImageType::SizeType physicalSize;
  physicalSize.Fill(physicalDiameter);
  typename WeightsImageType::RegionType physicalRegion(physicalSize);
  typename WeightsImageType::Pointer physicalWeightsImage = WeightsImageType::New();
  physicalWeightsImage->SetRegions(physicalRegion);
  physicalWeightsImage->SetSpacing(physicalSpacing);
  physicalWeightsImage->Allocate();
  physicalWeightsImage->FillBuffer(1.0);

  typename WeightsImageType::IndexType centerIndex;
  centerIndex.Fill(patchRadius);

  ImageRegionIteratorWithIndex<WeightsImageType> pwIt(physicalWeightsImage, physicalRegion);
  unsigned int pos = 0;
  for( pwIt.GoToBegin(); !pwIt.IsAtEnd(); ++pwIt )
    {
    typename WeightsImageType::IndexType curIndex;
    curIndex = pwIt.GetIndex();
    // Compute distances of each pixel from center pixel
    Vector <DistanceType, ImageDimension> distanceVector;
    for( unsigned int d = 0; d < ImageDimension; d++ )
      {
      distanceVector[d] = static_cast<DistanceType> (curIndex[d])
                          - static_cast<DistanceType> (centerIndex[d]);
      }
    const float distanceFromCenter = distanceVector.GetNorm();

    // Compute weights based on distance of pixel from center
    // details in Awate and Whitaker 2005 IEEE CVPR, 2006 IEEE TPAMI
    const unsigned int discRadius = patchRadius / 2;
    if( distanceFromCenter >= patchRadius + 1 )
      {
      // Weights for pixels outside disc of radius 2*discRadius+1
      // zero (minimum)
      pwIt.Set(0.0);
      }
    else if( distanceFromCenter <= discRadius )
      {
      // Weights for pixels inside disc of radius discRadius
      // one (maximum)
      pwIt.Set(1.0);
      }
    else
      {
      // Weights for pixels between the inner disc and the outer disc
      // weights between zero and one determined by cubic-spline interpolation
      const unsigned int interval = (patchRadius + 1) - discRadius;
      const float weight =
        (-2.0 / pow (interval, 3.0) ) * pow ( (patchRadius + 1) - distanceFromCenter, 3.0f)
        + ( 3.0 / pow (interval, 2.0) ) * pow ( (patchRadius + 1) - distanceFromCenter, 2.0f);
      pwIt.Set(std::max (static_cast<float>(0), std::min (static_cast<float>(1), weight) ) );
      }
    }

  // Take the patch weights in physical space and
  // resample to get patch weights in voxel space
  typedef itk::ResampleImageFilter<
      WeightsImageType, WeightsImageType > ResampleFilterType;
  typedef itk::IdentityTransform<
      double, ImageDimension >             TransformType;
  typedef itk::LinearInterpolateImageFunction<
      WeightsImageType, double >           InterpolatorType;

  typename ResampleFilterType::Pointer  resampler = ResampleFilterType::New();
  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();
  typename TransformType::Pointer       transform = TransformType::New();
  transform->SetIdentity();

  resampler->SetTransform( transform );
  resampler->SetInterpolator( interpolator );

  resampler->SetInput( physicalWeightsImage );
  resampler->SetSize( this->GetPatchDiameterInVoxels() );
  resampler->SetDefaultPixelValue( 0 );
  resampler->SetOutputOrigin( physicalWeightsImage->GetOrigin() );
  resampler->SetOutputDirection( physicalWeightsImage->GetDirection() );
  resampler->SetOutputSpacing( this->m_InputImage->GetSpacing() );
  resampler->Update();

  // Patch weights (mask) in voxel space
  const typename WeightsImageType::Pointer voxelWeightsImage = resampler->GetOutput();
  ImageRegionConstIterator<WeightsImageType>
  vwIt (voxelWeightsImage, voxelWeightsImage->GetLargestPossibleRegion() );

  // Disc-smooth patch weights in voxel space
  PatchWeightsType patchWeights;
  patchWeights.SetSize(this->GetPatchLengthInVoxels() );
  patchWeights.Fill(1);
  for( vwIt.GoToBegin(), pos = 0; !vwIt.IsAtEnd(); ++vwIt, ++pos )
    {
    patchWeights[pos] = std::max ( static_cast<float>(0), // ensure weight >= 0
                       std::min ( static_cast<float>(1), // ensure weight <= 1
                       vwIt.Get() ) );
    itkDebugMacro( "patchWeights[ " << pos << " ]: " << patchWeights[pos] );

    } // end for each element in the patch

  typename PatchWeightsType::ValueType centerWeight =
    patchWeights[(this->GetPatchLengthInVoxels() - 1)/2];
  if( centerWeight != 1.0 )
    {
    if( centerWeight <= 0.0 )
      {
      itkExceptionMacro (<< "Center pixel's weight ("
                         << patchWeights[(this->GetPatchLengthInVoxels() - 1)/2]
                         << ") must be greater than 0.0 ");
      }

    // Normalize to the center weight to guarantee that the center weight == 1.0
    typename PatchWeightsType::SizeValueType pSize = patchWeights.Size();
    for( pos = 0; pos < pSize; ++pos )
      {
      patchWeights[pos] = patchWeights[pos] / centerWeight;
      }
    }

  this->SetPatchWeights(patchWeights);
}

template <typename TInputImage, typename TOutputImage>
template <typename TInputImageType>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::DispatchedMinMax(const TInputImageType* img)
{
  typedef MinimumMaximumImageFilter<TInputImageType> MinMaxFilter;
  typename MinMaxFilter::Pointer minmax = MinMaxFilter::New();
  minmax->SetInput(img);
  minmax->Modified();
  minmax->Update();

  m_ImageMin[0] = minmax->GetMinimum();
  m_ImageMax[0] = minmax->GetMaximum();
}

template <typename TInputImage, typename TOutputImage>
template <typename TInputImageType>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::DispatchedArrayMinMax(const TInputImageType* img)
{
  typedef NthElementImageAdaptor<TInputImageType, PixelValueType> AdaptorType;
  typedef MinimumMaximumImageFilter<AdaptorType>                  MinMaxFilter;

  typename AdaptorType::Pointer adaptor = AdaptorType::New();
  adaptor->SetImage( const_cast<InputImageType*>(img) );

  typename MinMaxFilter::Pointer minmax = MinMaxFilter::New();

  for( unsigned int pc = 0; pc < m_NumPixelComponents; ++pc )
    {
    adaptor->SelectNthElement(pc);
    minmax->SetInput(adaptor);
    minmax->Modified();
    minmax->Update();

    m_ImageMin[pc] = minmax->GetMinimum();
    m_ImageMax[pc] = minmax->GetMaximum();
    }
}

template <typename TInputImage, typename TOutputImage>
template <typename TInputImageType>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::DispatchedRiemannianMinMax(const TInputImageType* img)
{
  // Set up for multithreaded processing.
  ThreadFilterStruct str;
  str.Filter = this;
  str.Img = const_cast<InputImageType*>(img);
  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads() );
  this->GetMultiThreader()->SetSingleMethod(this->RiemannianMinMaxThreaderCallback,
                                            &str);
  // Multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();
  this->ResolveRiemannianMinMax();
}

template <typename TInputImage, typename TOutputImage>
ITK_THREAD_RETURN_TYPE
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::RiemannianMinMaxThreaderCallback(void * arg)
{
  const unsigned int threadId    = ( (MultiThreader::ThreadInfoStruct *)(arg) )->ThreadID;
  const unsigned int threadCount = ( (MultiThreader::ThreadInfoStruct *)(arg) )->NumberOfThreads;

  const ThreadFilterStruct * str =
    (ThreadFilterStruct *)( ( (MultiThreader::ThreadInfoStruct *)(arg) )->UserData);

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Using the SplitRequestedRegion method from itk::ImageSource.
  InputImageRegionType splitRegion;

  const unsigned int total =
    str->Filter->SplitRequestedRegion(threadId, threadCount, splitRegion);

  if( threadId < total )
    {
    str->Filter->SetThreadData(threadId,
                               str->Filter->ThreadedRiemannianMinMax(splitRegion, threadId,
                                                                     str->Img,
                                                                     str->Filter->GetThreadData(threadId) ) );
    }

  return ITK_THREAD_RETURN_VALUE;
}

template <typename TInputImage, typename TOutputImage>
typename PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>::ThreadDataStruct
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
// ::ThreadedRiemannianMinMax(const typename
// Image<DiffusionTensor3D<PixelValueType>, ImageDimension>::RegionType
// &regionToProcess,
::ThreadedRiemannianMinMax(const InputImageRegionType &regionToProcess,
                           const int itkNotUsed(threadId),
                           const InputImageType* img,
                           ThreadDataStruct threadData)
{
  // Compute the min and max norm of the length of the log map
  // with respect to identity
  typedef typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>
    FaceCalculatorType;
  typedef typename FaceCalculatorType::FaceListType
    FaceListType;

  typename InputImageType::SizeType radius;
  radius.Fill(1);

  if( m_NumIndependentComponents != 1 )
    {
    itkWarningMacro( << "ThreadedRiemannianMinMax calculation assumes that "
                     << "there is exactly 1 independent component, but "
                     << "num independent components = " << m_NumIndependentComponents
                     << " instead.\n" );
    }

  PixelType identityTensor(m_ZeroPixel);
  for( unsigned int ii = 0; ii < ImageDimension; ++ii )
    {
    identityTensor(ii,ii) = NumericTraits<PixelValueType>::OneValue();
    }
  RealArrayType identityWeight(m_NumIndependentComponents);
  identityWeight.Fill(NumericTraits<RealValueType>::OneValue());
  RealType      tmpDiff;
  RealArrayType tmpNorm(m_NumIndependentComponents);
  RealArrayType minNorm(m_NumIndependentComponents);
  RealArrayType maxNorm(m_NumIndependentComponents);
  maxNorm.Fill(NumericTraits<RealValueType>::min() );
  minNorm.Fill(NumericTraits<RealValueType>::max() );

  FaceCalculatorType faceCalculator;
  FaceListType faceList = faceCalculator(img, regionToProcess, radius);
  typename FaceListType::iterator fIt;
  bool foundMinMax = false;

  for( fIt = faceList.begin(); fIt != faceList.end(); ++fIt )
    {
    if( !( fIt->GetNumberOfPixels() ) )
      {
      // Empty region, move on to next
      continue;
      }

    // Only evaluating each pixel once, so nothing to cache
    bool useCachedComputations = false;
    InputImageRegionConstIteratorType imgIt(img, *fIt);
    imgIt.GoToBegin();
    for( imgIt.GoToBegin(); !imgIt.IsAtEnd(); ++imgIt )
      {
      this->ComputeLogMapAndWeightedSquaredGeodesicDifference(imgIt.Get(), identityTensor,
                                                        identityWeight,
                                                        useCachedComputations, 0,
                                                        threadData.eigenValsCache,
                                                        threadData.eigenVecsCache,
                                                        tmpDiff, tmpNorm);

      if( tmpNorm[0] < minNorm[0] )
        {
        minNorm[0] = tmpNorm[0];
        }
      if( tmpNorm[0] > maxNorm[0] )
        {
        maxNorm[0] = tmpNorm[0];
        }
      foundMinMax = true;
      } // end for each pixel in the region
    }   // end for each region
  if( foundMinMax )
    {
    threadData.validNorms[0] = 1;
    threadData.minNorm[0] = std::sqrt(minNorm[0]);
    threadData.maxNorm[0] = std::sqrt(maxNorm[0]);

    itkDebugMacro( <<"threadData minNorm: " << minNorm[0]
                   << ", maxNorm: " << maxNorm[0] );
    }
  else
    {
    itkDebugMacro( <<"no pixels in this region" );
    }
  return threadData;
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ResolveRiemannianMinMax()
{
  const size_t numThreads = m_ThreadData.size();

  m_ImageMin.Fill(NumericTraits<PixelValueType>::max() );
  m_ImageMax.Fill(NumericTraits<PixelValueType>::min() );

  for( unsigned int threadNum = 0; threadNum < numThreads; ++threadNum )
    {
    for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
      {
      if( m_ThreadData[threadNum].validNorms[ic] > 0 )
        {
        itkDebugMacro( "m_ThreadData[" << threadNum << "].minNorm[" << ic
                                       << "]: " << m_ThreadData[threadNum].minNorm[ic]
                                       << "\nm_ThreadData[" << threadNum << "].maxNorm[" << ic
                                       << "]: " << m_ThreadData[threadNum].maxNorm[ic] );
        if( m_ThreadData[threadNum].minNorm[ic] < m_ImageMin[ic] )
          {
          m_ImageMin[ic] = static_cast<PixelValueType> (m_ThreadData[threadNum].minNorm[ic]);
          }
        if( m_ThreadData[threadNum].maxNorm[ic] > m_ImageMax[ic] )
          {
          m_ImageMax[ic] = static_cast<PixelValueType> (m_ThreadData[threadNum].maxNorm[ic]);
          }
        }
      }
    }

  itkDebugMacro( << "image min resolved to: " << m_ImageMin
                 << ", image max resolved to: " << m_ImageMax );
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ComputeSignedEuclideanDifferenceAndWeightedSquaredNorm(const PixelType& a, const PixelType& b,
                                                         const RealArrayType& weight,
                                                         bool itkNotUsed(useCachedComputations),
                                                         SizeValueType itkNotUsed(cacheIndex),
                                                         EigenValuesCacheType& itkNotUsed(eigenValsCache),
                                                         EigenVectorsCacheType& itkNotUsed(eigenVecsCache),
                                                         RealType& diff, RealArrayType& norm)
{
  for( unsigned int pc = 0; pc < m_NumPixelComponents; ++pc )
    {
    RealValueType tmpDiff = this->GetComponent(b, pc) - this->GetComponent(a, pc);
    RealValueType tmpWeight = weight[pc];
    this->SetComponent(diff, pc, tmpDiff);
    norm[pc] = tmpWeight * tmpWeight * tmpDiff * tmpDiff;
    }
}

template <typename TInputImage, typename TOutputImage>
template <typename TensorValueT>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::Compute3x3EigenAnalysis(const DiffusionTensor3D<TensorValueT>& spdMatrix,
                          FixedArray< TensorValueT, 3 >&  eigenVals,
                          Matrix< TensorValueT, 3, 3 >& eigenVecs)
{

  // This implementation based on
  // Hasan KM, Basser PJ, Parker DL, Alexander AL.
  // Analytical computation of the eigenvalues and eigenvectors in DT-MRI.
  // J Magn Reson 2001; 152: 41-47.

  typedef typename NumericTraits<TensorValueT>::RealType RealTensorValueT;

  // Precompute some commonly used values
  // D = spdMatrix ( to avoid pointer math)
  // following index map for reference since
  // D is always symmetric:
  //     | D00 D01 D02 |   | Dxx Dxy Dxz |   | 0 1 2 |
  // D = | D10 D11 D12 | = | Dyx Dyy Dyz | = | 1 3 4 |
  //     | D20 D21 D22 |   | Dzx Dzy Dzz |   | 2 4 5 |
  // DSq = spdMatrix .* spdMatrix
  // We don't need DSq0, DSq3, DSq5, so don't compute them
  const TensorValueT D0 = spdMatrix[0];
  const TensorValueT D1 = spdMatrix[1];
  const TensorValueT D2 = spdMatrix[2];
  const TensorValueT D3 = spdMatrix[3];
  const TensorValueT D4 = spdMatrix[4];
  const TensorValueT D5 = spdMatrix[5];
  const TensorValueT DSq1 = D1 * D1;
  const TensorValueT DSq2 = D2 * D2;
  const TensorValueT DSq4 = D4 * D4;


  // Compute the eigenvalues
  //
  // Compute the 3 principal invariants I1, I2, I3 of D
  // I1 = trace(D)
  // I2 = (DxxDyy + DxxDzz + DyyDzz) - ((Dxy)^2 + (Dxz)^2 + (Dyz)^2)
  // I3 = det(D) = DxxDyyDzz + 2DxyDxzDyz - (Dzz(Dxy)^2 + Dyy(Dxz)^2 +
  // Dxx(Dyz)^2)

  RealTensorValueT I1, I2, I3, I1div3;
  I1 = D0 + D3 + D5;
  I2 = D0*D3 + D0*D5 + D3*D5 - (DSq1 + DSq2 + DSq4);
  I3 = D0*D3*D5 + 2*D1*D2*D4 - (D5*DSq1 + D3*DSq2 + D0*DSq4);
  I1div3 = I1/3;

  // Compute rotationally-invariant variables n and s
  // n = (I1/3)^2 - I2/3
  // s = (I1/3)^3 - I1*I2/6 + I3/2

  RealTensorValueT n, sqrtn, s;
  n = I1div3 * I1div3 - I2/3;
  s = I1div3 * I1div3 * I1div3 - I1*I2/6 + I3/2;
  sqrtn = std::sqrt(n);

  // Now check for some degenerate cases.
  // If these occur, default to the standard eigen analysis.
  // These basically enforce that the spdMatrix is in fact
  // symmetric positive definite.
  if ( (I3 < NumericTraits<RealTensorValueT>::epsilon() ) ||
       (D0 < NumericTraits<RealTensorValueT>::ZeroValue()) ||
       (D3 < NumericTraits<RealTensorValueT>::ZeroValue()) ||
       (D5 < NumericTraits<RealTensorValueT>::ZeroValue()) ||
       (D0 * D3 < DSq1) ||
       (D0 * D5 < DSq2) ||
       (D3 * D5 < DSq4) ||
       (n < NumericTraits<RealTensorValueT>::epsilon() ) )
    {
    spdMatrix.ComputeEigenAnalysis(eigenVals, eigenVecs);
    return;
    }

  // Compute phi = (acos((s/n) * sqrt(1/n)) / 3)
  RealTensorValueT phi;
  phi = std::acos( (s/n) * 1/sqrtn) / 3;

  // Now compute the eigenvalues
  // lambda1 = I1/3 + 2*sqrt(n)*cos(phi)
  // lambda2 = I1/3 - 2*sqrt(n)*cos(pi/3 + phi)
  // lambda3 = I1/3 - 2*sqrt(n)*cos(pi/3 - phi)
  // Due to trace invariance,
  // lambda3 also = I1 - lambda1 - lambda2

  RealTensorValueT lambda1, lambda2, lambda3;
  lambda1 = I1div3 + 2 * sqrtn * std::cos(phi);
  lambda2 = I1div3 - 2*sqrtn * std::cos(itk::Math::pi/3 + phi);
  lambda3 = I1 - lambda1 - lambda2;

  eigenVals[0] = lambda1;
  eigenVals[1] = lambda2;
  eigenVals[2] = lambda3;


  // Compute the eigenvectors
  //
  // This for loop could compute all 3 eigenvectors
  // instead of just the first two, but it is more
  // efficient to obtain the third eigenvector by
  // computing the cross product of the first two
  // eigenvectors.
  for( unsigned int i = 0; i < 2; ++i )
    {
    // Compute helper variables A, B, C
    // Ai = Dxx - eigenVals[i]
    // Bi = Dyy - eigenVals[i]
    // Ci = Dzz - eigenVals[i]
    RealTensorValueT A, B, C;
    A = D0 - eigenVals[i];
    B = D3 - eigenVals[i];
    C = D5 - eigenVals[i];

    // Compute eigenvec components x, y and z
    // eix = (DxyDyz - BiDxz)(DxzDyz - CiDxy)
    // eiy = (DxzDyz - CiDxy)(DxzDxy - AiDyz)
    // eiz = (DxyDyz - BiDxz)(DxzDxy - AiDyz)
    // recognizing common components, this becomes
    // eix = term1 * term2
    // eiy = term2 * term3
    // eiz = term1 * term3
    RealTensorValueT term1, term2, term3;
    RealTensorValueT ex, ey, ez;
    term1 = D1*D4 - B*D2;
    term2 = D2*D4 - C*D1;
    term3 = D2*D1 - A*D4;
    ex = term1 * term2;
    ey = term2 * term3;
    ez = term1 * term3;

    // Now normalize the vector
    // e = [ex ey ez]
    // eigenVec = e / sqrt(e'e)
    RealTensorValueT norm, sqrtnorm;
    norm = ex * ex + ey * ey + ez * ez;
    sqrtnorm = std::sqrt(norm);
    eigenVecs(i,0) = ex / sqrtnorm;
    eigenVecs(i,1) = ey / sqrtnorm;
    eigenVecs(i,2) = ez / sqrtnorm;
    }

  // Finally, compute the third eigenvector
  // e[2] = e[0] x e[1]
  eigenVecs(2,0) = eigenVecs(0,1) * eigenVecs(1,2) - eigenVecs(0,2) * eigenVecs(1,1);
  eigenVecs(2,1) = eigenVecs(0,2) * eigenVecs(1,0) - eigenVecs(0,0) * eigenVecs(1,2);
  eigenVecs(2,2) = eigenVecs(0,0) * eigenVecs(1,1) - eigenVecs(0,1) * eigenVecs(1,0);
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ComputeLogMapAndWeightedSquaredGeodesicDifference(const DiffusionTensor3D<PixelValueType>& spdMatrixA,
                                                    const DiffusionTensor3D<PixelValueType>& spdMatrixB,
                                                    const RealArrayType& weight,
                                                    bool useCachedComputations,
                                                    SizeValueType cacheIndex,
                                                    EigenValuesCacheType& eigenValsCache,
                                                    EigenVectorsCacheType& eigenVecsCache,
                                                    RealType& symMatrixLogMap, RealArrayType& geodesicDist)
{
  typedef typename RealType::EigenValuesArrayType   RealEigenValuesArrayType;
  typedef typename RealType::EigenVectorsMatrixType RealEigenVectorsMatrixType;
  EigenValuesArrayType       eigenVals;
  EigenVectorsMatrixType     eigenVecs;
  RealEigenValuesArrayType   YEigenVals;
  RealEigenVectorsMatrixType YEigenVecs;
  RealType                   Y;

  if( useCachedComputations )
    {
    eigenVals = eigenValsCache[cacheIndex];
    eigenVecs = eigenVecsCache[cacheIndex];
    }
  else
    {
    if( m_UseFastTensorComputations )
      {
      this->Compute3x3EigenAnalysis( spdMatrixA, eigenVals, eigenVecs );
      }
    else
      {
      spdMatrixA.ComputeEigenAnalysis(eigenVals, eigenVecs);
      }
    for( unsigned int ii = 0; ii < 3; ++ii )
      {
      eigenVals[ii] = std::max(PixelValueType(1e-15), eigenVals[ii]);
      }

    if( cacheIndex >= eigenValsCache.size() )
      {
      eigenValsCache.resize(cacheIndex+1);
      eigenVecsCache.resize(cacheIndex+1);
      }

    eigenValsCache[cacheIndex] = EigenValuesArrayType(eigenVals);
    eigenVecsCache[cacheIndex] = EigenVectorsMatrixType(eigenVecs);
    }

  // Note that the ComputeEigenAnalysis returns a row vector for each
  // eigenvector

  // Compute Y = gInv * spdMatrixB * gInv'
  // where gInv = sqrtEigenValsInvMatrix * eigenVecs
  // and sqrtEigenValsInvMatrix is a diagonal matrix whose entries are 1.0 /
  // sqrt(eigenVals)
  // g = eigenVecs.GetTranspose() * sqrtEigenValsMatrix
  // where sqrtEigenValsMatrix is a diagonal matrix whose entries are
  // sqrt(eigenVals)
  //
  // Since we are always working with DiffusionTensor3D pixels which are always
  // 3x3, these calculations can be optimized as follows.

  RealValueType factor0;
  RealValueType factor1;
  RealValueType factor2;
  factor0 = spdMatrixB[0] * eigenVecs(0,0) + spdMatrixB[1] * eigenVecs(0,1) + spdMatrixB[2] * eigenVecs(0,2);
  factor1 = spdMatrixB[1] * eigenVecs(0,0) + spdMatrixB[3] * eigenVecs(0,1) + spdMatrixB[4] * eigenVecs(0,2);
  factor2 = spdMatrixB[2] * eigenVecs(0,0) + spdMatrixB[4] * eigenVecs(0,1) + spdMatrixB[5] * eigenVecs(0,2);

  Y[0] = ( eigenVecs(0,0) * factor0 + eigenVecs(0,1) * factor1 + eigenVecs(0,2) * factor2 ) / eigenVals[0];
  Y[1] = ( eigenVecs(1,0) * factor0 + eigenVecs(1,1) * factor1 + eigenVecs(1,2) * factor2 ) / std::sqrt(
      eigenVals[0] * eigenVals[1]);
  Y[2] = ( eigenVecs(2,0) * factor0 + eigenVecs(2,1) * factor1 + eigenVecs(2,2) * factor2 ) / std::sqrt(
      eigenVals[0] * eigenVals[2]);

  factor0 = spdMatrixB[0] * eigenVecs(1,0) + spdMatrixB[1] * eigenVecs(1,1) + spdMatrixB[2] * eigenVecs(1,2);
  factor1 = spdMatrixB[1] * eigenVecs(1,0) + spdMatrixB[3] * eigenVecs(1,1) + spdMatrixB[4] * eigenVecs(1,2);
  factor2 = spdMatrixB[2] * eigenVecs(1,0) + spdMatrixB[4] * eigenVecs(1,1) + spdMatrixB[5] * eigenVecs(1,2);

  Y[3] = ( eigenVecs(1,0) * factor0 + eigenVecs(1,1) * factor1 + eigenVecs(1,2) * factor2 ) / eigenVals[1];
  Y[4] = ( eigenVecs(2,0) * factor0 + eigenVecs(2,1) * factor1 + eigenVecs(2,2) * factor2 ) / std::sqrt(
      eigenVals[1] * eigenVals[2]);

  factor0 = spdMatrixB[0] * eigenVecs(2,0) + spdMatrixB[1] * eigenVecs(2,1) + spdMatrixB[2] * eigenVecs(2,2);
  factor1 = spdMatrixB[1] * eigenVecs(2,0) + spdMatrixB[3] * eigenVecs(2,1) + spdMatrixB[4] * eigenVecs(2,2);
  factor2 = spdMatrixB[2] * eigenVecs(2,0) + spdMatrixB[4] * eigenVecs(2,1) + spdMatrixB[5] * eigenVecs(2,2);

  Y[5] = ( eigenVecs(2,0) * factor0 + eigenVecs(2,1) * factor1 + eigenVecs(2,2) * factor2 ) / eigenVals[2];

  if( m_UseFastTensorComputations )
    {
    this->Compute3x3EigenAnalysis(Y, YEigenVals, YEigenVecs);
    }
  else
    {
    Y.ComputeEigenAnalysis(YEigenVals, YEigenVecs);
    }

  // Note that the ComputeEigenAnalysis returns a row vector for each
  // eigenvector

  // Compute symMatrixLogMap = temp * logEigenVals * temp.GetTranspose()
  // where logEigenVals is a diagonal matrix whose entries are log(eigenVals of
  // Y)
  // and temp = g * (eigenVecs of Y).GetTranspose()
  // geodesic distance = weight^2 * logSquaredEigenValsMatrix.GetTrace()
  // where logSquaredEigenValsMatrix is a Diagonal matrix whose values =
  // (log(eigenVals of Y)) ^ 2
  //
  // Since we are always working with DiffusionTensor3D pixels which are always
  // 3x3, these calculations can be optimized as follows.

  for( unsigned int ii = 0; ii < 3; ++ii )
    {
    YEigenVals[ii] = std::log(std::max(RealValueType(1e-15),YEigenVals[ii]) );
    }
  const RealValueType eigVal0 = std::sqrt(eigenVals[0]);
  const RealValueType eigVal1 = std::sqrt(eigenVals[1]);
  const RealValueType eigVal2 = std::sqrt(eigenVals[2]);
  const RealValueType YEigVal0 = YEigenVals[0];
  const RealValueType YEigVal1 = YEigenVals[1];
  const RealValueType YEigVal2 = YEigenVals[2];

  const RealValueType temp00 = eigenVecs(0,0) * YEigenVecs(0,0) * eigVal0 +
    eigenVecs(1,0) * YEigenVecs(0,1) * eigVal1 +
    eigenVecs(2,0) * YEigenVecs(0,2) * eigVal2;
  const RealValueType temp01 = eigenVecs(0,0) * YEigenVecs(1,0) * eigVal0 +
    eigenVecs(1,0) * YEigenVecs(1,1) * eigVal1 +
    eigenVecs(2,0) * YEigenVecs(1,2) * eigVal2;
  const RealValueType temp02 = eigenVecs(0,0) * YEigenVecs(2,0) * eigVal0 +
    eigenVecs(1,0) * YEigenVecs(2,1) * eigVal1 +
    eigenVecs(2,0) * YEigenVecs(2,2) * eigVal2;

  const RealValueType temp10 = eigenVecs(0,1) * YEigenVecs(0,0) * eigVal0 +
    eigenVecs(1,1) * YEigenVecs(0,1) * eigVal1 +
    eigenVecs(2,1) * YEigenVecs(0,2) * eigVal2;
  const RealValueType temp11 = eigenVecs(0,1) * YEigenVecs(1,0) * eigVal0 +
    eigenVecs(1,1) * YEigenVecs(1,1) * eigVal1 +
    eigenVecs(2,1) * YEigenVecs(1,2) * eigVal2;
  const RealValueType temp12 = eigenVecs(0,1) * YEigenVecs(2,0) * eigVal0 +
    eigenVecs(1,1) * YEigenVecs(2,1) * eigVal1 +
    eigenVecs(2,1) * YEigenVecs(2,2) * eigVal2;

  const RealValueType temp20 = eigenVecs(0,2) * YEigenVecs(0,0) * eigVal0 +
    eigenVecs(1,2) * YEigenVecs(0,1) * eigVal1 +
    eigenVecs(2,2) * YEigenVecs(0,2) * eigVal2;
  const RealValueType temp21 = eigenVecs(0,2) * YEigenVecs(1,0) * eigVal0 +
    eigenVecs(1,2) * YEigenVecs(1,1) * eigVal1 +
    eigenVecs(2,2) * YEigenVecs(1,2) * eigVal2;
  const RealValueType temp22 = eigenVecs(0,2) * YEigenVecs(2,0) * eigVal0 +
    eigenVecs(1,2) * YEigenVecs(2,1) * eigVal1 +
    eigenVecs(2,2) * YEigenVecs(2,2) * eigVal2;
  symMatrixLogMap[0] = YEigVal0 * temp00 * temp00 + YEigVal1 * temp01 * temp01 + YEigVal2 * temp02 * temp02;
  symMatrixLogMap[1] = YEigVal0 * temp00 * temp10 + YEigVal1 * temp01 * temp11 + YEigVal2 * temp02 * temp12;
  symMatrixLogMap[2] = YEigVal0 * temp00 * temp20 + YEigVal1 * temp01 * temp21 + YEigVal2 * temp02 * temp22;

  symMatrixLogMap[3] = YEigVal0 * temp10 * temp10 + YEigVal1 * temp11 * temp11 + YEigVal2 * temp12 * temp12;
  symMatrixLogMap[4] = YEigVal0 * temp10 * temp20 + YEigVal1 * temp11 * temp21 + YEigVal2 * temp12 * temp22;

  symMatrixLogMap[5] = YEigVal0 * temp20 * temp20 + YEigVal1 * temp21 * temp21 + YEigVal2 * temp22 * temp22;

  RealValueType wt = weight[0];
  geodesicDist[0] = wt * wt * (YEigVal0 * YEigVal0 + YEigVal1 * YEigVal1 + YEigVal2 * YEigVal2);

}

template <typename TInputImage, typename TOutputImage>
typename PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>::RealType
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::AddEuclideanUpdate(const RealType& a,
                     const RealType& b)
{
  RealType result = m_ZeroPixel;

  for( unsigned int pc = 0; pc < m_NumPixelComponents; ++pc )
    {
    this->SetComponent( result, pc,
                 this->GetComponent(a, pc) + this->GetComponent(b, pc) );
    }
  return result;
}

template <typename TInputImage, typename TOutputImage>
typename PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>::RealType
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::AddExponentialMapUpdate(const DiffusionTensor3D<RealValueType>& spdMatrix,
                          const DiffusionTensor3D<RealValueType>& symMatrix)
{
  typedef typename RealType::EigenValuesArrayType   RealEigenValuesArrayType;
  typedef typename RealType::EigenVectorsMatrixType RealEigenVectorsMatrixType;
  RealEigenValuesArrayType   eigenVals;
  RealEigenVectorsMatrixType eigenVecs;
  RealEigenValuesArrayType   YEigenVals;
  RealEigenVectorsMatrixType YEigenVecs;
  RealType                   Y;
  RealType                   expMap;

  if( m_UseFastTensorComputations )
    {
    this->Compute3x3EigenAnalysis(spdMatrix, eigenVals, eigenVecs);
    }
  else
    {
    spdMatrix.ComputeEigenAnalysis(eigenVals, eigenVecs);
    }

  for( unsigned int ii = 0; ii < 3; ++ii )
    {
    eigenVals[ii] = std::max(RealValueType(1e-15), eigenVals[ii]);
    }

  // Note that the ComputeEigenAnalysis returns a row vector for each
  // eigenvector

  // Compute Y = gInv * symMatrix * gInv'
  // where gInv = sqrtEigenValsInvMatrix * eigenVecs
  // and sqrtEigenValsInvMatrix is a diagonal matrix whose entries are 1.0 /
  // sqrt(eigenVals)
  // g = eigenVecs.GetTranspose() * sqrtEigenValsMatrix
  // where sqrtEigenValsMatrix is a diagonal matrix whose entries are
  // sqrt(eigenVals)
  //
  // Since we are always working with DiffusionTensor3D pixels which are always
  // 3x3, these calculations can be optimized as follows.

  RealValueType factor0;
  RealValueType factor1;
  RealValueType factor2;

  factor0 = symMatrix[0] * eigenVecs(0,0) + symMatrix[1] * eigenVecs(0,1) + symMatrix[2] * eigenVecs(0,2);
  factor1 = symMatrix[1] * eigenVecs(0,0) + symMatrix[3] * eigenVecs(0,1) + symMatrix[4] * eigenVecs(0,2);
  factor2 = symMatrix[2] * eigenVecs(0,0) + symMatrix[4] * eigenVecs(0,1) + symMatrix[5] * eigenVecs(0,2);

  Y[0] = ( eigenVecs(0,0) * factor0 + eigenVecs(0,1) * factor1 + eigenVecs(0,2) * factor2 ) / eigenVals[0];
  Y[1] = ( eigenVecs(1,0) * factor0 + eigenVecs(1,1) * factor1 + eigenVecs(1,2) * factor2 ) / std::sqrt(
      eigenVals[0] * eigenVals[1]);
  Y[2] = ( eigenVecs(2,0) * factor0 + eigenVecs(2,1) * factor1 + eigenVecs(2,2) * factor2 ) / std::sqrt(
      eigenVals[0] * eigenVals[2]);

  factor0 = symMatrix[0] * eigenVecs(1,0) + symMatrix[1] * eigenVecs(1,1) + symMatrix[2] * eigenVecs(1,2);
  factor1 = symMatrix[1] * eigenVecs(1,0) + symMatrix[3] * eigenVecs(1,1) + symMatrix[4] * eigenVecs(1,2);
  factor2 = symMatrix[2] * eigenVecs(1,0) + symMatrix[4] * eigenVecs(1,1) + symMatrix[5] * eigenVecs(1,2);

  Y[3] = ( eigenVecs(1,0) * factor0 + eigenVecs(1,1) * factor1 + eigenVecs(1,2) * factor2 ) / eigenVals[1];
  Y[4] = ( eigenVecs(2,0) * factor0 + eigenVecs(2,1) * factor1 + eigenVecs(2,2) * factor2 ) / std::sqrt(
      eigenVals[1] * eigenVals[2]);

  factor0 = symMatrix[0] * eigenVecs(2,0) + symMatrix[1] * eigenVecs(2,1) + symMatrix[2] * eigenVecs(2,2);
  factor1 = symMatrix[1] * eigenVecs(2,0) + symMatrix[3] * eigenVecs(2,1) + symMatrix[4] * eigenVecs(2,2);
  factor2 = symMatrix[2] * eigenVecs(2,0) + symMatrix[4] * eigenVecs(2,1) + symMatrix[5] * eigenVecs(2,2);

  Y[5] = ( eigenVecs(2,0) * factor0 + eigenVecs(2,1) * factor1 + eigenVecs(2,2) * factor2 ) / eigenVals[2];

  // Note that the ComputeEigenAnalysis returns a row vector for each
  // eigenvector

  // Compute expMap = temp * expEigenVals * temp.GetTranspose()
  // where expEigenVals is a diagonal matrix whose entries are exp(eigenVals of
  // Y)
  // and temp = g * (eigenVecs of Y).GetTranspose()
  //
  // Since we are always working with DiffusionTensor3D pixels which are always
  // 3x3, these calculations can be optimized as follows.

  if( m_UseFastTensorComputations )
    {
    this->Compute3x3EigenAnalysis(Y, YEigenVals, YEigenVecs);
    }
  else
    {
    Y.ComputeEigenAnalysis(YEigenVals, YEigenVecs);
    }

  for( unsigned int ii = 0; ii < 3; ++ii )
    {
    YEigenVals[ii] = std::exp(YEigenVals[ii]);
    }
  const RealValueType eigVal0 = std::sqrt(eigenVals[0]);
  const RealValueType eigVal1 = std::sqrt(eigenVals[1]);
  const RealValueType eigVal2 = std::sqrt(eigenVals[2]);
  const RealValueType YEigVal0 = YEigenVals[0];
  const RealValueType YEigVal1 = YEigenVals[1];
  const RealValueType YEigVal2 = YEigenVals[2];

  const RealValueType temp00 = eigenVecs(0,0) * YEigenVecs(0,0) * eigVal0 +
    eigenVecs(1,0) * YEigenVecs(0,1) * eigVal1 +
    eigenVecs(2,0) * YEigenVecs(0,2) * eigVal2;
  const RealValueType temp01 = eigenVecs(0,0) * YEigenVecs(1,0) * eigVal0 +
    eigenVecs(1,0) * YEigenVecs(1,1) * eigVal1 +
    eigenVecs(2,0) * YEigenVecs(1,2) * eigVal2;
  const RealValueType temp02 = eigenVecs(0,0) * YEigenVecs(2,0) * eigVal0 +
    eigenVecs(1,0) * YEigenVecs(2,1) * eigVal1 +
    eigenVecs(2,0) * YEigenVecs(2,2) * eigVal2;

  const RealValueType temp10 = eigenVecs(0,1) * YEigenVecs(0,0) * eigVal0 +
    eigenVecs(1,1) * YEigenVecs(0,1) * eigVal1 +
    eigenVecs(2,1) * YEigenVecs(0,2) * eigVal2;
  const RealValueType temp11 = eigenVecs(0,1) * YEigenVecs(1,0) * eigVal0 +
    eigenVecs(1,1) * YEigenVecs(1,1) * eigVal1 +
    eigenVecs(2,1) * YEigenVecs(1,2) * eigVal2;
  const RealValueType temp12 = eigenVecs(0,1) * YEigenVecs(2,0) * eigVal0 +
    eigenVecs(1,1) * YEigenVecs(2,1) * eigVal1 +
    eigenVecs(2,1) * YEigenVecs(2,2) * eigVal2;

  const RealValueType temp20 = eigenVecs(0,2) * YEigenVecs(0,0) * eigVal0 +
    eigenVecs(1,2) * YEigenVecs(0,1) * eigVal1 +
    eigenVecs(2,2) * YEigenVecs(0,2) * eigVal2;
  const RealValueType temp21 = eigenVecs(0,2) * YEigenVecs(1,0) * eigVal0 +
    eigenVecs(1,2) * YEigenVecs(1,1) * eigVal1 +
    eigenVecs(2,2) * YEigenVecs(1,2) * eigVal2;
  const RealValueType temp22 = eigenVecs(0,2) * YEigenVecs(2,0) * eigVal0 +
    eigenVecs(1,2) * YEigenVecs(2,1) * eigVal1 +
    eigenVecs(2,2) * YEigenVecs(2,2) * eigVal2;
  expMap[0] = YEigVal0 * temp00 * temp00 + YEigVal1 * temp01 * temp01 + YEigVal2 * temp02 * temp02;
  expMap[1] = YEigVal0 * temp00 * temp10 + YEigVal1 * temp01 * temp11 + YEigVal2 * temp02 * temp12;
  expMap[2] = YEigVal0 * temp00 * temp20 + YEigVal1 * temp01 * temp21 + YEigVal2 * temp02 * temp22;

  expMap[3] = YEigVal0 * temp10 * temp10 + YEigVal1 * temp11 * temp11 + YEigVal2 * temp12 * temp12;
  expMap[4] = YEigVal0 * temp10 * temp20 + YEigVal1 * temp11 * temp21 + YEigVal2 * temp12 * temp22;

  expMap[5] = YEigVal0 * temp20 * temp20 + YEigVal1 * temp21 * temp21 + YEigVal2 * temp22 * temp22;

  return expMap;
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::InitializeKernelSigma()
{
  for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
    {
    // Initialize kernel sigma to 30% of the intensity range.
    // if the range is 100, the initial sigma will be 30.
    m_KernelBandwidthSigma[ic] = 30.0 * m_KernelBandwidthMultiplicationFactor /
      m_IntensityRescaleInvFactor[ic];
    }
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::InitializeIteration()
{
  const PatchRadiusType radius = this->GetPatchRadiusInVoxels();

  // Have sampler update any internal structures
  // across the entire image for each iteration
  m_SearchSpaceList->SetImage(this->m_OutputImage);
  m_SearchSpaceList->SetRadius(radius);
  m_Sampler->SetSample(m_SearchSpaceList);

  // Re-initialize thread data struct, especially validity flags
  const size_t structSize = m_ThreadData.size();
  for( unsigned int thread = 0; thread < structSize; ++thread )
    {
    // Reset entropy derivatives that are used to compute optimal sigma
    for (unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic)
      {
      m_ThreadData[thread].validDerivatives[ic] = 0;
      m_ThreadData[thread].entropyFirstDerivative[ic] = 0;
      m_ThreadData[thread].entropySecondDerivative[ic] = 0;
      m_ThreadData[thread].validNorms[ic] = 0;
      m_ThreadData[thread].minNorm[ic] = 0;
      m_ThreadData[thread].maxNorm[ic] = 0;
      }

    // Provide a sampler to the thread
    m_ThreadData[thread].sampler = dynamic_cast<BaseSamplerType*>(m_Sampler->Clone().GetPointer() );
    typename ListAdaptorType::Pointer searchList = ListAdaptorType::New();
    searchList->SetImage(this->m_OutputImage);
    searchList->SetRadius(radius);
    m_ThreadData[thread].sampler->SetSeed(thread);
    m_ThreadData[thread].sampler->SetSample(searchList);
    m_ThreadData[thread].sampler->SetSampleRegion(searchList->GetRegion() );
    }
}

template<typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ApplyUpdate()
{
  // Set up for multithreaded processing.
  ThreadFilterStruct str;
  str.Filter = this;
  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads() );
  this->GetMultiThreader()->SetSingleMethod(this->ApplyUpdateThreaderCallback,
                                            &str);
  // Multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();

  // Explicitly call Modified on GetOutput here since ThreadedApplyUpdate
  // changes this buffer through iterators which don't increment the output
  // timestamp
  this->m_OutputImage->Modified();
}

template <typename TInputImage, typename TOutputImage>
ITK_THREAD_RETURN_TYPE
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ApplyUpdateThreaderCallback( void * arg )
{
  const unsigned int threadId    = ( (MultiThreader::ThreadInfoStruct *)(arg) )->ThreadID;
  const unsigned int threadCount = ( (MultiThreader::ThreadInfoStruct *)(arg) )->NumberOfThreads;

  const ThreadFilterStruct * str =
    (ThreadFilterStruct *)( ( (MultiThreader::ThreadInfoStruct *)(arg) )->UserData);

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Using the SplitRequestedRegion method from itk::ImageSource.
  InputImageRegionType splitRegion;

  const unsigned int total = str->Filter->SplitRequestedRegion(threadId, threadCount,
                                                               splitRegion);

  if( threadId < total )
    {
    str->Filter->ThreadedApplyUpdate(splitRegion, threadId);
    }

  return ITK_THREAD_RETURN_VALUE;
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ThreadedApplyUpdate(const InputImageRegionType &regionToProcess,
                      int itkNotUsed(threadId) )
{
  ImageAlgorithm::Copy(m_UpdateBuffer.GetPointer(), this->m_OutputImage,
                       regionToProcess, regionToProcess);
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ComputeKernelBandwidthUpdate()
{
  // Set up for multithreaded processing.
  ThreadFilterStruct str;

  str.Filter = this;

  // Calculate sigma update
  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads() );
  this->GetMultiThreader()->SetSingleMethod(this->ComputeSigmaUpdateThreaderCallback,
                                            &str);

  m_SigmaConverged.SetSize(m_NumIndependentComponents);
  m_SigmaConverged.Fill(0);

  // Back out the multiplication factor prior to optimizing, then put it back
  // afterwards rescale Gaussian kernel sigma assuming intensity range of 100,
  // then undo afterwards
  for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
    {
    RealValueType scaledVal = m_KernelBandwidthSigma[ic];
    scaledVal /= m_KernelBandwidthMultiplicationFactor;
    scaledVal *= m_IntensityRescaleInvFactor[ic];
    m_KernelBandwidthSigma[ic] = scaledVal;
    }

  RealArrayType sigmaUpdate;

  // Perform Newton-Raphson optimization to find the optimal kernel sigma
  for( unsigned int i = 0; i < MaxSigmaUpdateIterations; ++i )
    {
    itkDebugMacro( "Computing iteration " << i
                                          << " of kernel sigma update" );

    // Multi-threaded computation of the first and second derivatives
    // of the entropy with respect to sigma
    this->GetMultiThreader()->SingleMethodExecute();

    // Accumulate results from each thread and appropriately update sigma after
    // checks to prevent divergence or invalid
    // sigma values
    sigmaUpdate = this->ResolveSigmaUpdate();

    itkDebugMacro( "sigmaUpdate: " << sigmaUpdate
                                   << ", m_KernelBandwidthSigma: " << m_KernelBandwidthSigma
                                   << ", m_SigmaUpdateConvergenceTolerance: " << m_SigmaUpdateConvergenceTolerance );

    // Check for convergence
    bool all_converged = true;
    for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
      {
      if( !m_SigmaConverged[ic] )
        {
        if( itk::Math::abs(sigmaUpdate[ic]) <
            m_KernelBandwidthSigma[ic] * m_SigmaUpdateConvergenceTolerance)
          {
          m_SigmaConverged[ic] = 1;
          }
        else
          {
          all_converged = false;
          }
        }
      }
    if (all_converged)
      {
      break;
      }
    } // end Newton-Raphson iterations

  // Undo rescale of Gaussian kernel sigma put the multiplication factor back
  // in
  for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
    {
    RealValueType scaledVal = m_KernelBandwidthSigma[ic];
    scaledVal /= m_IntensityRescaleInvFactor[ic];
    scaledVal *= m_KernelBandwidthMultiplicationFactor;
    m_KernelBandwidthSigma[ic] = scaledVal;
    }
}

template <typename TInputImage, typename TOutputImage>
ITK_THREAD_RETURN_TYPE
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ComputeSigmaUpdateThreaderCallback( void * arg )
{
  const unsigned int threadId    = ( (MultiThreader::ThreadInfoStruct *)(arg) )->ThreadID;
  const unsigned int threadCount = ( (MultiThreader::ThreadInfoStruct *)(arg) )->NumberOfThreads;

  const ThreadFilterStruct * str =
    (ThreadFilterStruct *)( ( (MultiThreader::ThreadInfoStruct *)(arg) )->UserData);

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Using the SplitRequestedRegion method from itk::ImageSource.
  InputImageRegionType splitRegion;

  const unsigned int total =
    str->Filter->SplitRequestedRegion(threadId, threadCount, splitRegion);

  if( threadId < total )
    {
    str->Filter->SetThreadData(threadId,
                               str->Filter->ThreadedComputeSigmaUpdate(splitRegion, threadId,
                                                                       str->Filter->GetThreadData(threadId) ) );
    }

  return ITK_THREAD_RETURN_VALUE;
}

template <typename TInputImage, typename TOutputImage>
typename PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>::ThreadDataStruct
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ThreadedComputeSigmaUpdate(const InputImageRegionType &regionToProcess,
                             const int itkNotUsed(threadId),
                             ThreadDataStruct threadData)
{
  // Create two images to list adaptors, one for the iteration over the region
  // the other for querying to find the patches set the region of interest for
  // the second to only include patches with at least as many in-bounds
  // neighborsin the same areas of the patch as the query patch
  // initialize accumulators
  // for each element in the region
  //   set region of interest
  //   select a set of patches (random, gaussian, etc)
  //   for each of the patches
  //     compute the difference between the element's patch and the selected
  // patch
  //     compute entropy derivatives using this difference
  //
  typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<OutputImageType>
    FaceCalculatorType;
  typedef typename FaceCalculatorType::FaceListType
    FaceListType;
  typedef typename ListAdaptorType::ConstIterator
    SampleIteratorType;
  typedef typename OutputImageType::IndexType
    IndexType;

  const PatchRadiusType radius = this->GetPatchRadiusInVoxels();

  const OutputImageType *output = this->m_OutputImage;
  const InputImageType * inputImage = this->m_InputImage;

  typename ListAdaptorType::Pointer inList = ListAdaptorType::New();
  inList->SetImage(output);
  inList->SetRadius(radius);

  BaseSamplerPointer sampler = threadData.sampler;

  // Break the input into a series of regions. The first region is free
  // of boundary conditions, the rest with boundary conditions. We operate
  // on the output region because input has been copied to output.
  // For the sigma estimation, we only want to use pixels from the first
  // region to avoid using boundary conditions (and it's faster).

  FaceCalculatorType faceCalculator;
  FaceListType       faceList = faceCalculator(output, regionToProcess, radius);
  typename FaceListType::iterator fIt;

  const unsigned int lengthPatch = this->GetPatchLengthInVoxels();
  const unsigned int center = (lengthPatch - 1) / 2;
  RealArrayType      jointEntropyFirstDerivative (m_NumIndependentComponents);
  RealArrayType      jointEntropySecondDerivative(m_NumIndependentComponents);
  RealArrayType      nbhdEntropyFirstDerivative  (m_NumIndependentComponents);
  RealArrayType      nbhdEntropySecondDerivative (m_NumIndependentComponents);
  jointEntropyFirstDerivative.Fill(0.0);
  jointEntropySecondDerivative.Fill(0.0);
  nbhdEntropyFirstDerivative.Fill(0.0);
  nbhdEntropySecondDerivative.Fill(0.0);

  // Only use pixels whose patch is entirely in bounds
  // for the sigma calculation
  fIt = faceList.begin();
  if( !( fIt->GetNumberOfPixels() ) )
    {
    // Empty region, don't use.
    return threadData;
    }
  inList->SetRegion(*fIt);

  unsigned int sampleNum = 0;
  for( SampleIteratorType sampleIt = inList->Begin();
       sampleIt != inList->End();
       ++sampleIt, ++sampleNum )
    {
    if( sampleNum % m_SigmaUpdateDecimationFactor != 0 )
      {
      // Skip this sample
      continue;
      }
    InputImagePatchIterator currentPatch = sampleIt.GetMeasurementVector()[0];
    IndexType               nIndex = currentPatch.GetIndex();
    InstanceIdentifier      currentPatchId = inList->GetImage()->ComputeOffset(nIndex);

    // Select a set of patches from the full image, excluding points that have
    // neighbors outside the boundary at locations different than that of the
    // current patch.
    // For example, say we have a 7x10 image and current patch index == (0,1)
    // with radius = 2.
    // Then the sampler should be constrained to only select other patches with
    // indices in the range (0:7-2-1,1:10-2-1) == (0:4,1:7)
    // Conversely if the current patch index == (5,8) with radius = 2,
    // the sampler should only select other patches with indices in the range
    // (2:5,2:8)
    // That is, the range formula is min(index,radius):max(index,size-radius-1)

    typename OutputImageType::RegionType region = inputImage->GetLargestPossibleRegion();
    IndexType rIndex;
    typename OutputImageType::SizeType   rSize = region.GetSize();
    for( unsigned int dim = 0; dim < OutputImageType::ImageDimension; ++dim )
      {
      rIndex[dim] = std::min(nIndex[dim],
                                 static_cast<IndexValueType>(radius[dim]) );
      rSize[dim]  = std::max(nIndex[dim],
                                 static_cast<IndexValueType>(rSize[dim] - radius[dim] - 1) ) - rIndex[dim] + 1;
      }
    region.SetIndex(rIndex);
    region.SetSize(rSize);

    typename BaseSamplerType::SubsamplePointer selectedPatches =
      BaseSamplerType::SubsampleType::New();
    sampler->SetRegionConstraint(region);
    sampler->CanSelectQueryOff();
    sampler->Search(currentPatchId, selectedPatches);

    const unsigned int numPatches = selectedPatches->GetTotalFrequency();

    RealArrayType probJointEntropy(m_NumIndependentComponents);
    RealArrayType probJointEntropyFirstDerivative(m_NumIndependentComponents);
    RealArrayType probJointEntropySecondDerivative(m_NumIndependentComponents);
    probJointEntropy.Fill(0.0);
    probJointEntropyFirstDerivative.Fill(0.0);
    probJointEntropySecondDerivative.Fill(0.0);

    RealArrayType probPatchEntropy(m_NumIndependentComponents);
    RealArrayType probPatchEntropyFirstDerivative(m_NumIndependentComponents);
    RealArrayType probPatchEntropySecondDerivative(m_NumIndependentComponents);
    probPatchEntropy.Fill(0.0);
    probPatchEntropyFirstDerivative.Fill(0.0);
    probPatchEntropySecondDerivative.Fill(0.0);

    VariableLengthVector<PixelType> currentPatchVec(lengthPatch);
    // Store the current patch prior to iterating over the selected patches
    // to avoid repeatedly calling GetPixel for this patch
    // because we know we are processing a region whose pixels are all in
    // bounds, we don't need to check this any further when dealing with the
    // patches
    for( unsigned int jj = 0; jj < lengthPatch; ++jj )
      {
      currentPatchVec[jj] = currentPatch.GetPixel(jj);
      }

    IndexType               lastSelectedIdx;
    IndexType               currSelectedIdx;
    InputImagePatchIterator selectedPatch;
    if( numPatches > 0 )
      {
      selectedPatch = selectedPatches->Begin().GetMeasurementVector()[0];
      lastSelectedIdx = selectedPatch.GetIndex();
      }
    else
      {
      InputImagePatchIterator queryIt = sampler->GetSample()->GetMeasurementVector(currentPatchId)[0];
      itkDebugMacro( << "unexpected index for current patch, search results are empty."
                     << "\ncurrent patch id: " << currentPatchId
                     << "\ncurrent patch index: " << nIndex
                     << "\nindex calculated by searcher: "
                     << queryIt.GetIndex(queryIt.GetCenterNeighborhoodIndex() )
                     << "\npatch accessed by searcher: " );
      }

    RealType      centerPatchDifference;
    RealArrayType squaredNorm(m_NumIndependentComponents);
    RealArrayType centerPatchSquaredNorm(m_NumIndependentComponents);
    RealArrayType tmpNorm1(m_NumIndependentComponents);
    RealArrayType tmpNorm2(m_NumIndependentComponents);

    bool useCachedComputations = false;
    for( typename BaseSamplerType::SubsampleConstIterator selectedIt = selectedPatches->Begin();
         selectedIt != selectedPatches->End();
         ++selectedIt )
      {
      currSelectedIdx = selectedIt.GetMeasurementVector()[0].GetIndex();
      selectedPatch += currSelectedIdx - lastSelectedIdx;
      lastSelectedIdx = currSelectedIdx;
      // Since we make sure that the search query can only take place in a
      // certain image region it is sufficient to rely on the fact that the
      // current patch is in bounds

      selectedPatch.NeedToUseBoundaryConditionOff();

      // This partial loop unrolling works because the length of the patch is
      // always odd guaranteeing that center - 0 == lengthPatch - center+1
      // always
      squaredNorm.Fill(0.0);
      for( unsigned int jj = 0, kk = center+1; jj < center; ++jj, ++kk )
        {
        // Rescale intensities, and differences, to a range of 100
        RealType diff1;
        RealType diff2;
        this->ComputeDifferenceAndWeightedSquaredNorm( currentPatchVec[jj],
                                                selectedPatch.GetPixel(jj),
                                                m_IntensityRescaleInvFactor,
                                                useCachedComputations, jj,
                                                threadData.eigenValsCache,
                                                threadData.eigenVecsCache,
                                                diff1, tmpNorm1 );
        this->ComputeDifferenceAndWeightedSquaredNorm( currentPatchVec[kk],
                                                selectedPatch.GetPixel(kk),
                                                m_IntensityRescaleInvFactor,
                                                useCachedComputations, kk,
                                                threadData.eigenValsCache,
                                                threadData.eigenVecsCache,
                                                diff2, tmpNorm2 );
        for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
          {
          squaredNorm[ic] += tmpNorm1[ic];
          squaredNorm[ic] += tmpNorm2[ic];
          }
        }

      // Rescale intensities, and differences, to a range of 100
      this->ComputeDifferenceAndWeightedSquaredNorm( currentPatchVec[center],
                                              selectedPatch.GetPixel(center),
                                              m_IntensityRescaleInvFactor,
                                              useCachedComputations, center,
                                              threadData.eigenValsCache,
                                              threadData.eigenVecsCache,
                                              centerPatchDifference, centerPatchSquaredNorm );
      useCachedComputations = true;

      for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
        {
        squaredNorm[ic] += centerPatchSquaredNorm[ic];

        const RealValueType sigmaKernel = m_KernelBandwidthSigma[ic];
        const RealValueType distanceJointEntropy = std::sqrt(squaredNorm[ic]);

        const RealValueType gaussianJointEntropy =
          exp(-itk::Math::sqr(distanceJointEntropy / sigmaKernel) / 2.0);

        probJointEntropy[ic] += gaussianJointEntropy;

        const RealValueType factorJoint = squaredNorm[ic] / pow(sigmaKernel, 3.0)
          - (lengthPatch * 1 / sigmaKernel);

        probJointEntropyFirstDerivative[ic] += gaussianJointEntropy * factorJoint;
        probJointEntropySecondDerivative[ic] += gaussianJointEntropy *
          (itk::Math::sqr(factorJoint) + (lengthPatch * 1 / itk::Math::sqr(sigmaKernel) ) -
           (3.0 * squaredNorm[ic] / pow(sigmaKernel, 4.0) ) );
        if( m_ComputeConditionalDerivatives )
          {
          const RealValueType distancePatchEntropySquared =
            squaredNorm[ic] - centerPatchSquaredNorm[ic];
          const RealValueType distancePatchEntropy =
            std::sqrt(distancePatchEntropySquared);
          const RealValueType gaussianPatchEntropy =
            exp(-itk::Math::sqr(distancePatchEntropy / sigmaKernel) / 2.0);
          probPatchEntropy[ic] += gaussianPatchEntropy;
          const RealValueType factorPatch =
            distancePatchEntropySquared / pow(sigmaKernel, 3.0)
            - ( (lengthPatch-1) / sigmaKernel);

          probPatchEntropyFirstDerivative[ic] += gaussianPatchEntropy * factorPatch;
          probPatchEntropySecondDerivative[ic] += gaussianPatchEntropy
              * (itk::Math::sqr(factorPatch) +
              ( (lengthPatch-1) / itk::Math::sqr(sigmaKernel) ) -
              (3.0*distancePatchEntropySquared / pow(sigmaKernel, 4.0) ) );
          }
        } // end for each independent pixel component
      }   // end for each selected patch

    for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
      {
      if( !m_SigmaConverged[ic] )
        {
        probJointEntropy[ic] /= numPatches;
        probJointEntropy[ic] += m_MinProbability;
        probJointEntropyFirstDerivative[ic] /= numPatches;
        probJointEntropyFirstDerivative[ic] += m_MinProbability;
        probJointEntropySecondDerivative[ic] /= numPatches;
        probJointEntropySecondDerivative[ic] += m_MinProbability;

        itkAssertOrThrowMacro(probJointEntropy[ic] > 0.0, "probJointEntropy must be > 0.0");

        jointEntropyFirstDerivative[ic] -=
          probJointEntropyFirstDerivative[ic] / probJointEntropy[ic];
        jointEntropySecondDerivative[ic] -=
          probJointEntropySecondDerivative[ic] / probJointEntropy[ic]
        - itk::Math::sqr(probJointEntropyFirstDerivative[ic] / probJointEntropy[ic]);

        if( m_ComputeConditionalDerivatives )
          {
          probPatchEntropy[ic] /= numPatches;
          probPatchEntropy[ic] += m_MinProbability;
          probPatchEntropyFirstDerivative[ic] /= numPatches;
          probPatchEntropyFirstDerivative[ic] += m_MinProbability;
          probPatchEntropySecondDerivative[ic] /= numPatches;
          probPatchEntropySecondDerivative[ic] += m_MinProbability;

          itkAssertOrThrowMacro(probPatchEntropy[ic] > 0.0, "probPatchEntropy must be > 0.0");

          nbhdEntropyFirstDerivative[ic] -=
            probPatchEntropyFirstDerivative[ic] / probPatchEntropy[ic];
          nbhdEntropySecondDerivative[ic] -=
            probPatchEntropySecondDerivative[ic] / probPatchEntropy[ic]
          - itk::Math::sqr(probPatchEntropyFirstDerivative[ic] / probPatchEntropy[ic]);
          }
        } // end if independent component hasn't converged yet
      }   // end for each independent component
    }     // end for each pixel in the sample

  for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
    {
    if( !m_SigmaConverged[ic] )
      {
      if( m_ComputeConditionalDerivatives )
        {
        threadData.entropyFirstDerivative[ic] =
          jointEntropyFirstDerivative[ic]  - nbhdEntropyFirstDerivative[ic];
        threadData.entropySecondDerivative[ic] =
          jointEntropySecondDerivative[ic] - nbhdEntropySecondDerivative[ic];
        }
      else
        {
        threadData.entropyFirstDerivative[ic]  = jointEntropyFirstDerivative[ic];
        threadData.entropySecondDerivative[ic] = jointEntropySecondDerivative[ic];
        }
      threadData.validDerivatives[ic] = 1;
      }
    else
      {
      threadData.validDerivatives[ic] = 0;
      }
    } // end for each independent component
  return threadData;
}

template <typename TInputImage, typename TOutputImage>
typename PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>::RealArrayType
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ResolveSigmaUpdate()
{
  RealArrayType sigmaUpdate(m_NumIndependentComponents);

  sigmaUpdate.Fill(0);

  for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
    {
    if( m_SigmaConverged[ic] )
      {
      // Don't need to resolve anything, go on to the next component
      continue;
      }

    itkDebugMacro( << "Resolving sigma update for pixel component " << ic );

    // Accumulate the first and second derivatives from all the threads
    const RealValueType kernelSigma = m_KernelBandwidthSigma[ic];
    RealValueType       firstDerivative = 0;
    RealValueType       secondDerivative = 0;
    const size_t numThreads = m_ThreadData.size();
    for( unsigned int threadNum = 0; threadNum < numThreads; ++threadNum )
      {
      if( m_ThreadData[threadNum].validDerivatives[ic] > 0 )
        {
        firstDerivative += m_ThreadData[threadNum].entropyFirstDerivative[ic];
        secondDerivative += m_ThreadData[threadNum].entropySecondDerivative[ic];
        }
      }
    firstDerivative /= static_cast<RealValueType>(m_TotalNumberPixels);
    secondDerivative /= static_cast<RealValueType>(m_TotalNumberPixels);

    itkDebugMacro(  << "first deriv: " << firstDerivative
                    << ", second deriv: " << secondDerivative
                    << ", old sigma: " << kernelSigma );

    // If second derivative is zero or negative, compute update using gradient
    // descent
    if( (Math::ExactlyEquals(itk::Math::abs(secondDerivative), NumericTraits<RealValueType>::ZeroValue())) ||
         (secondDerivative < 0) )
      {
      itkDebugMacro( << "** Second derivative NOT POSITIVE" );
      sigmaUpdate[ic] = -itk::Math::sgn(firstDerivative) * kernelSigma * 0.3;
      }
    else
      {
      // Compute update using Newton-Raphson
      sigmaUpdate[ic] = -firstDerivative / secondDerivative;
      }

    itkDebugMacro( << "update: " << sigmaUpdate[ic] );

    // Avoid very large updates to prevent instabilities in Newton-Raphson
    if( itk::Math::abs(sigmaUpdate[ic]) > kernelSigma * 0.3 )
      {
      itkDebugMacro( << "** Restricting large updates \n");
      sigmaUpdate[ic] = itk::Math::sgn(sigmaUpdate[ic]) * kernelSigma * 0.3;

      itkDebugMacro( << "update: " << sigmaUpdate[ic] );
      }

    // keep the updated sigma from being too close to zero
    if( kernelSigma + sigmaUpdate[ic] < m_MinSigma )
      {
      itkDebugMacro( << "** TOO SMALL SIGMA: adjusting sigma" );
      m_KernelBandwidthSigma[ic] = (kernelSigma + m_MinSigma) / 2.0;

      itkDebugMacro( << "new sigma: " << m_KernelBandwidthSigma[ic] );
      }
    else
      {
      m_KernelBandwidthSigma[ic] = kernelSigma + sigmaUpdate[ic];

      itkDebugMacro( << "new sigma: " << m_KernelBandwidthSigma[ic] );

      }
    } // end for each independent pixel component

  return sigmaUpdate;
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ComputeImageUpdate()
{
  // Set up for multithreaded processing.
  ThreadFilterStruct str;

  str.Filter = this;

  // Compute smoothing updated for intensites at each pixel
  // based on gradient of the joint entropy
  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads() );
  this->GetMultiThreader()->SetSingleMethod(this->ComputeImageUpdateThreaderCallback,
                                            &str);

  // Multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();
}

template <typename TInputImage, typename TOutputImage>
ITK_THREAD_RETURN_TYPE
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ComputeImageUpdateThreaderCallback( void * arg )
{
  const unsigned int threadId = ( (MultiThreader::ThreadInfoStruct *)(arg) )->ThreadID;
  const unsigned int threadCount = ( (MultiThreader::ThreadInfoStruct *)(arg) )->NumberOfThreads;

  const ThreadFilterStruct *str =
    (ThreadFilterStruct *)( ( (MultiThreader::ThreadInfoStruct *)(arg) )->UserData);

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Using the SplitRequestedRegion method from itk::ImageSource.
  InputImageRegionType splitRegion;

  const unsigned int total =
    str->Filter->SplitRequestedRegion(threadId, threadCount, splitRegion);

  if (threadId < total)
    {
    str->Filter->SetThreadData(threadId,
                               str->Filter->ThreadedComputeImageUpdate(splitRegion, threadId,
                                                                       str->Filter->GetThreadData(threadId) ) );
    }

  return ITK_THREAD_RETURN_VALUE;
}

template <typename TInputImage, typename TOutputImage>
typename PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>::ThreadDataStruct
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ThreadedComputeImageUpdate(const InputImageRegionType &regionToProcess,
                             const int threadId,
                             ThreadDataStruct threadData)
{
  // Create two images to list adaptors, one for the iteration over the region
  // the other for querying to find the patches set the region of interest for
  // the second to only include patches with as many in-bounds neighbors in the
  // same areas of the query patch
  // initialize accumulators
  // for each element in the region
  //   set region of interest
  //   select a set of patches (random, gaussian, etc)
  //   for each of the patches
  //     compute the difference between the element's patch and the selected
  // patch
  //     calculate the gradient of the joint entropy using this difference
  //
  typedef typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<OutputImageType>
    FaceCalculatorType;
  typedef typename FaceCalculatorType::FaceListType
    FaceListType;

  typedef typename ListAdaptorType::ConstIterator SampleIteratorType;
  typedef GaussianOperator<RealValueType,
                           OutputImageType::ImageDimension> GaussianOperatorType;

  const PatchRadiusType radius = this->GetPatchRadiusInVoxels();

  OutputImageType *     output = this->m_OutputImage;
  const InputImageType *inputImage = this->m_InputImage;

  typename ListAdaptorType::Pointer inList = ListAdaptorType::New();
  inList->SetImage(output);
  inList->SetRadius(radius);

  BaseSamplerPointer sampler = threadData.sampler;

  ProgressReporter progress(this, threadId, regionToProcess.GetNumberOfPixels() );

  // Break the input into a series of regions.  The first region is free
  // of boundary conditions, the rest with boundary conditions.  We operate
  // on the output region because input has been copied to output

  FaceCalculatorType faceCalculator;

  FaceListType faceList = faceCalculator(output, regionToProcess, radius);
  typename FaceListType::iterator fIt;

  for( fIt = faceList.begin(); fIt != faceList.end(); ++fIt )
    {

    if( !( fIt->GetNumberOfPixels() ) )
      {
      // Empty region, move on to next
      continue;
      }

    inList->SetRegion(*fIt);

    // Needed because the modified Bessel functions are protected member
    // functions of GaussianOperator for some strange reason instead of being
    // their own proper functions.
    GaussianOperatorType gOper;

    InputImageRegionConstIteratorType inputIt(inputImage, *fIt);
    OutputImageRegionIteratorType     updateIt(m_UpdateBuffer, *fIt);
    OutputImageRegionIteratorType     outputIt(output, *fIt);

    updateIt.GoToBegin();
    outputIt.GoToBegin();
    inputIt.GoToBegin();

    for( SampleIteratorType sampleIt = inList->Begin();
      sampleIt != inList->End(); ++sampleIt )
      {
      RealType result = outputIt.Get();

      const double smoothingWeight = this->GetSmoothingWeight();
      if( smoothingWeight > 0 )
        {
        // Get intensity update driven by patch-based denoiser
        const RealType gradientJointEntropy =
          this->ComputeGradientJointEntropy(sampleIt.GetInstanceIdentifier(), inList, sampler,
          threadData);

        const RealValueType stepSizeSmoothing = 0.2;
        result = AddUpdate(result,  gradientJointEntropy * (smoothingWeight * stepSizeSmoothing) );
        } // end if smoothingWeight > 0

      const double fidelityWeight = this->GetNoiseModelFidelityWeight();
      if( fidelityWeight > 0 )
        {
        // We should never have fidelity weight > 0 in the non-Euclidean case
        // so don't bother checking for component space here
        const PixelType in  = inputIt.Get();
        const PixelType out = outputIt.Get();
        switch( this->GetNoiseModel() )
          {
          case Superclass::NOMODEL:
            {
            // Do nothing
            break;
            }
          case Superclass::GAUSSIAN:
            {
            for( unsigned int pc = 0; pc < m_NumPixelComponents; ++pc )
              {
              const RealValueType gradientFidelity = 2.0 *
                ( this->GetComponent(in,pc) - this->GetComponent(out,pc) );
              const RealValueType stepSizeFidelity = 0.5;
              const RealValueType noiseVal = fidelityWeight * ( stepSizeFidelity * gradientFidelity );
              this->SetComponent(result, pc,
                           this->GetComponent(result, pc) + noiseVal);
              }
            break;
            }
          case Superclass::RICIAN:
            {
            for( unsigned int pc = 0; pc < m_NumPixelComponents; ++pc )
              {
              const PixelValueType inVal = this->GetComponent(in, pc);
              const PixelValueType outVal = this->GetComponent(out, pc);
              const RealValueType  sigmaSquared =
                this->GetComponent(m_NoiseSigmaSquared, pc);

              const RealValueType alpha = inVal * outVal / sigmaSquared;
              const RealValueType gradientFidelity =
                (inVal * (gOper.ModifiedBesselI1(alpha) / gOper.ModifiedBesselI0(alpha) ) - outVal)
                  / sigmaSquared;
              const RealValueType stepSizeFidelity = sigmaSquared;
              // Update
              const RealValueType noiseVal =
                fidelityWeight * ( stepSizeFidelity * gradientFidelity );
              // Ensure that the result is nonnegative
              this->SetComponent(result, pc,
                std::max(this->GetComponent(result, pc) + noiseVal,
                static_cast<RealValueType>(0.0) ) );
              }
            break;
            }
          case Superclass::POISSON:
            {
            for( unsigned int pc = 0; pc < m_NumPixelComponents; ++pc )
              {
              const PixelValueType inVal = this->GetComponent(in, pc);
              const PixelValueType outVal = this->GetComponent(out, pc);

              const RealValueType gradientFidelity = (inVal - outVal) /
                (outVal + 0.00001);
              // Prevent large unstable updates when out[pc] less than 1
              const RealValueType stepSizeFidelity =
                std::min(outVal, static_cast<PixelValueType>(0.99999) ) + 0.00001;
              // Update
              const RealValueType noiseVal =
                fidelityWeight * ( stepSizeFidelity * gradientFidelity );
              // Ensure that the result is positive
              this->SetComponent(result, pc,
                std::max(this->GetComponent(result, pc) + noiseVal,
                static_cast<RealValueType>(0.00001) ) );

              }
            break;
            }
          default:
            {
            itkExceptionMacro(<< "Unexpected noise model " << this->GetNoiseModel() << " specified.");
            break;
            }
          }
        } // end if fidelityWeight > 0

      // Set update value, because we can't change the output until the other
      // threads are done using it.
      // UpdateBuffer contains the current update (referred to by updateIt)
      // this->GetInput() contains the original data (referred to by inputIt)
      // this->GetOutput() contains results from the previous iteration
      // (referred to by outputIt) at the end of this update, update.
      // It will hold the current iteration image
      // this->ApplyUpdate() will then copy the results to outputIt since we
      // have to wait until all threads have finished using outputIt to
      // compute the update
      updateIt.Set(static_cast<PixelType>(result) );

      ++updateIt;
      ++outputIt;
      ++inputIt;

      progress.CompletedPixel();
      } // end for each pixel in the sample
    }   // end for each face in the face list
  return threadData;
}

template <typename TInputImage, typename TOutputImage>
typename PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>::RealType
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::ComputeGradientJointEntropy(InstanceIdentifier id,
                              typename ListAdaptorType::Pointer& inList,
                              BaseSamplerPointer& sampler,
                              ThreadDataStruct& threadData)
{
  typedef typename OutputImageType::IndexType IndexType;

  const InputImagePatchIterator currentPatch = inList->GetMeasurementVector(id)[0];
  const typename OutputImageType::IndexType nIndex = currentPatch.GetIndex();
  const InstanceIdentifier currentPatchId = inList->GetImage()->ComputeOffset(nIndex);

  const unsigned int lengthPatch = this->GetPatchLengthInVoxels();
  const unsigned int center = (lengthPatch - 1) / 2;

  const typename OutputImageType::Pointer output = this->m_OutputImage;
  typename OutputImageType::RegionType region =
    this->m_InputImage->GetLargestPossibleRegion();
  IndexType rIndex;
  typename OutputImageType::SizeType rSize = region.GetSize();
  const PatchRadiusType radius = this->GetPatchRadiusInVoxels();
  for( unsigned int dim = 0; dim < OutputImageType::ImageDimension; ++dim )
    {
    rIndex[dim] = std::min(nIndex[dim], static_cast<IndexValueType>(radius[dim]) );
    rSize[dim]  = std::max(nIndex[dim], static_cast<IndexValueType>(rSize[dim] - radius[dim] - 1) )
      - rIndex[dim] + 1;
    }
  region.SetIndex(rIndex);
  region.SetSize(rSize);

  typename BaseSamplerType::SubsamplePointer selectedPatches =
    BaseSamplerType::SubsampleType::New();

  sampler->SetRegionConstraint(region);
  sampler->CanSelectQueryOn();
  sampler->Search(currentPatchId, selectedPatches);

  const unsigned int numPatches = selectedPatches->GetTotalFrequency();

  RealType                             centerPatchDifference = m_ZeroPixel;
  VariableLengthVector<PixelType>      currentPatchVec(lengthPatch);
  VariableLengthVector<unsigned short> isInBoundsVec(lengthPatch);
  VariableLengthVector<RealArrayType>  patchWeightVec(lengthPatch);
  const PatchWeightsType               patchWeights = this->GetPatchWeights();

  // Store the current patch prior to iterating over the selected patches
  // to avoid repeatedly calling GetPixel for this patch.
  //
  // Determine which pixels are InBounds for the patch around the pixel being
  // denoised.
  // Distances for this patch are computed only based on pixels that are
  // InBounds.
  // As a result, the values of pixels outside the boundary is ignored, which
  // effectively ignores the boundary condition associated with the
  // ListAdaptorType

  for( unsigned int jj = 0; jj < lengthPatch; ++jj )
    {
    bool isInBounds;
    currentPatchVec[jj] = currentPatch.GetPixel(jj, isInBounds);
    patchWeightVec[jj].SetSize(m_NumIndependentComponents);
    patchWeightVec[jj].Fill(patchWeights[jj]);
    if( isInBounds )
      {
      isInBoundsVec[jj] = true;
      }
    else
      {
      isInBoundsVec[jj] = false;
      }
    }

  IndexType               lastSelectedIdx;
  IndexType               currSelectedIdx;
  InputImagePatchIterator selectedPatch;
  if( numPatches > 0 )
    {
    selectedPatch = selectedPatches->Begin().GetMeasurementVector()[0];
    lastSelectedIdx = selectedPatch.GetIndex();
    }

  RealValueType sumOfGaussiansJointEntropy = 0.0;

  RealType gradientJointEntropy = m_ZeroPixel;

  RealArrayType squaredNorm(m_NumIndependentComponents);
  RealArrayType centerPatchSquaredNorm(m_NumIndependentComponents);
  RealArrayType tmpNorm1(m_NumIndependentComponents);
  RealArrayType tmpNorm2(m_NumIndependentComponents);

  bool useCachedComputations = false;

  for( typename BaseSamplerType::SubsampleConstIterator selectedIt = selectedPatches->Begin();
       selectedIt != selectedPatches->End();
       ++selectedIt )
    {
    currSelectedIdx = selectedIt.GetMeasurementVector()[0].GetIndex();
    selectedPatch += currSelectedIdx - lastSelectedIdx;
    lastSelectedIdx = currSelectedIdx;

    RealValueType distanceJointEntropy = 0.0;

    squaredNorm.Fill(0.0);
    // Compute difference between selectedPatches[ii] and currentPatch
    if( currentPatch.InBounds() )
      {
      // since we make sure that the search query can only take place in a
      // certain image region.
      // It is sufficient to rely on the fact that the current patch is in
      // bounds.
      selectedPatch.NeedToUseBoundaryConditionOff();

      // This partial loop unrolling works because the length of the patch is
      // always odd guaranteeing that center - 0 == lengthPatch - center+1
      // always
      for( unsigned int jj = 0, kk = center+1; jj < center; ++jj, ++kk )
        {
        RealType diff1;
        RealType diff2;
        this->ComputeDifferenceAndWeightedSquaredNorm( currentPatchVec[jj],
                                                selectedPatch.GetPixel(jj),
                                                patchWeightVec[jj],
                                                useCachedComputations, jj,
                                                threadData.eigenValsCache,
                                                threadData.eigenVecsCache,
                                                diff1, tmpNorm1 );
        this->ComputeDifferenceAndWeightedSquaredNorm( currentPatchVec[kk],
                                                selectedPatch.GetPixel(kk),
                                                patchWeightVec[kk],
                                                useCachedComputations, kk,
                                                threadData.eigenValsCache,
                                                threadData.eigenVecsCache,
                                                diff2, tmpNorm2 );

        for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
          {
          squaredNorm[ic] += tmpNorm1[ic];
          squaredNorm[ic] += tmpNorm2[ic];
          }
        }
      // Now compute the center value
      this->ComputeDifferenceAndWeightedSquaredNorm( currentPatchVec[center],
                                              selectedPatch.GetPixel(center),
                                              patchWeightVec[center],
                                              useCachedComputations,
                                              center,
                                              threadData.eigenValsCache,
                                              threadData.eigenVecsCache,
                                              centerPatchDifference,
                                              centerPatchSquaredNorm );
      for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
        {
        squaredNorm[ic] += centerPatchSquaredNorm[ic];
        }
      }
    else
      {
      // Since we make sure that the search query can only take place in a
      // certain image region the randomly selected patch will be at least
      // as in bounds as the current patch.
      selectedPatch.NeedToUseBoundaryConditionOff();
      for( unsigned int jj = 0, kk= center+1; jj < center; ++jj, ++kk )
        {
        if( isInBoundsVec[jj] )
          {
          RealType diff;
          this->ComputeDifferenceAndWeightedSquaredNorm( currentPatchVec[jj],
                                                  selectedPatch.GetPixel(jj),
                                                  patchWeightVec[jj],
                                                  useCachedComputations, jj,
                                                  threadData.eigenValsCache,
                                                  threadData.eigenVecsCache,
                                                  diff, tmpNorm1 );
          for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
            {
            squaredNorm[ic] += tmpNorm1[ic];
            }
          }
        if( isInBoundsVec[kk] )
          {
          RealType diff;
          this->ComputeDifferenceAndWeightedSquaredNorm( currentPatchVec[kk],
                                                  selectedPatch.GetPixel(kk),
                                                  patchWeightVec[kk],
                                                  useCachedComputations, kk,
                                                  threadData.eigenValsCache,
                                                  threadData.eigenVecsCache,
                                                  diff, tmpNorm1 );
          for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
            {
            squaredNorm[ic] += tmpNorm1[ic];
            }
          }
        }

      // Compute the center value (center is always in bounds)
      this->ComputeDifferenceAndWeightedSquaredNorm( currentPatchVec[center],
                                              selectedPatch.GetPixel(center),
                                              patchWeightVec[center],
                                              useCachedComputations, center,
                                              threadData.eigenValsCache,
                                              threadData.eigenVecsCache,
                                              centerPatchDifference, centerPatchSquaredNorm );
      for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
        {
        squaredNorm[ic] += centerPatchSquaredNorm[ic];
        }
      } // end if entire patch is inbounds

    useCachedComputations = true;

    RealValueType gaussianJointEntropy = NumericTraits<RealValueType>::ZeroValue();
    for( unsigned int ic = 0; ic < m_NumIndependentComponents; ++ic )
      {
      RealValueType kernelSigma = m_KernelBandwidthSigma[ic];

      distanceJointEntropy += squaredNorm[ic] / itk::Math::sqr(kernelSigma);

      gaussianJointEntropy = exp( -distanceJointEntropy / 2.0);
      sumOfGaussiansJointEntropy += gaussianJointEntropy;
      }
    for( unsigned int pc = 0; pc < m_NumPixelComponents; ++pc )
      {
      this->SetComponent(gradientJointEntropy, pc,
                   GetComponent(gradientJointEntropy, pc) + GetComponent(centerPatchDifference,
                                                                         pc) * gaussianJointEntropy);
      }
    } // end for each selected patch

  for( unsigned int pc = 0; pc < m_NumPixelComponents; ++pc)
    {
    this->SetComponent(gradientJointEntropy, pc,
                 GetComponent(gradientJointEntropy, pc) / (sumOfGaussiansJointEntropy + m_MinProbability) );
    }

  return gradientJointEntropy;
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::PostProcessOutput()
{
}

template <typename TInputImage, typename TOutputImage>
void
PatchBasedDenoisingImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NumPixelComponents: " << m_NumPixelComponents << std::endl;
  os << indent << "NumIndependentComponents: " << m_NumIndependentComponents
    << std::endl;
  os << indent << "Total number of pixels: " << m_TotalNumberPixels
    << std::endl;

  os << indent << "PatchRadius (voxel space): ";
  if( this->m_InputImage != ITK_NULLPTR )
    {
    os << this->GetPatchRadiusInVoxels() << std::endl;
    }
  else
    {
    os << "(Cannot be computed: input not set)" << std::endl;
    }

  if( m_UseSmoothDiscPatchWeights )
    {
    os << indent << "UseSmoothDiscPatchWeights: On" << std::endl;
    }
  else
    {
    os << indent << "UseSmoothDiscPatchWeights: Off" << std::endl;
    }

  if( m_UseFastTensorComputations )
    {
    os << indent << "UseFastTensorComputations: On" << std::endl;
    }
  else
    {
    os << indent << "UseFastTensorComputations: Off" << std::endl;
    }

  os << indent << "Kernel bandwidth sigma: "
     << m_KernelBandwidthSigma << std::endl;
  if( m_KernelBandwidthSigmaIsSet )
    {
    os << indent << "KernelBandwidthSigmaIsSet: On" << std::endl;
    }
  else
    {
    os << indent << "KernelBandwidthSigmaIsSet: Off" << std::endl;
    }

  os << indent << "IntensityRescaleInvFactor: " << m_IntensityRescaleInvFactor
    << std::endl;

  os << indent << "ZeroPixel: " << m_ZeroPixel << std::endl;
  os << indent << "ImageMin: " << m_ImageMin << std::endl;
  os << indent << "ImageMax: " << m_ImageMax << std::endl;

  os << indent << "KernelBandwidthFractionPixelsForEstimation: "
     << m_KernelBandwidthFractionPixelsForEstimation << std::endl;

  if( m_ComputeConditionalDerivatives )
    {
    os << indent << "ComputeConditionalDerivatives: On" << std::endl;
    }
  else
    {
    os << indent << "ComputeConditionalDerivatives: Off" << std::endl;
    }

  os << indent << "Min sigma: " << m_MinSigma << std::endl;
  os << indent << "Min probability: " << m_MinProbability << std::endl;

  os << indent << "SigmaUpdateDecimationFactor: "
     << m_SigmaUpdateDecimationFactor << std::endl;
  os << indent << "Sigma update convergence tolerance: "
     << m_SigmaUpdateConvergenceTolerance << std::endl;
  os << indent << "SigmaConverged: " << m_SigmaConverged << std::endl;
  os << indent << "KernelBandwidthMultiplicationFactor: "
     << m_KernelBandwidthMultiplicationFactor << std::endl;

  os << indent << "NoiseSigma: " << m_NoiseSigma << std::endl;
  os << indent << "NoiseSigmaSquared: " << m_NoiseSigmaSquared << std::endl;
  if( m_NoiseSigmaIsSet )
    {
    os << indent << "NoiseSigmaIsSet: On" << std::endl;
    }
  else
    {
    os << indent << "NoiseSigmaIsSet: Off" << std::endl;
    }

  itkPrintSelfObjectMacro( Sampler );
  itkPrintSelfObjectMacro( UpdateBuffer );
}

} // end namespace itk

#endif
