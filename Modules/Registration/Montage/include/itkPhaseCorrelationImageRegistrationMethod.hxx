/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkPhaseCorrelationImageRegistrationMethod_hxx
#define itkPhaseCorrelationImageRegistrationMethod_hxx


#include "itkMath.h"
#include "itkNumericTraits.h"

#include <algorithm>
#include <cmath>

namespace itk
{
template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::
  PhaseCorrelationImageRegistrationMethod()
{
  this->SetNumberOfRequiredInputs(2);
  this->SetNumberOfRequiredOutputs(2); // for 0-the Transform, 1-the phase correlation image

  m_BandPassFilter->SetFunctor(m_IdentityFunctor);

  m_FixedConstantPadder->SetConstant(NumericTraits<FixedImagePixelType>::Zero);
  m_MovingConstantPadder->SetConstant(NumericTraits<MovingImagePixelType>::Zero);
  m_FixedMirrorWEDPadder->SetDecayBase(0.75);
  m_MovingMirrorWEDPadder->SetDecayBase(0.75);

  m_BandPassFunctor = [this](typename BandBassFilterType::FrequencyIteratorType & freqIt) {
    double f2 = freqIt.GetFrequencyModuloSquare(); // square of scalar frequency
    freqIt.Value() *= 1.0 - 1.0 / (1.0 + std::pow(f2 / this->m_LowFrequency2, this->m_ButterworthOrder));
    freqIt.Value() /= 1.0 + std::pow(f2 / this->m_HighFrequency2, this->m_ButterworthOrder);
  };
  m_HighPassFunctor = [this](typename BandBassFilterType::FrequencyIteratorType & freqIt) {
    double f2 = freqIt.GetFrequencyModuloSquare(); // square of scalar frequency
    freqIt.Value() *= 1.0 - 1.0 / (1.0 + std::pow(f2 / this->m_LowFrequency2, this->m_ButterworthOrder));
  };
  m_LowPassFunctor = [this](typename BandBassFilterType::FrequencyIteratorType & freqIt) {
    double f2 = freqIt.GetFrequencyModuloSquare(); // square of scalar frequency
    freqIt.Value() /= 1.0 + std::pow(f2 / this->m_HighFrequency2, this->m_ButterworthOrder);
  };

  m_PadToSize.Fill(0);
  m_ObligatoryPadding.Fill(8);
  m_PaddingMethod = PaddingMethodEnum::Zero;                       // make sure the next call does modifications
  SetPaddingMethod(PaddingMethodEnum::MirrorWithExponentialDecay); // this initializes a few things

  m_TransformParameters = ParametersType(ImageDimension);
  m_TransformParameters.Fill(0.0f);

  TransformOutputPointer transformDecorator = static_cast<TransformOutputType *>(this->MakeOutput(0).GetPointer());
  this->ProcessObject::SetNthOutput(0, transformDecorator.GetPointer());

  typename RealImageType::Pointer phaseCorrelation = static_cast<RealImageType *>(this->MakeOutput(1).GetPointer());
  this->ProcessObject::SetNthOutput(1, phaseCorrelation.GetPointer());
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::SetPaddingMethod(
  const PaddingMethodEnum paddingMethod)
{
  if (this->m_PaddingMethod != paddingMethod)
  {
    this->m_PaddingMethod = paddingMethod;

    switch (paddingMethod)
    {
      case PaddingMethodEnum::Zero:
        m_FixedPadder = m_FixedConstantPadder;
        m_MovingPadder = m_MovingConstantPadder;
        break;
      case PaddingMethodEnum::Mirror:
        m_FixedPadder = m_FixedMirrorPadder;
        m_MovingPadder = m_MovingMirrorPadder;
        break;
      case PaddingMethodEnum::MirrorWithExponentialDecay:
        m_FixedPadder = m_FixedMirrorWEDPadder;
        m_MovingPadder = m_MovingMirrorWEDPadder;
        break;
      default:
        itkExceptionMacro("Unknown padding method");
        break;
    }

    m_FixedFFT->SetInput(m_FixedPadder->GetOutput());
    m_MovingFFT->SetInput(m_MovingPadder->GetOutput());
    this->Modified();
  }
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::Initialize()
{
  itkDebugMacro("initializing registration");
  if (!m_FixedImage)
  {
    itkExceptionMacro(<< "FixedImage is not present");
  }
  if (!m_MovingImage)
  {
    itkExceptionMacro(<< "MovingImage is not present");
  }
  if (!m_Operator)
  {
    itkExceptionMacro(<< "Operator is not present");
  }
  if (!m_Optimizer)
  {
    itkExceptionMacro(<< "Optimizer is not present");
  }

  // Connect new transform to the Decorator if necessary.
  TransformOutputPointer transformOutput(static_cast<TransformOutputType *>(this->ProcessObject::GetOutput(0)));
  TransformPointer       transform(const_cast<TransformType *>(transformOutput->Get()));

  if (transform.IsNull())
  {
    transform = TransformType::New();
    transformOutput->Set(transform.GetPointer());
  }

  // set up the pipeline
  m_FixedRoI->SetInput(m_FixedImage);
  m_MovingRoI->SetInput(m_MovingImage);
  if (m_CropToOverlap)
  {
    m_FixedPadder->SetInput(m_FixedRoI->GetOutput());
    m_MovingPadder->SetInput(m_MovingRoI->GetOutput());
  }
  else
  {
    m_FixedPadder->SetInput(m_FixedImage);
    m_MovingPadder->SetInput(m_MovingImage);
  }
  if (m_FixedImageFFT.IsNull())
  {
    m_Operator->SetFixedImage(m_FixedFFT->GetOutput());
  }
  else
  {
    m_Operator->SetFixedImage(m_FixedImageFFT);
  }
  if (m_MovingImageFFT.IsNull())
  {
    m_Operator->SetMovingImage(m_MovingFFT->GetOutput());
  }
  else
  {
    m_Operator->SetMovingImage(m_MovingImageFFT);
  }
  m_BandPassFilter->SetInput(m_Operator->GetOutput());

  using ImageFilter = ImageToImageFilter<ComplexImageType, ComplexImageType>;
  ImageFilter * finalOperatorFilter = m_BandPassFilter;

  if (m_LowFrequency2 > 0.0 && m_HighFrequency2 > 0.0)
  {
    m_BandPassFilter->SetFunctor(m_BandPassFunctor);
  }
  else if (m_HighFrequency2 > 0.0)
  {
    m_BandPassFilter->SetFunctor(m_HighPassFunctor);
  }
  else if (m_LowFrequency2 > 0.0)
  {
    m_BandPassFilter->SetFunctor(m_LowPassFunctor);
  }
  else // neither high nor low filtering is set
  {
    m_BandPassFilter->SetFunctor(m_IdentityFunctor);
    finalOperatorFilter = m_Operator; // we skip the band-pass entirely
  }

  m_Optimizer->SetComplexInput(finalOperatorFilter->GetOutput());
  m_IFFT->SetInput(finalOperatorFilter->GetOutput());
  m_Optimizer->SetRealInput(m_IFFT->GetOutput());
  if (m_CropToOverlap)
  {
    m_Optimizer->SetFixedImage(m_FixedRoI->GetOutput());
    m_Optimizer->SetMovingImage(m_MovingRoI->GetOutput());
  }
  else
  {
    m_Optimizer->SetFixedImage(m_FixedImage);
    m_Optimizer->SetMovingImage(m_MovingImage);
  }
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
typename PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::SizeType
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::RoundUpToFFTSize(SizeType size)
{
  // FFTs are faster when image size can be factorized using smaller prime numbers
  const auto sizeGreatestPrimeFactor = std::min<SizeValueType>(5, m_FixedFFT->GetSizeGreatestPrimeFactor());

  for (unsigned int d = 0; d < ImageDimension; ++d)
  {
    if (sizeGreatestPrimeFactor > 1)
    {
      while (Math::GreatestPrimeFactor(size[d]) > sizeGreatestPrimeFactor)
      {
        ++size[d];
      }
    }
    else if (sizeGreatestPrimeFactor == 1)
    {
      // make sure the total size is even
      size[d] += size[d] % 2;
    }
  }

  return size;
}

template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::DeterminePadding()
{
  const SizeType fixedSize = m_FixedImage->GetLargestPossibleRegion().GetSize();
  const SizeType movingSize = m_MovingImage->GetLargestPossibleRegion().GetSize();
  const SizeType size0 = SizeType::Filled(0);
  SizeType       fftSize, fixedPad, movingPad;

  if (m_CropToOverlap)
  {
    typename MovingImageType::RegionType  fRegion = m_FixedImage->GetLargestPossibleRegion();
    typename MovingImageType::RegionType  mRegion = m_MovingImage->GetLargestPossibleRegion();
    typename MovingImageType::SpacingType spacing = m_MovingImage->GetSpacing();
    typename MovingImageType::IndexType   shiftIndex, fIndex;
    typename MovingImageType::IndexType   mIndex = mRegion.GetIndex();
    typename MovingImageType::PointType   originShift = m_MovingImage->GetOrigin() - m_FixedImage->GetOrigin();
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      shiftIndex[d] = std::round(originShift[d] / spacing[d]);
      mIndex[d] += shiftIndex[d];
    }
    mRegion.SetIndex(mIndex);
    fRegion.Crop(mRegion);

    // now expand this region somewhat
    SizeType iSize = fRegion.GetSize();
    fIndex = fRegion.GetIndex();
    SizeType                     extraPadding;
    std::array<SizeValueType, 3> padCandidates;
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      padCandidates[0] = 16;                                          // a fixed 16-pixel padding
      padCandidates[1] = std::ceil(iSize[d] / 2);                     // 50% of overlapping region
      padCandidates[2] = std::min(fixedSize[d], movingSize[d]) / 100; // 1% of smaller image's size
      std::sort(padCandidates.begin(), padCandidates.end());
      extraPadding[d] = padCandidates[1]; // pick median

      // clip it to actual image sizes
      if (extraPadding[d] + iSize[d] > fixedSize[d])
      {
        extraPadding[d] = fixedSize[d] - iSize[d];
      }
      if (extraPadding[d] + iSize[d] > movingSize[d])
      {
        extraPadding[d] = movingSize[d] - iSize[d];
      }

      // expand regions appropriately
      iSize[d] += extraPadding[d];
      if (shiftIndex[d] > 0) // fixed is to the "left" of moving
      {
        fIndex[d] -= extraPadding[d];
        mIndex[d] = 0;
      }
      else
      {
        mIndex[d] = movingSize[d] - iSize[d];
      }
    }

    // construct regions from indices and size
    fRegion.SetIndex(fIndex);
    fRegion.SetSize(iSize);
    mRegion.SetIndex(mIndex);
    mRegion.SetSize(iSize);
    m_FixedRoI->SetRegionOfInterest(fRegion);
    m_MovingRoI->SetRegionOfInterest(mRegion);

    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      fftSize[d] = iSize[d] + 2 * m_ObligatoryPadding[d];
    }
    fftSize = RoundUpToFFTSize(fftSize);
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      fixedPad[d] = (fftSize[d] - iSize[d]) - m_ObligatoryPadding[d];
      movingPad[d] = (fftSize[d] - iSize[d]) - m_ObligatoryPadding[d];
    }
  }
  else // do not crop to overlap
  {
    if (m_PadToSize == size0)
    {
      // set up padding to resize the images to the same size
      SizeType maxSize;

      for (unsigned int d = 0; d < ImageDimension; ++d)
      {
        if (fixedSize[d] >= movingSize[d])
        {
          maxSize[d] = fixedSize[d];
        }
        else
        {
          maxSize[d] = movingSize[d];
        }
        // we need to pad on both ends along this dimension
        maxSize[d] += 2 * m_ObligatoryPadding[d];
      }

      fftSize = RoundUpToFFTSize(maxSize);
    }
    else
    {
      fftSize = m_PadToSize;
    }

    SizeType fftHalf = fftSize;
    fftHalf[0] = fftSize[0] / 2 + 1;
    if (m_FixedImageFFT.IsNotNull())
    {
      SizeType fftCached = m_FixedImageFFT->GetLargestPossibleRegion().GetSize();
      itkAssertOrThrowMacro(fftCached == fftHalf,
                            "FixedImage's cached FFT (" << fftCached << ") must have the common padded size: "
                                                        << fftSize << " halved in first dimension: " << fftHalf);
    }
    if (m_MovingImageFFT.IsNotNull())
    {
      SizeType fftCached = m_MovingImageFFT->GetLargestPossibleRegion().GetSize();
      itkAssertOrThrowMacro(fftCached == fftHalf,
                            "MovingImage's cached FFT (" << fftCached << ") must have the common padded size: "
                                                         << fftSize << " halved in first dimension: " << fftHalf);
    }

    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      if (fixedSize[d] + 2 * m_ObligatoryPadding[d] > fftSize[d])
      {
        itkExceptionMacro("PadToSize(" << fftSize[d] << ") for dimension " << d
                                       << " must be larger than fixed image size (" << fixedSize[d] << ")"
                                       << " and twice the obligatory padding (" << m_ObligatoryPadding[d] << ")");
      }
      fixedPad[d] = (fftSize[d] - fixedSize[d]) - m_ObligatoryPadding[d];
      if (movingSize[d] + 2 * m_ObligatoryPadding[d] > fftSize[d])
      {
        itkExceptionMacro("PadToSize(" << fftSize[d] << ") for dimension " << d
                                       << " must be larger than moving image size (" << movingSize[d] << ")"
                                       << " and twice the obligatory padding (" << m_ObligatoryPadding[d] << ")");
      }
      movingPad[d] = (fftSize[d] - movingSize[d]) - m_ObligatoryPadding[d];
    }
  }

  m_FixedPadder->SetPadLowerBound(m_ObligatoryPadding);
  m_MovingPadder->SetPadLowerBound(m_ObligatoryPadding);
  m_FixedPadder->SetPadUpperBound(fixedPad);
  m_MovingPadder->SetPadUpperBound(movingPad);
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::StartOptimization()
{
  ParametersType empty(ImageDimension);
  empty.Fill(0.0);
  m_TransformParameters = empty;
  itkDebugMacro("starting optimization");
  using OffsetType = typename OptimizerType::OffsetType;
  OffsetType offset;
  try
  {
    if (this->GetDebug())
    {
      WriteDebug(m_FixedImage.GetPointer(), "m_FixedImage.nrrd");
      WriteDebug(m_MovingImage.GetPointer(), "m_MovingImage.nrrd");
      WriteDebug(m_FixedPadder->GetOutput(), "m_FixedPadder.nrrd");
      WriteDebug(m_MovingPadder->GetOutput(), "m_MovingPadder.nrrd");
      WriteDebug(m_FixedFFT->GetOutput(), "m_FixedFFT.nrrd");
      WriteDebug(m_MovingFFT->GetOutput(), "m_MovingFFT.nrrd");
      if (m_CropToOverlap)
      {
        WriteDebug(m_FixedRoI->GetOutput(), "m_FixedRoI.nrrd");
        WriteDebug(m_MovingRoI->GetOutput(), "m_MovingRoI.nrrd");
      }
    }

    m_FixedPadder->UpdateOutputInformation(); // to make sure xSize is valid
    unsigned xSize = m_FixedPadder->GetOutput()->GetLargestPossibleRegion().GetSize(0);
    m_IFFT->SetActualXDimensionIsOdd(xSize % 2 != 0);
    auto * phaseCorrelation = static_cast<RealImageType *>(this->ProcessObject::GetOutput(1));
    phaseCorrelation->Allocate();
    m_IFFT->GraftOutput(phaseCorrelation);
    m_IFFT->Update();

    const unsigned offsetCount = ImageDimension;
    m_Optimizer->SetOffsetCount(offsetCount); // update can reduce this, so we have to set it each time
    m_Optimizer->Update();
    offset = m_Optimizer->GetOffsets()[0];
    phaseCorrelation->Graft(m_IFFT->GetOutput());

    if (m_FixedImageFFT.IsNull())
    {
      m_FixedImageFFT = m_FixedFFT->GetOutput();
      m_FixedImageFFT->DisconnectPipeline();
    }
    if (m_MovingImageFFT.IsNull())
    {
      m_MovingImageFFT = m_MovingFFT->GetOutput();
      m_MovingImageFFT->DisconnectPipeline();
    }

    if (this->GetDebug())
    {
      WriteDebug(m_IFFT->GetOutput(), "m_IFFT.nrrd");
      WriteDebug(m_BandPassFilter->GetOutput(), "m_BandPassFilter.nrrd");
      WriteDebug(m_Operator->GetOutput(), "m_Operator.nrrd");

      // now do banpass of input images and inverse FFT
      m_IFFT->SetInput(m_BandPassFilter->GetOutput());
      m_BandPassFilter->SetInput(m_FixedFFT->GetOutput());
      typename RealImageType::Pointer invImage = m_IFFT->GetOutput();
      invImage->Update();
      invImage->DisconnectPipeline();
      invImage->CopyInformation(m_FixedPadder->GetOutput());
      WriteDebug(invImage.GetPointer(), "iFixed.nrrd");
      m_BandPassFilter->SetInput(m_MovingFFT->GetOutput());
      invImage = m_IFFT->GetOutput();
      invImage->Update();
      invImage->DisconnectPipeline();
      invImage->CopyInformation(m_MovingPadder->GetOutput());
      WriteDebug(invImage.GetPointer(), "iMoving.nrrd");
    }
  }
  catch (ExceptionObject & err)
  {
    // Pass exception to caller
    itkDebugMacro("exception caught during optimization - passing");
    throw err;
  }
  itkDebugMacro("optimization finished");

  m_TransformParameters = ParametersType(ImageDimension);
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    m_TransformParameters[i] = offset[i];
  }

  // set the output transform
  auto *           transformOutput = static_cast<TransformOutputType *>(this->ProcessObject::GetOutput(0));
  TransformPointer transform(const_cast<TransformType *>(transformOutput->Get()));
  transform->SetParameters(m_TransformParameters);

  itkDebugMacro("output set to " << transform);
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::PrintSelf(std::ostream & os,
                                                                                                  Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Operator: " << m_Operator.GetPointer() << std::endl;
  os << indent << "Optimizer: " << m_Optimizer.GetPointer() << std::endl;
  os << indent << "Fixed Padder: " << m_FixedPadder.GetPointer() << std::endl;
  os << indent << "Moving Padder: " << m_MovingPadder.GetPointer() << std::endl;

  os << indent << "Pad To Size: " << m_PadToSize << std::endl;
  os << indent << "Obligatory Padding: " << m_ObligatoryPadding << std::endl;
  switch (m_PaddingMethod)
  {
    case PaddingMethodEnum::Zero:
      os << indent << "Padding Method: "
         << "Zero" << std::endl;
      break;
    case PaddingMethodEnum::Mirror:
      os << indent << "Padding Method: "
         << "Mirror" << std::endl;
      break;
    case PaddingMethodEnum::MirrorWithExponentialDecay:
      os << indent << "Padding Method: "
         << "MirrorWithExponentialDecay" << std::endl;
      break;
    default:
      os << indent << "Padding Method: "
         << "Unknown" << std::endl;
      break;
  }

  os << indent << "Crop To Overlap: " << m_CropToOverlap << std::endl;
  os << indent << "Butterworth Order: " << m_ButterworthOrder << std::endl;
  os << indent << "Low Frequency: " << this->GetButterworthLowFrequency() << std::endl;
  os << indent << "High Frequency: " << this->GetButterworthHighFrequency() << std::endl;

  os << indent << "Fixed Image: " << m_FixedImage.GetPointer() << std::endl;
  os << indent << "Moving Image: " << m_MovingImage.GetPointer() << std::endl;
  os << indent << "Fixed Image FFT: " << m_FixedImageFFT.GetPointer() << std::endl;
  os << indent << "Moving Image FFT: " << m_MovingImageFFT.GetPointer() << std::endl;
  os << indent << "Transform Parameters: " << m_TransformParameters << std::endl;

  typename TransformType::ConstPointer t(this->GetOutput()->Get());
  os << indent << "Output transform: " << t.GetPointer() << std::endl;
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  this->Initialize();
  this->DeterminePadding();

  if (m_FixedImage->GetSpacing() != m_MovingImage->GetSpacing())
  {
    itkExceptionMacro("Fixed image and moving image must have the same spacing!\nFixed spacing: "
                      << m_FixedImage->GetSpacing() << "\nMoving spacing: " << m_MovingImage->GetSpacing());
  }
  if (m_FixedImage->GetDirection() != m_MovingImage->GetDirection())
  {
    itkExceptionMacro("Fixed image and moving image must have the same direction!\nFixed direction:\n"
                      << m_FixedImage->GetDirection() << "\nMoving direction:\n"
                      << m_MovingImage->GetDirection());
  }

  m_IFFT->UpdateOutputInformation();

  auto * phaseCorrelation = static_cast<RealImageType *>(this->ProcessObject::GetOutput(1));
  phaseCorrelation->CopyInformation(m_IFFT->GetOutput());
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::GenerateData()
{
  this->Initialize();
  this->StartOptimization();
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
const typename PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::
  TransformOutputType *
  PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::GetOutput() const
{
  return static_cast<const TransformOutputType *>(this->ProcessObject::GetOutput(0));
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
const typename PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::RealImageType *
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::GetPhaseCorrelationImage() const
{
  return static_cast<const RealImageType *>(this->ProcessObject::GetOutput(1));
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
DataObject::Pointer
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::MakeOutput(
  DataObjectPointerArraySizeType output)
{
  switch (output)
  {
    case 0:
      return static_cast<DataObject *>(TransformOutputType::New().GetPointer());
      break;
    case 1:
      return static_cast<DataObject *>(RealImageType::New().GetPointer());
      break;
    default:
      itkExceptionMacro("MakeOutput request for an output number larger than the expected number of outputs");
  }
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::SetFixedImage(
  const FixedImageType * fixedImage)
{
  itkDebugMacro("setting Fixed Image to " << fixedImage);
  if (this->m_FixedImage.GetPointer() != fixedImage)
  {
    this->m_FixedImage = fixedImage;
    this->m_FixedImageFFT = nullptr; // clear cached FFT
    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput(0, const_cast<FixedImageType *>(fixedImage));
    this->Modified();
  }
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::SetMovingImage(
  const MovingImageType * movingImage)
{
  itkDebugMacro("setting Moving Image to " << movingImage);
  if (this->m_MovingImage.GetPointer() != movingImage)
  {
    this->m_MovingImage = movingImage;
    this->m_MovingImageFFT = nullptr; // clear cached FFT
    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput(1, const_cast<MovingImageType *>(movingImage));
    this->Modified();
  }
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::SetFixedImageFFT(
  const ComplexImageType * fixedImageFFT)
{
  itkDebugMacro("setting fixedImageFFT Image to " << fixedImageFFT);
  if (this->m_FixedImageFFT.GetPointer() != fixedImageFFT)
  {
    this->m_FixedImageFFT = const_cast<ComplexImageType *>(fixedImageFFT);
    this->Modified();
  }
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::SetMovingImageFFT(
  const ComplexImageType * movingImageFFT)
{
  itkDebugMacro("setting movingImageFFT Image to " << movingImageFFT);
  if (this->m_MovingImageFFT.GetPointer() != movingImageFFT)
  {
    this->m_MovingImageFFT = const_cast<ComplexImageType *>(movingImageFFT);
    this->Modified();
  }
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::SetReleaseDataFlag(bool a_flag)
{
  Superclass::SetReleaseDataFlag(a_flag);
  m_FixedRoI->SetReleaseDataFlag(a_flag);
  m_MovingRoI->SetReleaseDataFlag(a_flag);
  m_FixedConstantPadder->SetReleaseDataFlag(a_flag);
  m_MovingConstantPadder->SetReleaseDataFlag(a_flag);
  m_FixedMirrorPadder->SetReleaseDataFlag(a_flag);
  m_MovingMirrorPadder->SetReleaseDataFlag(a_flag);
  m_FixedMirrorWEDPadder->SetReleaseDataFlag(a_flag);
  m_MovingMirrorWEDPadder->SetReleaseDataFlag(a_flag);
  m_FixedFFT->SetReleaseDataFlag(a_flag);
  m_MovingFFT->SetReleaseDataFlag(a_flag);
  m_IFFT->SetReleaseDataFlag(a_flag);
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::SetReleaseDataBeforeUpdateFlag(
  bool a_flag)
{
  Superclass::SetReleaseDataBeforeUpdateFlag(a_flag);
  m_FixedRoI->SetReleaseDataBeforeUpdateFlag(a_flag);
  m_MovingRoI->SetReleaseDataBeforeUpdateFlag(a_flag);
  m_FixedConstantPadder->SetReleaseDataBeforeUpdateFlag(a_flag);
  m_MovingConstantPadder->SetReleaseDataBeforeUpdateFlag(a_flag);
  m_FixedMirrorPadder->SetReleaseDataBeforeUpdateFlag(a_flag);
  m_MovingMirrorPadder->SetReleaseDataBeforeUpdateFlag(a_flag);
  m_FixedMirrorWEDPadder->SetReleaseDataBeforeUpdateFlag(a_flag);
  m_MovingMirrorWEDPadder->SetReleaseDataBeforeUpdateFlag(a_flag);
  m_FixedFFT->SetReleaseDataBeforeUpdateFlag(a_flag);
  m_MovingFFT->SetReleaseDataBeforeUpdateFlag(a_flag);
  m_IFFT->SetReleaseDataBeforeUpdateFlag(a_flag);
}


template <typename TFixedImage, typename TMovingImage, typename TInternalPixelType>
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage, TInternalPixelType>::SetOptimizer(
  OptimizerType * optimizer)
{
  itkDebugMacro("setting Optimizer to " << optimizer);
  if (this->m_Optimizer != optimizer)
  {
    this->m_Optimizer = optimizer;
    this->Modified();
  }
}

} // end namespace itk

#endif
