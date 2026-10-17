/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkExtractImageFilter_hxx
#define itkExtractImageFilter_hxx

#include "itkImageAlgorithm.h"
#include "itkBridgeMathDeterminant.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
ExtractImageFilter<TInputImage, TOutputImage>::ExtractImageFilter()
{
  Superclass::InPlaceOff();
  this->DynamicMultiThreadingOn();
}

template <typename TInputImage, typename TOutputImage>
void
ExtractImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ExtractionRegion: " << m_ExtractionRegion << std::endl;
  os << indent << "OutputImageRegion: " << m_OutputImageRegion << std::endl;
  os << indent << "DirectionCollapseStrategy: " << m_DirectionCollapseStrategy << std::endl;
}

template <typename TInputImage, typename TOutputImage>
void
ExtractImageFilter<TInputImage, TOutputImage>::CallCopyOutputRegionToInputRegion(
  InputImageRegionType &        destRegion,
  const OutputImageRegionType & srcRegion)
{
  const ExtractImageFilterRegionCopierType extractImageRegionCopier;

  extractImageRegionCopier(destRegion, srcRegion, m_ExtractionRegion);
}

template <typename TInputImage, typename TOutputImage>
void
ExtractImageFilter<TInputImage, TOutputImage>::SetExtractionRegion(InputImageRegionType extractRegion)
{
  static_assert(InputImageDimension >= OutputImageDimension,
                "InputImageDimension must be greater than OutputImageDimension");
  m_ExtractionRegion = extractRegion;

  InputImageSizeType   inputSize = extractRegion.GetSize();
  OutputImageSizeType  outputSize{};
  OutputImageIndexType outputIndex{};

  /**
   * check to see if the number of non-zero entries in the extraction region
   * matches the number of dimensions in the output image.
   */
  unsigned int nonzeroSizeCount = 0;
  for (unsigned int i = 0; i < InputImageDimension; ++i)
  {
    if (inputSize[i])
    {
      if (nonzeroSizeCount < OutputImageDimension)
      {
        outputSize[nonzeroSizeCount] = inputSize[i];
        outputIndex[nonzeroSizeCount] = extractRegion.GetIndex()[i];
      }
      ++nonzeroSizeCount;
    }
  }

  if (nonzeroSizeCount != OutputImageDimension)
  {
    itkExceptionMacro("The number of zero sized dimensions in the input image Extraction Region\n"
                      << "is not consistent with the dimensionality of the output image.\n"
                      << "Expected the extraction region size (" << extractRegion.GetSize() << ") to contain "
                      << InputImageDimension - OutputImageDimension << " zero sized dimensions to collapse.");
  }

  m_OutputImageRegion = { outputIndex, outputSize };
  this->Modified();
}

template <typename TInputImage, typename TOutputImage>
template <unsigned int VOutputDimension, unsigned int VInputDimension>
Matrix<double, VOutputDimension, VOutputDimension>
ExtractImageFilter<TInputImage, TOutputImage>::CollapseDirection(
  const Matrix<double, VInputDimension, VInputDimension> & direction,
  const FixedArray<bool, VInputDimension> &                collapseDimensions,
  const DirectionCollapseStrategyEnum                      strategy)
{
  using SubMatrixType = Matrix<double, VOutputDimension, VOutputDimension>;

  SubMatrixType subMatrix{};
  unsigned int  row = 0;
  for (unsigned int i = 0; i < VInputDimension; ++i)
  {
    if (collapseDimensions[i])
    {
      continue;
    }
    unsigned int col = 0;
    for (unsigned int j = 0; j < VInputDimension; ++j)
    {
      if (collapseDimensions[j])
      {
        continue;
      }
      if (row < VOutputDimension && col < VOutputDimension)
      {
        subMatrix[row][col] = direction[i][j];
      }
      ++col;
    }
    ++row;
  }

  if (row != VOutputDimension)
  {
    itkGenericExceptionMacro("The number of retained dimensions (" << row << ") does not match the output dimension ("
                                                                   << VOutputDimension << ").");
  }

  switch (strategy)
  {
    case DirectionCollapseStrategyEnum::DIRECTIONCOLLAPSETOIDENTITY:
      return SubMatrixType::GetIdentity();
    case DirectionCollapseStrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX:
      if (bridge::Math::Determinant(subMatrix.GetVnlMatrix()) == 0.0)
      {
        itkGenericExceptionMacro("Invalid submatrix extracted for collapsed direction.");
      }
      return subMatrix;
    case DirectionCollapseStrategyEnum::DIRECTIONCOLLAPSETOGUESS:
      if (bridge::Math::Determinant(subMatrix.GetVnlMatrix()) == 0.0)
      {
        return SubMatrixType::GetIdentity();
      }
      return subMatrix;
    case DirectionCollapseStrategyEnum::DIRECTIONCOLLAPSETOUNKOWN:
    default:
      itkGenericExceptionMacro(
        "It is required that the strategy for collapsing the direction matrix be explicitly specified. "
        "Set with either myfilter->SetDirectionCollapseToIdentity() or "
        "myfilter->SetDirectionCollapseToSubmatrix().");
  }
}

template <typename TInputImage, typename TOutputImage>
void
ExtractImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{
  // do not call the superclass' implementation of this method since
  // this filter allows the input and the output to be of different dimensions

  // get pointers to the input and output
  const typename Superclass::OutputImagePointer     outputPtr = this->GetOutput();
  const typename Superclass::InputImageConstPointer inputPtr = this->GetInput();

  if (!outputPtr || !inputPtr)
  {
    return;
  }

  // Set the output image size to the same value as the extraction region.
  outputPtr->SetLargestPossibleRegion(m_OutputImageRegion);

  // Set the output spacing and origin

  // Copy what we can from the image from spacing and origin of the input
  // This logic needs to be augmented with logic that select which
  // dimensions to copy

  const typename InputImageType::SpacingType &   inputSpacing = inputPtr->GetSpacing();
  const typename InputImageType::DirectionType & inputDirection = inputPtr->GetDirection();
  const typename InputImageType::PointType &     inputOrigin = inputPtr->GetOrigin();

  typename OutputImageType::SpacingType   outputSpacing;
  typename OutputImageType::DirectionType outputDirection;
  typename OutputImageType::PointType     outputOrigin{};

  if constexpr (OutputImageDimension > InputImageDimension)
  {
    // copy the input to the output and fill the rest of the
    // output with zeros.
    for (unsigned int i = 0; i < InputImageDimension; ++i)
    {
      outputSpacing[i] = inputSpacing[i];
      outputOrigin[i] = inputOrigin[i];
      for (unsigned int dim = 0; dim < InputImageDimension; ++dim)
      {
        outputDirection[i][dim] = inputDirection[i][dim];
      }
    }
    for (unsigned int i = InputImageDimension; i < OutputImageDimension; ++i)
    {
      outputSpacing[i] = 1.0;
      outputOrigin[i] = 0.0;
      for (unsigned int dim = 0; dim < InputImageDimension; ++dim)
      {
        outputDirection[i][dim] = 0.0;
      }
      outputDirection[i][i] = 1.0;
    }
  }
  else
  {
    // copy the non-collapsed part of the input spacing and origin to the
    // output
    int nonZeroCount = 0;
    for (unsigned int i = 0; i < InputImageDimension; ++i)
    {
      if (m_ExtractionRegion.GetSize()[i])
      {
        outputSpacing[nonZeroCount] = inputSpacing[i];
        outputOrigin[nonZeroCount] = inputOrigin[i];
        ++nonZeroCount;
      }
    }

    if constexpr (InputImageDimension == OutputImageDimension)
    {
      outputDirection = inputDirection;
    }
    else
    {
      FixedArray<bool, InputImageDimension> collapseDimensions;
      for (unsigned int i = 0; i < InputImageDimension; ++i)
      {
        collapseDimensions[i] = (m_ExtractionRegion.GetSize()[i] == 0);
      }
      outputDirection = Self::template CollapseDirection<OutputImageDimension>(
        inputDirection, collapseDimensions, m_DirectionCollapseStrategy);
    }
  }
  // set the spacing and origin
  outputPtr->SetSpacing(outputSpacing);
  outputPtr->SetDirection(outputDirection);
  outputPtr->SetOrigin(outputOrigin);
  outputPtr->SetNumberOfComponentsPerPixel(inputPtr->GetNumberOfComponentsPerPixel());
}

template <typename TInputImage, typename TOutputImage>
void
ExtractImageFilter<TInputImage, TOutputImage>::GenerateData()
{

  // InPlace::AllocateOutputs set the running in place ivar.
  // This method will be called again, by GenerateData, but there is
  // no harm done.
  this->AllocateOutputs();

  // The input matched the output, nothing to do.
  if (this->GetRunningInPlace())
  {
    OutputImageType * outputPtr = this->GetOutput();

    // the in-place grafting copies the meta data, this needs to be
    // set back.
    outputPtr->SetLargestPossibleRegion(m_OutputImageRegion);

    this->UpdateProgress(1.0);
    return;
  }

  this->Superclass::GenerateData();
}

template <typename TInputImage, typename TOutputImage>
void
ExtractImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  itkDebugMacro("Actually executing");

  const InputImageType * inputPtr = this->GetInput();
  OutputImageType *      outputPtr = this->GetOutput();


  // Define the portion of the input to walk for this thread
  InputImageRegionType inputRegionForThread;
  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  // copy the input pixel to the output
  ImageAlgorithm::Copy(inputPtr, outputPtr, inputRegionForThread, outputRegionForThread);
}

} // end namespace itk

#endif
