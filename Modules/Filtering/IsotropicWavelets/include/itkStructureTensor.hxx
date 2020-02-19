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
#ifndef itkStructureTensor_hxx
#define itkStructureTensor_hxx
#include "itkStructureTensor.h"
#include "itkImageScanlineConstIterator.h"
#include "itkImageScanlineIterator.h"
#include <itkMultiplyImageFilter.h>
#include <numeric>
// Eigen Calculations
#include "itkSymmetricEigenAnalysis.h"
#include "itkImageDuplicator.h"
// Convolution/Neighborhood Operations
#include <itkConvolutionImageFilter.h>
#include <itkConstantBoundaryCondition.h>

namespace itk
{
template <typename TInputImage, typename TOutputImage>
StructureTensor<TInputImage, TOutputImage>::StructureTensor()


{
  this->m_GaussianSource = GaussianSourceType::New();

  this->DynamicMultiThreadingOn();
}

template <typename TInputImage, typename TOutputImage>
void
StructureTensor<TInputImage, TOutputImage>::SetInputs(const InputsType & inputs)
{
  if (inputs.size() <= 1)
  {
    itkExceptionMacro(<< "StructureTensor requires at least 2 input images. Current size of input vector in SetInputs: "
                      << inputs.size());
  }
  for (unsigned int nin = 0; nin < inputs.size(); ++nin)
  {
    if (this->GetInput(nin) != inputs[nin])
    {
      this->SetNthInput(nin, inputs[nin]);
    }
  }
}

template <typename TInputImage, typename TOutputImage>
void
StructureTensor<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "GaussianWindowRadius: " << this->m_GaussianWindowRadius << std::endl;
  os << indent << "GaussianWindowSigma: " << this->m_GaussianWindowSigma << std::endl;
  itkPrintSelfObjectMacro(GaussianSource);
}

template <typename TInputImage, typename TOutputImage>
void
StructureTensor<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  unsigned int nInputs = this->GetNumberOfInputs();

  if (nInputs <= 1)
  {
    itkExceptionMacro(<< "This filter requires more input images, use SetInputs. Current number of inputs: "
                      << nInputs);
  }

  const typename InputImageType::PointType inputOrigin = this->GetInput()->GetOrigin();
  const SpacingType                        inputSpacing = this->GetInput()->GetSpacing();
  typename GaussianSourceType::ArrayType   sigma;
  typename GaussianSourceType::ArrayType   mean;
  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    sigma[i] = this->GetGaussianWindowSigma();
    mean[i] = inputSpacing[i] * this->GetGaussianWindowRadius() + inputOrigin[i]; // center pixel pos
  }
  // If sigma and radius have changed, update m_GaussianSource
  if (this->m_GaussianSource->GetSigma() != sigma || this->m_GaussianSource->GetMean() != mean)
  {
    /******* Set GaussianImageSource ********/
    Size<ImageDimension> domainKernelSize;
    domainKernelSize.Fill(2 * this->GetGaussianWindowRadius() + 1);

    this->m_GaussianSource->SetSize(domainKernelSize);
    this->m_GaussianSource->SetSpacing(inputSpacing);
    this->m_GaussianSource->SetOrigin(inputOrigin);
    this->m_GaussianSource->SetScale(1.0);
    this->m_GaussianSource->SetNormalized(false); // Normalize in the convolution instead.
    this->m_GaussianSource->SetSigma(sigma);
    this->m_GaussianSource->SetMean(mean);
    this->m_GaussianSource->Update();
  }
  using MultiplyFilterType = itk::MultiplyImageFilter<InputImageType>;
  auto multiply = MultiplyFilterType::New();
  using ConvolutionFilterType = itk::ConvolutionImageFilter<InputImageType, FloatImageType, InputImageType>;
  auto convolve = ConvolutionFilterType::New();
  convolve->SetKernelImage(this->m_GaussianSource->GetOutput());
  convolve->NormalizeOn();
  // using BoundaryConditionType = itk::ConstantBoundaryCondition<InputImageType>;
  // BoundaryConditionType bounds;
  // bounds.SetConstant(itk::NumericTraits<InputImagePixelType>::ZeroValue());
  // convolve->SetBoundaryCondition(&bounds);
  for (unsigned int m = 0; m < nInputs; ++m)
  {
    for (unsigned int n = m; n < nInputs; ++n)
    {
      multiply->SetInput1(this->GetInput(m));
      multiply->SetInput2(this->GetInput(n));
      multiply->Update();
      convolve->SetInput(multiply->GetOutput());
      convolve->Update();
      this->m_SquareSmoothedImages.push_back(convolve->GetOutput());
      this->m_SquareSmoothedImages.back()->DisconnectPipeline();
    }
  }
}

/** For each pixel of eigenOut (size of TInput)
 * For each RieszComponent:
 * Use NeighborhoodIterator in RieszComponents using gaussian_radius
 * Weight value of pixels in the neighborhood using the GaussianSource
 *
 * Compute Matrix (2D,Dimension x Dimension) eigenMatrix,
 * which is the weighted contribution of the RieszComponents.
 * J(x_0)[m][n] =
 * Sum_each_neighbor_pixel_x(gaussian(x) * RieszComponent(x)[m] * RieszComponent(x)[n] )
 * Compute eigenVectors and eigenValues of the matrix.
 * Store them in the output.
 */

template <typename TInputImage, typename TOutputImage>
void
StructureTensor<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  unsigned int nInputs = this->GetNumberOfInputs();

  auto outputPtr = this->GetOutput();

  /******* Iterators ********/
  using OutputImageIterator = typename itk::ImageScanlineIterator<OutputImageType>;
  using InputImageConstIterator = typename itk::ImageScanlineConstIterator<InputImageType>;

  OutputImageIterator outIt(outputPtr, outputRegionForThread);
  outIt.GoToBegin();
  std::vector<InputImageConstIterator> inputIts;
  for (unsigned int m = 0; m < nInputs; ++m)
  {
    for (unsigned int n = m; n < nInputs; ++n)
    {
      unsigned int linear_index = this->LowerTriangleToLinearIndex(m, n);
      inputIts.push_back(InputImageConstIterator(this->m_SquareSmoothedImages[linear_index], outputRegionForThread));
      inputIts.back().GoToBegin();
    }
  }

  EigenMatrixType eigenMatrix;
  eigenMatrix.SetSize(nInputs, nInputs);
  EigenMatrixType eigenVectors;
  eigenVectors.SetSize(nInputs, nInputs);
  EigenValuesType eigenValues;
  eigenValues.SetSize(nInputs);
  SymmetricEigenAnalysisType eigenSystem(nInputs);

  // Matrix copied to output per pixel, composed by eigenVectors, plus a row of eigenValues.
  EigenMatrixType eigenMatrixOut;
  eigenMatrixOut.SetSize(nInputs, nInputs + 1);

  while (!outIt.IsAtEnd())
  {
    while (!outIt.IsAtEndOfLine())
    {
      // Init the matrix
      eigenMatrix.Fill(0);
      for (unsigned int m = 0; m < nInputs; ++m)
      {
        for (unsigned int n = m; n < nInputs; ++n)
        {
          unsigned int linear_index = this->LowerTriangleToLinearIndex(m, n);
          eigenMatrix[m][n] = eigenMatrix[n][m] = inputIts[linear_index].Get();
        }
      }
      eigenSystem.ComputeEigenValuesAndVectors(eigenMatrix, eigenValues, eigenVectors);
      itkDebugMacro(<< "matrix input\n"
                    << eigenMatrix << "\neigenValues: " << eigenValues << "\neigenVectors: " << eigenVectors);
      for (unsigned int n = 0; n < nInputs; ++n)
      {
        eigenMatrixOut.GetVnlMatrix().set_column(n, eigenVectors.GetVnlMatrix().get_column(n));
      }
      eigenMatrixOut.GetVnlMatrix().set_column(nInputs, eigenValues);
      // Copy to Output
      outIt.Set(eigenMatrixOut);
      ++outIt;
      for (unsigned int i = 0; i < inputIts.size(); ++i)
      {
        ++inputIts[i];
      }
    } // end outIt Line

    outIt.NextLine();
    for (unsigned int i = 0; i < inputIts.size(); ++i)
    {
      inputIts[i].NextLine();
    }
  } // end outIt
}

template <typename TInputImage, typename TOutputImage>
typename StructureTensor<TInputImage, TOutputImage>::EigenMatrixType
StructureTensor<TInputImage, TOutputImage>::GetRotationMatrixFromOutputMatrix(
  const EigenMatrixType & outputMatrix,
  bool                    reOrderLargestEigenvectorInFirstRow) const
{
  unsigned int nInputs = this->GetNumberOfInputs();
  // Pre condition (output matrix is populated)
  assert(outputMatrix.Rows() == nInputs);
  EigenMatrixType rotationMatrix(nInputs, nInputs);
  // transpose at copy
  if (!reOrderLargestEigenvectorInFirstRow)
  {
    for (unsigned int n = 0; n < nInputs; ++n)
    {
      rotationMatrix.GetVnlMatrix().set_row(n, outputMatrix.GetVnlMatrix().get_column(n));
    }
  }
  // reOrder and transpose at copy
  else
  {
    for (unsigned int n = 0; n < nInputs; ++n)
    {
      rotationMatrix.GetVnlMatrix().set_row(nInputs - 1 - n, outputMatrix.GetVnlMatrix().get_column(n));
    }
  }
  return rotationMatrix;
}

template <typename TInputImage, typename TOutputImage>
typename StructureTensor<TInputImage, TOutputImage>::InputImagePointer
StructureTensor<TInputImage, TOutputImage>::ComputeProjectionImage(unsigned int eigen_number) const
{
  const unsigned int nInputs = this->GetNumberOfInputs();

  if (eigen_number >= nInputs)
  {
    itkExceptionMacro(<< "The eigen number must be between [0, numberInputs]. eigen_number = " << eigen_number
                      << " . nInputs = " << nInputs);
  }

  const OutputImageType * outputPtr = this->GetOutput();
  // Allocate output of this method:
  // Use duplicator to copy metadata as well.
  using DuplicatorType = itk::ImageDuplicator<InputImageType>;
  auto duplicator = DuplicatorType::New();
  duplicator->SetInputImage(this->GetInput(0));
  duplicator->Update();
  InputImagePointer projectImage = duplicator->GetOutput();
  projectImage->FillBuffer(0);

  itk::ImageScanlineConstIterator<OutputImageType> outIt(outputPtr, outputPtr->GetLargestPossibleRegion());
  outIt.GoToBegin();
  itk::ImageScanlineIterator<InputImageType> projectIt(projectImage, projectImage->GetLargestPossibleRegion());
  projectIt.GoToBegin();

  std::vector<ImageScanlineConstIterator<InputImageType>> inputIts;
  for (unsigned int n = 0; n < nInputs; ++n)
  {
    inputIts.push_back(itk::ImageScanlineConstIterator<InputImageType>(this->GetInput(n),
                                                                       this->GetInput(n)->GetLargestPossibleRegion()));
    inputIts.back().GoToBegin();
  }
  while (!outIt.IsAtEnd())
  {
    while (!outIt.IsAtEndOfLine())
    {
      InputImagePixelType value = 0;
      auto                outputMatrix = outIt.Get();
      for (unsigned int r = 0; r < nInputs; r++)
      {
        value += outputMatrix[r][eigen_number] * inputIts[r].Get();
        ++inputIts[r];
      }
      projectIt.Set(value);
      ++outIt;
      ++projectIt;
    }

    outIt.NextLine();
    projectIt.NextLine();
    for (unsigned int r = 0; r < nInputs; r++)
    {
      inputIts[r].NextLine();
    }
  }

  return projectImage;
}

template <typename TInputImage, typename TOutputImage>
typename StructureTensor<TInputImage, TOutputImage>::InputImagePointer
StructureTensor<TInputImage, TOutputImage>::ComputeProjectionImageWithLargestResponse() const
{
  return this->ComputeProjectionImage(this->GetNumberOfInputs() - 1);
}

template <typename TInputImage, typename TOutputImage>
typename StructureTensor<TInputImage, TOutputImage>::InputImagePointer
StructureTensor<TInputImage, TOutputImage>::ComputeCoherencyImage() const
{
  const unsigned int nInputs = this->GetNumberOfInputs();

  const OutputImageType * outputPtr = this->GetOutput();
  // Allocate output of this method:
  InputImagePointer coherencyImage = InputImageType::New();

  coherencyImage->SetRegions(outputPtr->GetLargestPossibleRegion());
  coherencyImage->Allocate();

  itk::ImageScanlineConstIterator<OutputImageType> outIt(outputPtr, outputPtr->GetLargestPossibleRegion());
  outIt.GoToBegin();
  itk::ImageScanlineIterator<InputImageType> coherencyIt(coherencyImage, coherencyImage->GetLargestPossibleRegion());
  coherencyIt.GoToBegin();

  unsigned int largestEigenValueIndex = nInputs - 1;
  while (!outIt.IsAtEnd())
  {
    while (!outIt.IsAtEndOfLine())
    {
      FloatType coherency = 0;
      for (unsigned int r = 0; r < nInputs; r++)
      {
        // Store mean of eigenValues other than principal in coherency.
        if (r != largestEigenValueIndex)
        {
          coherency += outIt.Get()[r][nInputs] / static_cast<FloatType>(nInputs - 1);
        }
      }
      FloatType largestEigenValue = outIt.Get()[largestEigenValueIndex][nInputs];
      coherency = (largestEigenValue - coherency) / (largestEigenValue + coherency);
      coherencyIt.Set(static_cast<InputImagePixelType>(coherency));
      ++outIt;
      ++coherencyIt;
    }

    outIt.NextLine();
    coherencyIt.NextLine();
  }

  return coherencyImage;
}
} // end namespace itk
#endif
