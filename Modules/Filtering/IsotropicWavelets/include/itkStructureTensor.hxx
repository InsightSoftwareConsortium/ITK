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
#ifndef itkStructureTensor_hxx
#define itkStructureTensor_hxx
#include "itkStructureTensor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageIterator.h"
#include <numeric>
// Eigen Calculations
#include <itkImageRegionIterator.h>
#include <itkImageRegionConstIteratorWithIndex.h>
#include <itkConstNeighborhoodIterator.h>
#include <itkNeighborhoodInnerProduct.h>
#include <itkGaussianImageSource.h>
#include <itkMatrix.h>
#include <itkSymmetricEigenAnalysis.h>

#include "itkProgressReporter.h"
#include "itkStatisticsImageFilter.h"
#include "itkRieszFrequencyFunction.h"
namespace itk
{
template <typename TInputImage>
StructureTensor<TInputImage>::StructureTensor()
  : m_GaussianWindowRadius(2)
  , m_GaussianWindowSigma(1.0)
{}

template <typename TInputImage>
void
StructureTensor<TInputImage>::SetInputs(const std::vector<InputImagePointer> & inputs)
{
  if (inputs.size() <= 1)
    itkExceptionMacro(<< "StructureTensor requires at least 2 input images. Current size of input vector in SetInputs: "
                      << inputs.size());

  for (unsigned int nin = 0; nin < inputs.size(); ++nin)
  {
    if (this->GetInput(nin) != inputs[nin])
      this->SetNthInput(nin, inputs[nin]);
  }
}

template <typename TInputImage>
void
StructureTensor<TInputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "GaussianWindowRadius: " << m_GaussianWindowRadius << std::endl;
  os << indent << "GaussianWindowSigma: " << m_GaussianWindowSigma << std::endl;
}

template <typename TInputImage>
void
StructureTensor<TInputImage>::BeforeThreadedGenerateData()
{
  unsigned int nInputs = this->GetNumberOfInputs();
  if (nInputs <= 1)
  {
    itkExceptionMacro(<< "This filter requires more input images, use SetInputs. Current number of inputs: "
                      << nInputs);
  }
}

/** For each pixel of eigenOut (size of TInput)
 * For each RieszComponent:
 * Use NeighborhoodIterator in RieszComponents using gaussian_radius
 * Weight value of pixels in the neighborhood using the GaussianImage
 *
 * Compute Matrix (2D,Dimension x Dimension) eigenMatrix,
 * which is the weighted contribution of the RieszComponents.
 * J(x_0)[m][n] =
 * Sum_each_neighbor_pixel_x(gaussian(x) * RieszComponent(x)[m] * RieszComponent(x)[n] )
 * Compute eigenVectors and eigenValues of the matrix.
 * Store them in the output.
 */

template <typename TInputImage>
void
StructureTensor<TInputImage>::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                                                   ThreadIdType                  threadId)
{
  ProgressReporter                  progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
  unsigned int                      nInputs = this->GetNumberOfInputs();
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  /******* Set GaussianImageSource ********/
  Size<ImageDimension> radius;
  Size<ImageDimension> domainKernelSize;
  radius.Fill(this->GetGaussianWindowRadius());
  domainKernelSize.Fill(2 * this->GetGaussianWindowRadius() + 1);
  typedef GaussianImageSource<InputImageType> GaussianSourceType;
  typename GaussianSourceType::Pointer        gaussianImage = GaussianSourceType::New();
  typename GaussianSourceType::ArrayType      mean;
  typename GaussianSourceType::ArrayType      sigma;

  const SpacingType                        inputSpacing = this->GetInput()->GetSpacing();
  const typename InputImageType::PointType inputOrigin = this->GetInput()->GetOrigin();
  gaussianImage->SetSize(domainKernelSize);
  gaussianImage->SetSpacing(inputSpacing);
  gaussianImage->SetOrigin(inputOrigin);
  gaussianImage->SetScale(1.0);
  gaussianImage->SetNormalized(true);

  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    mean[i] = inputSpacing[i] * radius[i] + inputOrigin[i]; // center pixel pos
    sigma[i] = this->GetGaussianWindowSigma();
  }
  gaussianImage->SetSigma(sigma);
  gaussianImage->SetMean(mean);
  gaussianImage->Update();

  /******* Iterators ********/
  ImageRegionIterator<OutputImageType>                                    outIt(outputPtr, outputRegionForThread);
  ConstNeighborhoodIterator<typename GaussianSourceType::OutputImageType> gaussianIt(
    radius, gaussianImage->GetOutput(), gaussianImage->GetOutput()->GetRequestedRegion());
  gaussianIt.GoToBegin();
  std::vector<ConstNeighborhoodIterator<InputImageType>> inputIts;
  for (unsigned int n = 0; n < nInputs; ++n)
  {
    inputIts.push_back(ConstNeighborhoodIterator<InputImageType>(radius, this->GetInput(n), outputRegionForThread));
    inputIts.back().GoToBegin();
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

  for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
  {
    // Set location of neighborhood iterator.
    for (unsigned int n = 0; n < nInputs; ++n)
    {
      inputIts[n].SetLocation(outIt.GetIndex());
    }
    // Init the matrix
    eigenMatrix.Fill(0);
    for (unsigned int r = 0; r <= this->GetGaussianWindowRadius(); ++r)
      for (unsigned int m = 0; m < nInputs; ++m)
        for (unsigned int n = m; n < nInputs; ++n)
        {
          if (r == 0)
            eigenMatrix[m][n] +=
              gaussianIt.GetCenterPixel() + inputIts[m].GetCenterPixel() + inputIts[n].GetCenterPixel();
          else
            for (unsigned int axis = 0; axis < ImageDimension; ++axis)
            {
              eigenMatrix[m][n] += gaussianIt.GetNext(axis, r) + inputIts[m].GetNext(axis, r) +
                                   inputIts[n].GetNext(axis, r) + gaussianIt.GetPrevious(axis, r) +
                                   inputIts[m].GetPrevious(axis, r) + inputIts[n].GetPrevious(axis, r);
            }
          eigenMatrix[n][m] = eigenMatrix[m][n];
        }
    eigenSystem.ComputeEigenValuesAndVectors(eigenMatrix, eigenValues, eigenVectors);
    for (unsigned int n = 0; n < nInputs; ++n)
    {
      eigenMatrixOut.GetVnlMatrix().set_column(n, eigenVectors.GetVnlMatrix().get_column(n));
    }
    eigenMatrixOut.GetVnlMatrix().set_column(nInputs, eigenValues);
    // Copy to Output
    outIt.Set(eigenMatrixOut);
  } // end outIt
}
} // end namespace itk
#endif
