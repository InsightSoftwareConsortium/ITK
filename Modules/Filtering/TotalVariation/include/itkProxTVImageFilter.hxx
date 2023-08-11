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
#ifndef itkProxTVImageFilter_hxx
#define itkProxTVImageFilter_hxx


#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkCastImageFilter.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
void
ProxTVImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "MaximumNumberOfIterations: " << m_MaximumNumberOfIterations << std::endl;
  os << indent << "Weights: " << m_Weights << std::endl;
  os << indent << "Norms: " << m_Norms << std::endl;
}


template <typename TInputImage, typename TOutputImage>
void
ProxTVImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  this->AllocateOutputs();
  OutputImageType *      output = this->GetOutput();
  const InputImageType * input = this->GetInput();
  auto                   regionSize = output->GetLargestPossibleRegion().GetSize();

  using DoubleImageType = itk::Image<double, ImageDimension>;

  using CastInputToDoubleImageFilter = itk::CastImageFilter<InputImageType, DoubleImageType>;
  auto inputCastFilter = CastInputToDoubleImageFilter::New();
  inputCastFilter->SetInput(input);
  inputCastFilter->Update();
  // Allocate result (double image)
  auto resultImage = DoubleImageType::New();
  resultImage->SetRegions(output->GetLargestPossibleRegion());
  resultImage->Allocate();
  resultImage->CopyInformation(input);

  /************ proxTV *************/

  const double * inputProxTV = inputCastFilter->GetOutput()->GetBufferPointer();
  double *       resultProxTV = resultImage->GetBufferPointer();
  int            nThreads = itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads();
  int            maxIters = m_MaximumNumberOfIterations;
  double *       info = nullptr;
  if (ImageDimension == 2)
  {
    // int DR2_TV(size_t M (rows), size_t N (cols), double*inputProxTV (image),
    // double W1, double W2, double norm1, double norm2, double*s, int nThreads,
    // int maxit, double* info);
    std::ignore = DR2_TV(regionSize[0],
                         regionSize[1],
                         const_cast<double *>(inputProxTV),
                         m_Weights[0],
                         m_Weights[1],
                         m_Norms[0],
                         m_Norms[1],
                         resultProxTV,
                         nThreads,
                         maxIters,
                         info);
  }
  else
  {
    // int PD_TV(double *y,double *lambdas,double *norms,double *dims,double *x,
    // double *info,int *ns,int nds,int npen,int ncores,int maxIters){
    double norms[ImageDimension];
    double weights[ImageDimension];
    int    elements[ImageDimension];
    double dims[ImageDimension]; // [1, 2, ..., N]
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      weights[i] = m_Weights[i];
      norms[i] = m_Norms[i];
      elements[i] = regionSize[i];
      dims[i] = i + 1;
    }
    std::ignore = PD_TV(const_cast<double *>(inputProxTV),
                        weights,
                        norms,
                        dims /* Apply weights in these dims */,
                        resultProxTV,
                        info,
                        elements,
                        ImageDimension /* Number of penalty terms */,
                        ImageDimension /* Number of dimensions */,
                        nThreads,
                        maxIters);
  }

  using CastDoubleToOutputImageFilter = itk::CastImageFilter<DoubleImageType, OutputImageType>;
  auto outputCastFilter = CastDoubleToOutputImageFilter::New();
  outputCastFilter->SetInput(resultImage);
  outputCastFilter->GraftOutput(this->GetOutput());
  outputCastFilter->Update();
  this->GraftOutput(outputCastFilter->GetOutput());
}

} // end namespace itk

#endif // itkProxTVImageFilter_hxx
