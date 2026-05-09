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

#ifndef itkGPUSmoothingRecursiveYvvGaussianImageFilter_hxx
#define itkGPUSmoothingRecursiveYvvGaussianImageFilter_hxx

#ifdef GPU

#  include "itkGPUSmoothingRecursiveYvvGaussianImageFilter.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GPUSmoothingRecursiveYvvGaussianImageFilter()
{
  m_NormalizeAcrossScale = false;
  otPtr = dynamic_cast<GPUOutputImage *>(this->ProcessObject::GetOutput(0));

  // NB: We must call SetSigma in order to initialize the smoothing
  // filters with the default scale.  However, m_Sigma must first be
  // initialized (it is used inside SetSigma) and it must be >= 0.5
  // or the call will be ignored.

  this->m_Sigma.Fill(0.0);
  this->SetSigma(0.1);
  m_FilterGPUKernelHandle = -1;
}

template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::BuildKernel()
{
  const char * GPUSource = GPUSmoothingRecursiveYvvGaussianImageFilterKernel::GetOpenCLSource();

  typename GPUOutputImage::SizeType lineSizes = inPtr->GetRequestedRegion().GetSize();
  unsigned int                      maxLineSize = lineSizes[0];
  for (unsigned int d = 1; d < ImageDimension; ++d)
  {
    maxLineSize = maxLineSize < lineSizes[d] ? lineSizes[d] : maxLineSize;
  }

#  ifdef WITH_DOUBLE
  defines << "#pragma OPENCL EXTENSION cl_khr_fp64 : enable \n";
#  endif

#  ifdef NVIDIA
  defines << "#pragma OPENCL EXTENSION cl_nv_pragma_unroll : enable \n";
#  endif

  defines << "#define DIM_" << TInputImage::ImageDimension << "\n";
  defines << "#define INPIXELTYPE ";
  GetTypenameInString(typeid(typename TInputImage::PixelType), defines);
  defines << "#define OUTPIXELTYPE ";
  GetTypenameInString(typeid(typename TOutputImage::PixelType), defines);
  defines << "#define REALTYPE ";
  GetTypenameInString(typeid(ScalarRealType), defines);
  defines << "#define MAX_LINE_LENGTH " << maxLineSize << "\n";

  // load, build, create kernel
  this->m_GPUKernelManager->LoadProgramFromString(GPUSource, defines.str().c_str());
  m_FilterGPUKernelHandle = this->m_GPUKernelManager->CreateKernel("YvvFilter");
}

template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetUp(ScalarRealType spacing)
{
  const ScalarRealType sigmad = this->GetSigma() / spacing;

  if (this->GetSigma() >= 0.5)
  {
    // Compute q according to 16 in Young et al on Gabor filering
    ScalarRealType q = 0;
    if (sigmad >= 3.556)
    {
      q = 0.9804 * (sigmad - 3.556) + 2.5091;
    }
    else
    {
      if (sigmad < 0.5)
      {
        std::cerr << "Too low sigma value (< 0.5), computation will not be precise." << std::endl;
      }

      q = 0.0561 * sigmad * sigmad + 0.5784 * sigmad - 0.2568;
    }

    // Compute B and B0 to B3 according to Young et al 1995

    ScalarRealType m0 = 1.16680;
    ScalarRealType m1 = 1.10783;
    ScalarRealType m2 = 1.40586;
    ScalarRealType scale = (m0 + q) * (m1 * m1 + m2 * m2 + 2 * m1 * q + q * q);
    m_Bvalues = new ScalarRealType[4];

    m_B1 = m_Bvalues[1] = q * (2 * m0 * m1 + m1 * m1 + m2 * m2 + (2 * m0 + 4 * m1) * q + 3 * q * q) / scale;
    m_B2 = m_Bvalues[2] = -q * q * (m0 + 2 * m1 + 3 * q) / scale;
    m_B3 = m_Bvalues[3] = q * q * q / scale;

    ScalarRealType baseB = (m0 * (m1 * m1 + m2 * m2)) / scale;
    m_B = m_Bvalues[0] = baseB * baseB;

    // M Matrix for initialization on backward pass, from Triggs and Sdika, IEEE
    // TSP
    m_CPUMatrix = new ScalarRealType[9];
    const ScalarRealType factor =
      (1 + m_B1 - m_B2 + m_B3) * (1 - m_B1 - m_B2 - m_B3) * (1 + m_B2 + (m_B1 - m_B3) * m_B3);

    m_CPUMatrix[0] = (-m_B3 * m_B1 + 1 - m_B3 * m_B3 - m_B2) / factor;
    m_CPUMatrix[1] = ((m_B3 + m_B1) * (m_B2 + m_B3 * m_B1)) / factor;
    m_CPUMatrix[2] = (m_B3 * (m_B1 + m_B3 * m_B2)) / factor;

    m_CPUMatrix[3] = (m_B1 + m_B3 * m_B2) / factor;
    m_CPUMatrix[4] = ((1 - m_B2) * (m_B2 + m_B3 * m_B1)) / factor;
    m_CPUMatrix[5] = (-m_B3 * (m_B3 * m_B1 + m_B3 * m_B3 + m_B2 - 1)) / factor;

    m_CPUMatrix[6] = (m_B3 * m_B1 + m_B2 + m_B1 * m_B1 - m_B2 * m_B2) / factor;
    m_CPUMatrix[7] =
      (m_B1 * m_B2 + m_B3 * m_B2 * m_B2 - m_B1 * m_B3 * m_B3 - m_B3 * m_B3 * m_B3 - m_B3 * m_B2 + m_B3) / factor;
    m_CPUMatrix[8] = (m_B3 * (m_B1 + m_B3 * m_B2)) / factor;

    if (this->GetDebug())
    {
      for (int i = 0; i < 4; ++i)
      {
        std::cout << "B" << i << "  " << m_Bvalues[i] << std::endl;
      }

      for (int i = 0; i < 9; ++i)
      {
        std::cout << "M" << i << "  " << m_CPUMatrix[i] << std::endl;
      }
    }
    this->AllocateGPUCoefficients();
    this->Modified();
  }
}

template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetInput(const TInputImage * input)
{
  // ProcessObject is not const_correct so this const_cast is required
  ProcessObject::SetNthInput(0, const_cast<TInputImage *>(input));

  inPtr = dynamic_cast<GPUInputImage *>(this->ProcessObject::GetInput(0));
  inPtr->GetGPUDataManager()->SetBufferFlag(CL_MEM_READ_ONLY);

  m_requestedSize = inPtr->GetRequestedRegion().GetSize();

  itkDebugMacro(<< "GPUSmoothingRecursiveYvvGaussianImageFilter generating data ");

  for (unsigned int d = 0; d < ImageDimension; d++)
  {
    if (m_requestedSize[d] < 4)
    {
      itkExceptionMacro(
        "The number of pixels along dimension "
        << d << " is less than 4. This filter requires a minimum of four pixels along the dimension to be processed.");
    }
  }
}

template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetSigma(ScalarRealType sigma)
{
  SigmaArrayType sigmas(sigma);
  this->SetSigmaArray(sigmas);
}

template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetSigmaArray(const SigmaArrayType & sigma)
{
  if (this->m_Sigma != sigma)
  {
    this->m_Sigma = sigma;

    const typename InputImageType::SpacingType & pixelSize = this->GetOutput()->GetSpacing();
    if (pixelSize[0] != 0)
    {
      this->SetUp(pixelSize[0]);
    }
    this->Modified();
  }
}

template <typename TInputImage, typename TOutputImage>
typename GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SigmaArrayType
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GetSigmaArray() const
{
  return m_Sigma;
}

// Get the sigma scalar. If the sigma is anisotropic, we will just
// return the sigma along the first dimension.
template <typename TInputImage, typename TOutputImage>
typename GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::ScalarRealType
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GetSigma() const
{
  return m_Sigma[0];
}

template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetNormalizeAcrossScale(bool normalize)
{
  m_NormalizeAcrossScale = normalize;
  this->Modified();
}

template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion() ITK_NOEXCEPT
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // This filter needs all of the input
  typename GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::InputImagePointer image =
    const_cast<InputImageType *>(this->GetInput());
  if (image)
  {
    image->SetRequestedRegion(this->GetInput()->GetLargestPossibleRegion());
  }
}

template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(
  DataObject * output)
{
  auto * out = dynamic_cast<TOutputImage *>(output);

  if (out)
  {
    out->SetRequestedRegion(out->GetLargestPossibleRegion());
  }
}

template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::AllocateGPUCoefficients()
{
  m_GPUBCoefficientsDataManager = GPUDataManager::New();
  m_GPUBCoefficientsDataManager->SetBufferSize(4 * sizeof(ScalarRealType));
  m_GPUBCoefficientsDataManager->SetCPUBufferPointer(m_Bvalues);
  m_GPUBCoefficientsDataManager->SetBufferFlag(CL_MEM_READ_ONLY);
  m_GPUBCoefficientsDataManager->Allocate();
  if (m_Bvalues)
  {
    m_GPUBCoefficientsDataManager->SetGPUDirtyFlag(true);
  }

  m_GPUMMatrixDataManager = GPUDataManager::New();
  m_GPUMMatrixDataManager->SetBufferSize(9 * sizeof(ScalarRealType));
  m_GPUMMatrixDataManager->SetCPUBufferPointer(m_CPUMatrix);
  m_GPUMMatrixDataManager->SetBufferFlag(CL_MEM_READ_ONLY);
  m_GPUMMatrixDataManager->Allocate();
  if (m_CPUMatrix)
  {
    m_GPUMMatrixDataManager->SetGPUDirtyFlag(true);
  }
}

/**
 * Compute filter for Gaussian kernel
 */
template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GPUGenerateData()
{
  if (m_FilterGPUKernelHandle == -1)
  {
    this->BuildKernel();
  }

  constexpr unsigned int X = 0;
  constexpr unsigned int Y = 1;
  constexpr unsigned int Z = 2;
  // arguments set up
  int       argidx = 0;
  const int ndim = (int)TInputImage::ImageDimension;

  this->m_GPUKernelManager->SetKernelArgWithImage(m_FilterGPUKernelHandle, argidx++, inPtr->GetGPUDataManager());
  this->m_GPUKernelManager->SetKernelArgWithImage(m_FilterGPUKernelHandle, argidx++, otPtr->GetGPUDataManager());
  this->m_GPUKernelManager->SetKernelArgWithImage(m_FilterGPUKernelHandle, argidx++, m_GPUBCoefficientsDataManager);
  this->m_GPUKernelManager->SetKernelArgWithImage(m_FilterGPUKernelHandle, argidx++, m_GPUMMatrixDataManager);

  for (int i = 0; i < ndim; ++i)
  {
    this->m_GPUKernelManager->SetKernelArg(m_FilterGPUKernelHandle, argidx++, sizeof(int), &(m_requestedSize[i]));
  }

  const unsigned int dimArg = argidx;

  if (ndim < 3)
  {
    int globalSize1D = (m_requestedSize[1] > m_requestedSize[0] ? m_requestedSize[1] : m_requestedSize[0]);

    this->m_GPUKernelManager->SetKernelArg(m_FilterGPUKernelHandle, dimArg, sizeof(unsigned int), &(X));
    this->m_GPUKernelManager->LaunchKernel1D(m_FilterGPUKernelHandle, globalSize1D, 16);

    // change ONLY input and direction of filter
    this->m_GPUKernelManager->SetKernelArgWithImage(m_FilterGPUKernelHandle, 0, otPtr->GetGPUDataManager());
    this->m_GPUKernelManager->SetKernelArg(m_FilterGPUKernelHandle, dimArg, sizeof(unsigned int), &(Y));
    this->m_GPUKernelManager->LaunchKernel1D(m_FilterGPUKernelHandle, globalSize1D, 16);
  }
  else
  {
    // We must optimize our 2D workgroup sizes to go over our 3D volume in each
    // dimension.

    this->m_GPUKernelManager->SetKernelArg(m_FilterGPUKernelHandle, argidx++, sizeof(unsigned int), &(X));
    this->m_GPUKernelManager->LaunchKernel2D(m_FilterGPUKernelHandle, m_requestedSize[2], m_requestedSize[1], 16, 16);

    // change ONLY input and direction of filter
    this->m_GPUKernelManager->SetKernelArgWithImage(m_FilterGPUKernelHandle, 0, otPtr->GetGPUDataManager());
    this->m_GPUKernelManager->SetKernelArg(m_FilterGPUKernelHandle, dimArg, sizeof(unsigned int), &(Y));
    this->m_GPUKernelManager->LaunchKernel2D(m_FilterGPUKernelHandle, m_requestedSize[0], m_requestedSize[2], 16, 16);

    // input is already pointing to previous output; change ONLY direction of
    // filter
    this->m_GPUKernelManager->SetKernelArg(m_FilterGPUKernelHandle, dimArg, sizeof(unsigned int), &(Z));
    this->m_GPUKernelManager->LaunchKernel2D(m_FilterGPUKernelHandle, m_requestedSize[0], m_requestedSize[1], 16, 16);
  }
}

template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os,
                                                                                  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
  os << "Sigma: " << m_Sigma << std::endl;
}
} // end namespace itk

#endif // GPU
#endif // itkGPUSmoothingRecursiveYvvGaussianImageFilter_hxx
