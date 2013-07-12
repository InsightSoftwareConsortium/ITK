#ifdef GPU

#  ifndef _ITK_GPU_SMOOTHING_RECURSIVE_YVV_GAUSSIAN_IMAGE_FILTER_HXX_
#    define _ITK_GPU_SMOOTHING_RECURSIVE_YVV_GAUSSIAN_IMAGE_FILTER_HXX_

#    include "itkGPUSmoothingRecursiveYvvGaussianImageFilter.h"

// #define VERBOSE
namespace itk
{

/**
 * Constructor
 */
template <typename TInputImage, typename TOutputImage>
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GPUSmoothingRecursiveYvvGaussianImageFilter()
{
#    ifdef VERBOSE
  telltale = rand();
  std::cout << telltale << ". GPUSmoothing::constructor \n";
#    endif
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
  const char *                      GPUSource = GPUSmoothingRecursiveYvvGaussianImageFilterKernel::GetOpenCLSource();
  typename GPUOutputImage::SizeType lineSizes = inPtr->GetRequestedRegion().GetSize();
  unsigned int                      maxLineSize = lineSizes[0];
  for (unsigned int d = 1; d < ImageDimension; ++d)
  {
    maxLineSize = maxLineSize < lineSizes[d] ? lineSizes[d] : maxLineSize;
  }

#    ifdef WITH_DOUBLE
  defines << "#pragma OPENCL EXTENSION cl_khr_fp64 : enable \n";
#    endif

#    ifdef NVIDIA
  defines << "#pragma OPENCL EXTENSION cl_nv_pragma_unroll : enable \n";
#    endif

  defines << "#define DIM_" << TInputImage::ImageDimension << "\n";
  defines << "#define INPIXELTYPE ";
  GetTypenameInString(typeid(typename TInputImage::PixelType), defines);
  defines << "#define OUTPIXELTYPE ";
  GetTypenameInString(typeid(typename TOutputImage::PixelType), defines);
  defines << "#define REALTYPE ";
  GetTypenameInString(typeid(ScalarRealType), defines);
  defines << "#define MAX_LINE_LENGTH " << maxLineSize << "\n";

#    ifdef VERBOSE
  std::cout << telltale << ".   Defines: \n" << defines.str() << "\n";
#    endif

  // load, build, create kernel
  this->m_GPUKernelManager->LoadProgramFromString(GPUSource, defines.str().c_str());
  m_FilterGPUKernelHandle = this->m_GPUKernelManager->CreateKernel("YvvFilter");
}

/**
 *   Compute filter for Gaussian kernel.
 */
template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetUp(ScalarRealType spacing)
{
#    ifdef VERBOSE
  std::cout << telltale << ". GPUSmoothing::SetUp with spacing " << spacing << "\n";
#    endif
  const ScalarRealType sigmad = this->GetSigma() / spacing;

  if (this->GetSigma() >= 0.5)
  {
#    ifdef VERBOSE
    std::cout << telltale << ". GPUSmoothing::SetUp with sigma " << m_Sigma << "\n";
#    endif
    // Compute q according to 16 in Young et al on Gabor filering
    ScalarRealType q = 0;
    if (sigmad >= 3.556)
      q = 0.9804 * (sigmad - 3.556) + 2.5091;
    else
    {
      if (sigmad < 0.5)
        std::cerr << "Too low sigma value (< 0.5), computation will not be precise." << std::endl;

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

    // M Matrix for initialization on backward pass, from Triggs and Sdika, IEEE TSP
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

#    ifdef VERBOSE
    for (int i = 0; i < 4; ++i)
    {
      std::cout << "B" << i << "  " << m_Bvalues[i] << std::endl;
    }

    for (int i = 0; i < 9; ++i)
    {
      std::cout << "M" << i << "  " << m_CPUMatrix[i] << std::endl;
    }
#    endif
    this->AllocateGPUCoefficients();
    this->Modified();
  }
}

template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetInput(const TInputImage * input)
{
#    ifdef VERBOSE
  std::cout << telltale << ".   GPUSmoothingRecursiveYvvGaussianImageFilter::SetInput \n";
#    endif
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

// Set value of Sigma (isotropic)
template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetSigma(ScalarRealType sigma)
{
#    ifdef VERBOSE
  std::cout << telltale << ". GPUSmoothing::SetSigma: " << sigma << " \n";
#    endif
  SigmaArrayType sigmas(sigma);
  this->SetSigmaArray(sigmas);
}


// Set value of Sigma (an-isotropic)

template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetSigmaArray(const SigmaArrayType & sigma)
{
#    ifdef VERBOSE
  std::cout << telltale << ". GPUSmoothing::SetSigmaArray" << m_Sigma << " vs new: " << sigma[0] << " \n";
#    endif
  if (this->m_Sigma != sigma)
  {
    this->m_Sigma = sigma;

    const typename InputImageType::SpacingType & pixelSize = this->GetOutput()->GetSpacing();
    if (pixelSize[0] != 0)
    {
#    ifdef VERBOSE
      std::cout << telltale << ". GPUSmoothing::SetSigmaArray. pixelSize: [" << pixelSize[0] << "," << pixelSize[1]
                << "," << pixelSize[2] << "] \n";
#    endif
      this->SetUp(pixelSize[0]);
    }
    this->Modified();
  }
}


// Get the sigma array.
template <typename TInputImage, typename TOutputImage>
typename GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SigmaArrayType
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GetSigmaArray() const
{
  /*#ifdef VERBOSE
          std::cout<< telltale << ". GPUSmoothing::GetSigmaArray \n";
  #endif*/
  return m_Sigma;
}


// Get the sigma scalar. If the sigma is anisotropic, we will just
// return the sigma along the first dimension.
template <typename TInputImage, typename TOutputImage>
typename GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::ScalarRealType
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GetSigma() const
{
  /*#ifdef VERBOSE
          std::cout<< telltale << ". GPUSmoothing::GetSigma \n";
  #endif*/
  return m_Sigma[0];
}


/**
 * Set Normalize Across Scale Space
 */
template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetNormalizeAcrossScale(bool normalize)
{
  /*#ifdef VERBOSE
          std::cout<< telltale << ". GPUSmoothing::SetNormalizeAcrossScale \n";
  #endif*/
  m_NormalizeAcrossScale = normalize;
  this->Modified();
}

template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion() throw(
  InvalidRequestedRegionError)
{
#    ifdef VERBOSE
  std::cout << telltale << ". GPUSmoothing::GenerateInputRequestedRegion \n";
#    endif
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
#    ifdef VERBOSE
  std::cout << telltale << ". GPUSmoothing::EnlargeOutputRequestedRegion \n";
#    endif
  TOutputImage * out = dynamic_cast<TOutputImage *>(output);

  if (out)
  {
    out->SetRequestedRegion(out->GetLargestPossibleRegion());
  }
}


template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::AllocateGPUCoefficients()
{
#    ifdef VERBOSE
  std::cout << telltale << ".   GPUSmoothingRecursiveYvvGaussianImageFilter::AllocateGPUInputBuffer \n";
#    endif

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
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GPUGenerateData(void)
{
#    ifdef VERBOSE
  std::cout << telltale << ".    GPUSmoothing::GPUGenerateData \n";
#    endif
  if (m_FilterGPUKernelHandle == -1)
  {
    this->BuildKernel();
  }

  const unsigned int X = 0;
  const unsigned int Y = 1;
  const unsigned int Z = 2;
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
#    ifdef VERBOSE
    std::cout << telltale << ".Arg     " << argidx << ": " << m_requestedSize[i] << "\n";
#    endif
  }

  const unsigned int dimArg = argidx;

  if (ndim < 3)
  {
    int globalSize1D = (m_requestedSize[1] > m_requestedSize[0] ? m_requestedSize[1] : m_requestedSize[0]);

    this->m_GPUKernelManager->SetKernelArg(m_FilterGPUKernelHandle, dimArg, sizeof(unsigned int), &(X));
#    ifdef VERBOSE
    std::cout << telltale << ". Calling 1D kernel on X.\n";
#    endif
    this->m_GPUKernelManager->LaunchKernel1D(m_FilterGPUKernelHandle, globalSize1D, 16);

    // change ONLY input and direction of filter
    this->m_GPUKernelManager->SetKernelArgWithImage(m_FilterGPUKernelHandle, 0, otPtr->GetGPUDataManager());
    this->m_GPUKernelManager->SetKernelArg(m_FilterGPUKernelHandle, dimArg, sizeof(unsigned int), &(Y));
#    ifdef VERBOSE
    std::cout << telltale << ". Calling 1D kernel on Y.\n";
#    endif
    this->m_GPUKernelManager->LaunchKernel1D(m_FilterGPUKernelHandle, globalSize1D, 16);
  }
  else
  {
    // We must optimize our 2D workgroup sizes to go over our 3D volume in each dimension.

    this->m_GPUKernelManager->SetKernelArg(m_FilterGPUKernelHandle, argidx++, sizeof(unsigned int), &(X));
#    ifdef VERBOSE
    std::cout << telltale << ". Calling 2D kernel on X.\n";
#    endif
    this->m_GPUKernelManager->LaunchKernel2D(m_FilterGPUKernelHandle, m_requestedSize[2], m_requestedSize[1], 16, 16);

    // change ONLY input and direction of filter
    this->m_GPUKernelManager->SetKernelArgWithImage(m_FilterGPUKernelHandle, 0, otPtr->GetGPUDataManager());
    this->m_GPUKernelManager->SetKernelArg(m_FilterGPUKernelHandle, dimArg, sizeof(unsigned int), &(Y));
#    ifdef VERBOSE
    std::cout << telltale << ". Calling 2D kernel on Y.\n";
#    endif
    this->m_GPUKernelManager->LaunchKernel2D(m_FilterGPUKernelHandle, m_requestedSize[0], m_requestedSize[2], 16, 16);

    // input is already pointing to previous output; change ONLY direction of filter
    this->m_GPUKernelManager->SetKernelArg(m_FilterGPUKernelHandle, dimArg, sizeof(unsigned int), &(Z));
#    ifdef VERBOSE
    std::cout << telltale << ". Calling 2D kernel on Z.\n";
#    endif
    this->m_GPUKernelManager->LaunchKernel2D(m_FilterGPUKernelHandle, m_requestedSize[0], m_requestedSize[1], 16, 16);
  }
}


template <typename TInputImage, typename TOutputImage>
void
GPUSmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os,
                                                                                  Indent         indent) const
{
  /*#ifdef VERBOSE
          std::cout<< telltale << ". GPUSmoothing::PrintSelf \n";
  #endif*/
  Superclass::PrintSelf(os, indent);

  os << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
  os << "Sigma: " << m_Sigma << std::endl;
}

} // end namespace itk

#  endif //_ITK_GPU_SMOOTHING_RECURSIVE_YVV_GAUSSIAN_IMAGE_FILTER_HXX_
#endif   // GPU
