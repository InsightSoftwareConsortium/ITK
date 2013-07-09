#ifndef _ITK_SMOOTHING_RECURSIVE_YVV_GAUSSIAN_IMAGE_FILTER_HXX_
#define _ITK_SMOOTHING_RECURSIVE_YVV_GAUSSIAN_IMAGE_FILTER_HXX_

#include "itkSmoothingRecursiveYvvGaussianImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressAccumulator.h"

// #define VERBOSE

namespace itk
{

/**
 * Constructor
 */
template <typename TInputImage, typename TOutputImage>
SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SmoothingRecursiveYvvGaussianImageFilter()
{
  /*#ifdef VERBOSE
          telltale = rand();
          std::cout<<telltale << ". itkSmoothingYvv::constructor \n";
  #endif*/
  m_NormalizeAcrossScale = false;

  m_FirstSmoothingFilter = FirstGaussianFilterType::New();
  m_FirstSmoothingFilter->SetDirection(0);
  m_FirstSmoothingFilter->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
  m_FirstSmoothingFilter->ReleaseDataFlagOn();

  for (unsigned int i = 0; i < ImageDimension - 1; i++)
  {
    m_SmoothingFilters[i] = InternalGaussianFilterType::New();
    m_SmoothingFilters[i]->SetNormalizeAcrossScale(m_NormalizeAcrossScale);
    m_SmoothingFilters[i]->SetDirection(i + 1);
    m_SmoothingFilters[i]->ReleaseDataFlagOn();
  }

  m_SmoothingFilters[0]->SetInput(m_FirstSmoothingFilter->GetOutput());
  for (unsigned int i = 1; i < ImageDimension - 1; i++)
  {
    m_SmoothingFilters[i]->SetInput(m_SmoothingFilters[i - 1]->GetOutput());
  }

  m_CastingFilter = CastingFilterType::New();
  m_CastingFilter->SetInput(m_SmoothingFilters[ImageDimension - 2]->GetOutput());

  //
  // NB: We must call SetSigma in order to initialize the smoothing
  // filters with the default scale.  However, m_Sigma must first be
  // initialized (it is used inside SetSigma) and it must be different
  // from 1.0 or the call will be ignored.
  this->m_Sigma.Fill(0.0);
  this->SetSigma(1.0);

#ifdef VERBOSE
  std::cout << "-----------Smoothing filter TYPES\n";

  if (typeid(typename TInputImage::PixelType) == typeid(double))
    std::cout << "PixelType double\n";
  if (typeid(typename TOutputImage::PixelType) == typeid(double))
    std::cout << "Output PixelType double\n";

  if (typeid(ScalarRealType) == typeid(double))
    std::cout << "ScalarRealType double\n";

  if (typeid(RealType) == typeid(double))
    std::cout << "RealType double\n";

  if (typeid(InternalRealType) == typeid(double))
    std::cout << "InternalRealType double\n";

#endif
}


template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetNumberOfThreads(int nb)
{
  /*#ifdef VERBOSE
          std::cout<<telltale  << ". itkSmoothingYvv::SetNumberOfThreads \n";
  #endif*/
  Superclass::SetNumberOfThreads(nb);
  for (unsigned int i = 0; i < ImageDimension - 1; i++)
  {
    m_SmoothingFilters[i]->SetNumberOfThreads(nb);
  }
  m_FirstSmoothingFilter->SetNumberOfThreads(nb);
}


// Set value of Sigma (isotropic)

template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetSigma(ScalarRealType sigma)
{
  /*#ifdef VERBOSE
          std::cout<<telltale  << ". itkSmoothingYvv::SetSigma \n";
  #endif*/
  SigmaArrayType sigmas(sigma);
  this->SetSigmaArray(sigmas);
}


// Set value of Sigma (an-isotropic)

template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetSigmaArray(const SigmaArrayType & sigma)
{
  /*#ifdef VERBOSE
          std::cout<<telltale  << ". itkSmoothingYvv::SetSigmaArray \n";
  #endif*/
  if (this->m_Sigma != sigma)
  {
    this->m_Sigma = sigma;
    for (unsigned int i = 0; i < ImageDimension - 1; i++)
    {
      m_SmoothingFilters[i]->SetSigma(m_Sigma[i + 1]);
      m_SmoothingFilters[i]->Modified();
    }
    m_FirstSmoothingFilter->SetSigma(m_Sigma[0]);
    m_FirstSmoothingFilter->Modified();

    this->Modified();
  }
}


// Get the sigma array.
template <typename TInputImage, typename TOutputImage>
typename SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SigmaArrayType
SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GetSigmaArray() const
{
  /*#ifdef VERBOSE
          std::cout<<telltale  << ". itkSmoothingYvv::GetSigmaArray \n";
  #endif*/
  return m_Sigma;
}


// Get the sigma scalar. If the sigma is anisotropic, we will just
// return the sigma along the first dimension.
template <typename TInputImage, typename TOutputImage>
typename SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::ScalarRealType
SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GetSigma() const
{
  /*#ifdef VERBOSE
          std::cout<<telltale  << ". itkSmoothingYvv::GetSigma \n";
  #endif*/
  return m_Sigma[0];
}


/**
 * Set Normalize Across Scale Space
 */
template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::SetNormalizeAcrossScale(bool normalize)
{
  /*#ifdef VERBOSE
          std::cout<<telltale  << ". itkSmoothingYvv::SetNormalizeAcrossScale \n";
  #endif*/
  m_NormalizeAcrossScale = normalize;

  for (unsigned int i = 0; i < ImageDimension - 1; i++)
  {
    m_SmoothingFilters[i]->SetNormalizeAcrossScale(normalize);
  }
  m_FirstSmoothingFilter->SetNormalizeAcrossScale(normalize);

  this->Modified();
}


//
//
//
template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion() throw(
  InvalidRequestedRegionError)
{
  /*#ifdef VERBOSE
          std::cout<<telltale  << ". itkSmoothingYvv::GenerateInputRequestedRegion \n";
  #endif*/
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // This filter needs all of the input
  typename SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::InputImagePointer image =
    const_cast<InputImageType *>(this->GetInput());
  if (image)
  {
    image->SetRequestedRegion(this->GetInput()->GetLargestPossibleRegion());
  }
}

template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject * output)
{
  /*#ifdef VERBOSE
          std::cout<<telltale  << ". itkSmoothingYvv::EnlargeOutputRequestedRegion \n";
  #endif*/
  TOutputImage * out = dynamic_cast<TOutputImage *>(output);

  if (out)
  {
    out->SetRequestedRegion(out->GetLargestPossibleRegion());
  }
}

/**
 * Compute filter for Gaussian kernel
 */
template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::GenerateData(void)
{
  /*#ifdef VERBOSE
          std::cout<<telltale  << ". itkSmoothingYvv::GenerateData \n";
  #endif*/
  itkDebugMacro(<< "SmoothingRecursiveYvvGaussianImageFilter generating data ");

  const typename TInputImage::ConstPointer inputImage(this->GetInput());

  const typename TInputImage::RegionType region = inputImage->GetRequestedRegion();
  const typename TInputImage::SizeType   size = region.GetSize();

  for (unsigned int d = 0; d < ImageDimension; d++)
  {
    if (size[d] < 4)
    {
      itkExceptionMacro(
        "The number of pixels along dimension "
        << d << " is less than 4. This filter requires a minimum of four pixels along the dimension to be processed.");
    }
  }


  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Register the filter with the with progress accumulator using
  // equal weight proportion
  for (unsigned int i = 0; i < ImageDimension - 1; i++)
  {
    progress->RegisterInternalFilter(m_SmoothingFilters[i], 1.0 / (ImageDimension));
  }

  progress->RegisterInternalFilter(m_FirstSmoothingFilter, 1.0 / (ImageDimension));
  m_FirstSmoothingFilter->SetInput(inputImage);
  // graft our output to the internal filter to force the proper regions
  // to be generated
  m_CastingFilter->GraftOutput(this->GetOutput());
  m_CastingFilter->Update();
  this->GraftOutput(m_CastingFilter->GetOutput());
}


template <typename TInputImage, typename TOutputImage>
void
SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  /*#ifdef VERBOSE
          std::cout<<telltale  << ". itkSmoothingYvv::PrintSelf \n";
  #endif*/
  Superclass::PrintSelf(os, indent);

  os << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
  os << "Sigma: " << m_Sigma << std::endl;
}


} // end namespace itk

#endif
