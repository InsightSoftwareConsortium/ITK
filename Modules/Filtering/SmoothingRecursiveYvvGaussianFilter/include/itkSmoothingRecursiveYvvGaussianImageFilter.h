
#ifndef _ITK_SMOOTHING_RECURSIVE_YVV_GAUSSIAN_IMAGE_FILTER_H_
#define _ITK_SMOOTHING_RECURSIVE_YVV_GAUSSIAN_IMAGE_FILTER_H_

#include "itkRecursiveLineYvvGaussianImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkImage.h"
#include "itkPixelTraits.h"
#include "itkCommand.h"
#include "itkFixedArray.h"


namespace itk
{

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_EXPORT SmoothingRecursiveYvvGaussianImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef SmoothingRecursiveYvvGaussianImageFilter      Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;


  /** Pixel Type of the input image */
  typedef TInputImage                     InputImageType;
  typedef TOutputImage                    OutputImageType;
  typedef typename TInputImage::PixelType PixelType;
#ifdef WITH_DOUBLE
  typedef typename NumericTraits<PixelType>::RealType       RealType;
  typedef typename NumericTraits<PixelType>::ScalarRealType ScalarRealType;
#else
  typedef typename NumericTraits<PixelType>::FloatType RealType;
  typedef typename NumericTraits<PixelType>::FloatType ScalarRealType;
#endif

  /** Runtime information support. */
  itkTypeMacro(SmoothingRecursiveYvvGaussianImageFilter, ImageToImageFilter);

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  /** Define the type for the sigma array */
  typedef FixedArray<ScalarRealType, itkGetStaticConstMacro(ImageDimension)> SigmaArrayType;

  /** Define the image type for internal computations
   RealType is usually 'double' in NumericTraits.
   Here we prefer float in order to save memory.  */

  typedef typename NumericTraits<PixelType>::FloatType                    InternalRealType;
  typedef Image<InternalRealType, itkGetStaticConstMacro(ImageDimension)> RealImageType;

  /**  The first in the pipeline  */
  typedef RecursiveLineYvvGaussianImageFilter<InputImageType, RealImageType> FirstGaussianFilterType;

  /**  Smoothing filter type */
  typedef RecursiveLineYvvGaussianImageFilter<RealImageType, RealImageType> InternalGaussianFilterType;

  /**  The last in the pipeline  */
  typedef CastImageFilter<RealImageType, OutputImageType> CastingFilterType;


  /**  Pointer to a gaussian filter.  */
  typedef typename InternalGaussianFilterType::Pointer InternalGaussianFilterPointer;

  /**  Pointer to the first gaussian filter.  */
  typedef typename FirstGaussianFilterType::Pointer FirstGaussianFilterPointer;

  /**  Pointer to the last filter, casting  */
  typedef typename CastingFilterType::Pointer CastingFilterPointer;

  /**  Pointer to the Output Image */
  typedef typename OutputImageType::Pointer OutputImagePointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set Sigma value. Sigma is measured in the units of image spacing. You
   may use the method SetSigma to set the same value across each axis or
   use the method SetSigmaArray if you need different values along each
   axis. */
  void
  SetSigmaArray(const SigmaArrayType & sigmas);
  void
  SetSigma(ScalarRealType sigma);
  SigmaArrayType
  GetSigmaArray() const;
  ScalarRealType
  GetSigma() const;

  /** Define which normalization factor will be used for the Gaussian */
  void
  SetNormalizeAcrossScale(bool normalizeInScaleSpace);
  itkGetConstMacro(NormalizeAcrossScale, bool);

  void
  SetNumberOfThreads(int nb);
  using Superclass::SetNumberOfThreads;

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<PixelType>));
  /** End concept checking */
#endif

protected:
  SmoothingRecursiveYvvGaussianImageFilter();
  virtual ~SmoothingRecursiveYvvGaussianImageFilter() {};
  void
  PrintSelf(std::ostream & os, Indent indent) const;

  /** Generate Data */
  void
  GenerateData(void);

  /** SmoothingRecursiveYvvGaussianImageFilter needs all of the input to produce an
   * output. Therefore, SmoothingRecursiveYvvGaussianImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void
  GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

  // Override since the filter produces the entire dataset
  void
  EnlargeOutputRequestedRegion(DataObject * output);

private:
  SmoothingRecursiveYvvGaussianImageFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  InternalGaussianFilterPointer m_SmoothingFilters[ImageDimension - 1];
  FirstGaussianFilterPointer    m_FirstSmoothingFilter;
  CastingFilterPointer          m_CastingFilter;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;

  /** Standard deviation of the gaussian used for smoothing */
  SigmaArrayType m_Sigma;
  int            telltale; // TODO: REMOVE
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSmoothingRecursiveYvvGaussianImageFilter.hxx"
#endif

#endif
