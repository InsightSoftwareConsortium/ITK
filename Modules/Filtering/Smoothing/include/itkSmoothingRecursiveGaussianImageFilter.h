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
#ifndef __itkSmoothingRecursiveGaussianImageFilter_h
#define __itkSmoothingRecursiveGaussianImageFilter_h

#include "itkRecursiveGaussianImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkImage.h"
#include "itkPixelTraits.h"
#include "itkCommand.h"

namespace itk
{
/** \class SmoothingRecursiveGaussianImageFilter
 * \brief Computes the smoothing of an image by convolution
 *        with the Gaussian kernels implemented as IIR filters.
 *
 * This filter is implemented using the recursive gaussian
 * filters. For multi-component images, the filter works on each
 * component independently.
 *
 * For this filter to be able to run in-place the input and output
 * image types need to be the same and/or the same type as the
 * RealImageType.
 *
 * \ingroup IntensityImageFilters
 * \ingroup SingleThreaded
 * \ingroup ITKSmoothing
 *
 * \wiki
 * \wikiexample{Smoothing/SmoothingRecursiveGaussianImageFilter,Gaussian smoothing that works with image adaptors}
 * \endwiki
 */
template< typename TInputImage,
          typename TOutputImage = TInputImage >
class SmoothingRecursiveGaussianImageFilter:
  public InPlaceImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef SmoothingRecursiveGaussianImageFilter           Self;
  typedef InPlaceImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Pixel Type of the input image */
  typedef TInputImage                                         InputImageType;
  typedef TOutputImage                                        OutputImageType;
  typedef typename TInputImage::PixelType                     PixelType;
  typedef typename NumericTraits< PixelType >::RealType       RealType;
  typedef typename NumericTraits< PixelType >::ScalarRealType ScalarRealType;

  /** Runtime information support. */
  itkTypeMacro(SmoothingRecursiveGaussianImageFilter,
               ImageToImageFilter);

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Define the type for the sigma array */
  typedef FixedArray< ScalarRealType,
                      itkGetStaticConstMacro(ImageDimension) > SigmaArrayType;

  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */

  typedef typename NumericTraits< PixelType >::FloatType InternalRealType;
  typedef typename InputImageType::template Rebind<InternalRealType>::Type RealImageType;

  /**  The first in the pipeline  */
  typedef RecursiveGaussianImageFilter<
    InputImageType,
    RealImageType
    >    FirstGaussianFilterType;

  /**  Smoothing filter type */
  typedef RecursiveGaussianImageFilter<
    RealImageType,
    RealImageType
    >    InternalGaussianFilterType;

  /**  The last in the pipeline  */
  typedef CastImageFilter<
    RealImageType,
    OutputImageType
    >    CastingFilterType;

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
  void SetSigmaArray(const SigmaArrayType & sigmas);
  void SetSigma(ScalarRealType sigma);

  SigmaArrayType GetSigmaArray() const;
  ScalarRealType GetSigma() const;

  /** This method does not effect the output of this filter.
   *
   *  \sa  RecursiveGaussianImageFilter::SetNormalizeAcrossScale
   */
  void SetNormalizeAcrossScale(bool normalizeInScaleSpace);
  itkGetConstMacro(NormalizeAcrossScale, bool);

  // See super class for doxygen documentation
  //
  void SetNumberOfThreads(ThreadIdType nb);

  // See super class for doxygen documentation
  //
  virtual bool CanRunInPlace( void ) const;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  // This concept does not work with variable length vector images
  //itkConceptMacro( InputHasNumericTraitsCheck,
  //( Concept::HasNumericTraits< PixelType > ) );
  // End concept checking
#endif

protected:
  SmoothingRecursiveGaussianImageFilter();
  virtual ~SmoothingRecursiveGaussianImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Generate Data */
  void GenerateData(void);

  /** SmoothingRecursiveGaussianImageFilter needs all of the input to produce an
   * output. Therefore, SmoothingRecursiveGaussianImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();

  // Override since the filter produces the entire dataset
  void EnlargeOutputRequestedRegion(DataObject *output);

private:
  SmoothingRecursiveGaussianImageFilter(const Self &); //purposely not
                                                       // implemented
  void operator=(const Self &);                        //purposely not

  // implemented

  InternalGaussianFilterPointer m_SmoothingFilters[ImageDimension - 1];
  FirstGaussianFilterPointer    m_FirstSmoothingFilter;
  CastingFilterPointer          m_CastingFilter;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;

  /** Standard deviation of the gaussian used for smoothing */
  SigmaArrayType m_Sigma;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSmoothingRecursiveGaussianImageFilter.hxx"
#endif

#endif
