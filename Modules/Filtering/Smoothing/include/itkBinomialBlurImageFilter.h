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
#ifndef itkBinomialBlurImageFilter_h
#define itkBinomialBlurImageFilter_h

#include "itkImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkSize.h"

namespace itk
{
/** \class BinomialBlurImageFilter
 * \brief Performs a separable blur on each dimension of an image.
 *
 * The binomial blur consists of a nearest neighbor average along each
 * image dimension. The net result after n-iterations approaches
 * convultion with a gaussian.
 *
 * \ingroup ImageEnhancement
 * \ingroup ITKSmoothing
 *
 * \wiki
 * \wikiexample{Smoothing/BinomialBlurImageFilter,Blur an image}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT BinomialBlurImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BinomialBlurImageFilter                         Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinomialBlurImageFilter, ImageToImageFilter);

  /** Number of dimensions */
  itkStaticConstMacro(NDimensions, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(NOutputDimensions, unsigned int, TOutputImage::ImageDimension);

  /** Typedef for images */
  typedef TInputImage                           InputImageType;
  typedef TOutputImage                          OutputImageType;
  typedef typename OutputImageType::Pointer     OutputImagePointer;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;

  /** Image size typedef */
  typedef Size< itkGetStaticConstMacro(NDimensions) > SizeType;

  /** Image index typedef */
  typedef typename TOutputImage::IndexType IndexType;

  /** Image pixel value typedef */
  typedef typename TOutputImage::PixelType PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Get and set the number of times to repeat the filter. */
  itkSetMacro(Repetitions, unsigned int);
  itkGetConstMacro(Repetitions, unsigned int);

  /** This filter needs to request a larger input than its requested output.
   * If this filter runs "Repetitions" iterations, then it needs an input
   * that is 2*Repetitions larger than the output. In other words, this
   * filter needs a border of "Repetitions" pixels. */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< itkGetStaticConstMacro(NDimensions),
                                             itkGetStaticConstMacro(NOutputDimensions) > ) );
  itkConceptMacro( InputConvertibleToDoubleCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType, double > ) );
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, PixelType > ) );
  // End concept checking
#endif

protected:
  BinomialBlurImageFilter();
  virtual ~BinomialBlurImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Method for evaluating the implicit function over the image. */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinomialBlurImageFilter);

  /** How many times should we apply the blur? */
  unsigned int m_Repetitions;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinomialBlurImageFilter.hxx"
#endif

#endif
