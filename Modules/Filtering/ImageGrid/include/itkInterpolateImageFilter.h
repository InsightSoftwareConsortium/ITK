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
#ifndef itkInterpolateImageFilter_h
#define itkInterpolateImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"

namespace itk
{
/** \class InterpolateImageFilter
 * \brief Interpolate an image from two N-D images.
 *
 * Interpolates an image from two input images of the same type
 * and same dimension (N). In particular, this filter forms an intermediate
 * (N+1)D image by concatenating the two input images and interpolating
 * an image a distance \f$ d \in [0,1] \f$ away from the first image.
 *
 * The interpolation is delegated to a user specified InterpolateImageFunction.
 * By default, linear interpolation is used.
 *
 * The filter is templated over the input image type and output image type.
 * It assumes that the input and output have the same number of dimensions.
 *
 * \ingroup MultiThreaded
 * \ingroup ITKImageGrid
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT InterpolateImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef InterpolateImageFilter                          Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(InterpolateImageFilter, ImageToImageFilter);

  /** Inherit typedefs from Superclass */
  typedef typename Superclass::InputImageType        InputImageType;
  typedef typename Superclass::InputImagePointer     InputImagePointer;
  typedef typename Superclass::OutputImageType       OutputImageType;
  typedef typename Superclass::OutputImagePointer    OutputImagePointer;
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Number of dimensions. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(IntermediateImageDimension, unsigned int,
                      TOutputImage::ImageDimension + 1);

  /** Interpolator typedef. */
  typedef typename TInputImage::PixelType                                             InputPixelType;
  typedef Image< InputPixelType, itkGetStaticConstMacro(IntermediateImageDimension) > IntermediateImageType;
  typedef InterpolateImageFunction< IntermediateImageType >                           InterpolatorType;

  /** Set/Get the first image */
  void SetInput1(const InputImageType *image)
  { this->SetInput(image); }
  const InputImageType * GetInput1()
  { return this->GetInput(); }

  /** Set/Get the second image */
  void SetInput2(const InputImageType *image);

  const InputImageType * GetInput2();

  /** Set/Get the distance from the first image from which to generate
   * interpolated image. The default value is 0.5 */
  itkSetClampMacro(Distance, double, 0.0, 1.0);
  itkGetConstMacro(Distance, double);

  /** Get/Set the interpolator function */
  itkSetObjectMacro(Interpolator, InterpolatorType)
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** This method is used to set the state of the filter before
   * multi-threading. */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** This method is used to run after multi-threading. */
  void AfterThreadedGenerateData() ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  // End concept checking
#endif

protected:
  InterpolateImageFilter();
  ~InterpolateImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** InterpolateImageFilter can be implemented as a multithreaded filter. */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(InterpolateImageFilter);

  typename InterpolatorType::Pointer m_Interpolator;

  typename IntermediateImageType::Pointer m_IntermediateImage;

  double m_Distance;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkInterpolateImageFilter.hxx"
#endif

#endif
