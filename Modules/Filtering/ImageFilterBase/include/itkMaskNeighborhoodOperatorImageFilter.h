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
#ifndef itkMaskNeighborhoodOperatorImageFilter_h
#define itkMaskNeighborhoodOperatorImageFilter_h

#include "itkNeighborhoodOperatorImageFilter.h"

namespace itk
{
/** \class MaskNeighborhoodOperatorImageFilter
 * \brief Applies a single NeighborhoodOperator to an image,
 * processing only those pixels that are under a mask.
 *
 * This filter calculates successive inner products between a single
 * NeighborhoodOperator and a NeighborhoodIterator, which is swept
 * across every pixel that is set in the input mask. If no mask is
 * given, this filter is equivalent to its superclass. Output pixels
 * that are outside of the mask will be set to DefaultValue if
 * UseDefaultValue is true (default). Otherwise, they will be set to
 * the value of the input pixel.
 *
 * \ingroup ImageFilters
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodOperatorImageFilter
 * \sa NeighborhoodIterator
 * \ingroup ITKImageFilterBase
 *
 * \wiki
 * \wikiexample{Images/MaskNeighborhoodOperatorImageFilter,Apply a kernel to every pixel in an image that is non-zero in a mask}
 * \endwiki
 */
template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TOperatorValueType =
            typename TOutputImage::PixelType >
class ITK_TEMPLATE_EXPORT MaskNeighborhoodOperatorImageFilter:
  public NeighborhoodOperatorImageFilter< TInputImage, TOutputImage, TOperatorValueType >
{
public:
  /** Standard "Self" & Superclass typedef. */
  typedef MaskNeighborhoodOperatorImageFilter Self;
  typedef NeighborhoodOperatorImageFilter<
    TInputImage, TOutputImage, TOperatorValueType > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MaskNeighborhoodOperatorImageFilter, NeighborhoodOperatorImageFilter);

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  typedef typename TOutputImage::PixelType         OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename  TInputImage::PixelType         InputPixelType;
  typedef typename  TInputImage::InternalPixelType InputInternalPixelType;
  typedef typename   TMaskImage::PixelType         MaskPixelType;
  typedef typename   TMaskImage::InternalPixelType MaskInternalPixelType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(MaskImageDimension, unsigned int,
                      TMaskImage::ImageDimension);

  /** Image typedef support. */
  typedef TInputImage                      InputImageType;
  typedef TMaskImage                       MaskImageType;
  typedef TOutputImage                     OutputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename MaskImageType::Pointer  MaskImagePointer;

  /** Typedef for generic boundary condition pointer. */
  typedef ImageBoundaryCondition< OutputImageType > *
  ImageBoundaryConditionPointerType;

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename Superclass::OperatorValueType     OperatorValueType;

  /** Neighborhood types */
  typedef typename Superclass::OutputNeighborhoodType OutputNeighborhoodType;

  /** Set the mask image. Using a mask is optional.  When a mask is
   * specified, the normalized correlation is only calculated for
   * those pixels under the mask. */
  void SetMaskImage(const TMaskImage *mask);

  /** Get the mask image. Using a mask is optional.  When a mask is
   * specified, the normalized correlation is only calculated for
   * those pixels under the mask. */
  const TMaskImage * GetMaskImage() const;

  /** Set the output value for the pixels that are not under the mask.
   * Defaults to zero.
   */
  itkSetMacro(DefaultValue, OutputPixelType);

  /** Get the output value for the pixels that are not under the
   * mask. */
  itkGetConstMacro(DefaultValue, OutputPixelType);

  /** Set the UseDefaultValue flag. If true, the pixels outside the
   *  mask will e set to m_DefaultValue. Otherwise, they will be set
   *  to the input pixel. */
  itkSetMacro(UseDefaultValue, bool);

  /** Get the UseDefaultValue flag. */
  itkGetConstReferenceMacro(UseDefaultValue, bool);

  /** Turn on and off the UseDefaultValue flag. */
  itkBooleanMacro(UseDefaultValue);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputEqualityComparableCheck,
                   ( Concept::EqualityComparable< OutputPixelType > ) );
  itkConceptMacro( SameDimensionCheck1,
                   ( Concept::SameDimension< InputImageDimension, ImageDimension > ) );
  itkConceptMacro( SameDimensionCheck2,
                   ( Concept::SameDimension< InputImageDimension, MaskImageDimension > ) );
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< InputPixelType, OutputPixelType > ) );
  itkConceptMacro( OperatorConvertibleToOutputCheck,
                   ( Concept::Convertible< OperatorValueType, OutputPixelType > ) );
  itkConceptMacro( OutputOStreamWritable,
                   ( Concept::OStreamWritable< OutputPixelType > ) );
  // End concept checking
#endif

protected:
  MaskNeighborhoodOperatorImageFilter():m_DefaultValue(NumericTraits< OutputPixelType >::ZeroValue()),
    m_UseDefaultValue(true) {}
  virtual ~MaskNeighborhoodOperatorImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** MaskNeighborhoodOperatorImageFilter needs to request enough of an
   * input image to account for template size.  The input requested
   * region is expanded by the radius of the template.  If the request
   * extends past the LargestPossibleRegion for the input, the request
   * is cropped by the LargestPossibleRegion. */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** MaskNeighborhoodOperatorImageFilter can be implemented as a
   * multithreaded filter.  Therefore, this implementation provides a
   * ThreadedGenerateData() routine which is called for each
   * processing thread. The output image data is allocated
   * automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to
   * the portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MaskNeighborhoodOperatorImageFilter);

  OutputPixelType m_DefaultValue;
  bool            m_UseDefaultValue;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMaskNeighborhoodOperatorImageFilter.hxx"
#endif

#endif
