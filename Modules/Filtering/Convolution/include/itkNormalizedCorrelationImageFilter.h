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
#ifndef itkNormalizedCorrelationImageFilter_h
#define itkNormalizedCorrelationImageFilter_h

#include "itkNeighborhoodOperatorImageFilter.h"

namespace itk
{
/** \class NormalizedCorrelationImageFilter
 * \brief Computes the normalized correlation of an image and a template.
 *
 * This filter calculates the normalized correlation between an image
 * and the template.  Normalized correlation is frequently use in
 * feature detection because it is invariant to local changes in
 * contrast.
 *
 * The filter can be given a mask. When presented with an input image
 * and a mask, the normalized correlation is only calculated at those
 * pixels under the mask.
 *
 * \ingroup ImageFilters
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \ingroup ITKConvolution
 *
 * \wiki
 * \wikiexample{Images/NormalizedCorrelationImageFilter,Normalized correlation}
 * \endwiki
 */
template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TOperatorValueType =
            typename TOutputImage::PixelType >
class ITK_TEMPLATE_EXPORT NormalizedCorrelationImageFilter:
  public NeighborhoodOperatorImageFilter< TInputImage, TOutputImage, TOperatorValueType >
{
public:
  /** Standard "Self" & Superclass typedef. */
  typedef NormalizedCorrelationImageFilter Self;
  typedef NeighborhoodOperatorImageFilter<
    TInputImage, TOutputImage, TOperatorValueType > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NormalizedCorrelationImageFilter, NeighborhoodOperatorImageFilter);

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
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(MaskImageDimension, unsigned int,
                      TMaskImage::ImageDimension);

  /** Image typedef support. */
  typedef TInputImage                      InputImageType;
  typedef TMaskImage                       MaskImageType;
  typedef TOutputImage                     OutputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename MaskImageType::Pointer  MaskImagePointer;

  /** Typedef for generic boundary condition pointer. */
  typedef ImageBoundaryCondition< OutputImageType > *ImageBoundaryConditionPointerType;

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

  /** Set the template used in the calculation of the normalized
   * correlation. The elements of the template must be set prior to
   * calling SetTemplate(). */
  void SetTemplate(const OutputNeighborhoodType & t)
  {
    this->SetOperator(t);
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< InputImageDimension, MaskImageDimension > ) );
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< OutputPixelType > ) );
  itkConceptMacro( OperatorHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< OperatorValueType > ) );
  // This filter can only operate on data types that are signed.
  itkConceptMacro( SignedOutputPixelType,
                   ( Concept::Signed< OutputPixelType > ) );
  // End concept checking
#endif

protected:
  NormalizedCorrelationImageFilter() {}
  virtual ~NormalizedCorrelationImageFilter() ITK_OVERRIDE {}

  /** NormalizedCorrelationImageFilter needs to request enough of an
   * input image to account for template size.  The input requested
   * region is expanded by the radius of the template.  If the request
   * extends past the LargestPossibleRegion for the input, the request
   * is cropped by the LargestPossibleRegion. */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** NormalizedCorrelationImageFilter can be implemented as a
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

  /** Standard PrintSelf method */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {  Superclass::PrintSelf(os, indent); }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NormalizedCorrelationImageFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNormalizedCorrelationImageFilter.hxx"
#endif

#endif
