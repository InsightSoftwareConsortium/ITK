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
#ifndef itkShrinkDecimateImageFilter_h
#define itkShrinkDecimateImageFilter_h

#include "itkShrinkImageFilter.h"
#include "itkEnableIf.h"

namespace itk
{

/** \class ShrinkDecimateImageFilter
 * \brief Reduce the size of an image by an integer factor in each
 * dimension just cutting off samples without any interpolation.
 * The first index is always kept.
 *
 * The output image size in each dimension is given by:
 *
 * outputSize[j] = max( std::floor(inputSize[j]/shrinkFactor[j]), 1 );
 *
 * This filter is implemented so that the starting extent of the first
 * pixel of the output matches that of the input.
 *
 * \ingroup ITKImageGrid
 * \ingroup IsotropicWavelets
 */
template <typename TInputImage, typename TOutputImage>
class ShrinkDecimateImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ShrinkDecimateImageFilter                     Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShrinkDecimateImageFilter, ImageToImageFilter);

  /** Typedef to images */
  typedef TOutputImage                          OutputImageType;
  typedef TInputImage                           InputImageType;
  typedef typename OutputImageType::Pointer     OutputImagePointer;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;

  typedef typename TOutputImage::OffsetType OutputOffsetType;
  typedef typename TOutputImage::IndexType  OutputIndexType;
  typedef typename TInputImage::IndexType   InputIndexType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);

  typedef FixedArray<unsigned int, ImageDimension> ShrinkFactorsType;

  /** Set the shrink factors. Values are clamped to
   * a minimum value of 1. Default is 1 for all dimensions. */
  itkSetMacro(ShrinkFactors, ShrinkFactorsType);
  void
  SetShrinkFactors(unsigned int factor);
  void
  SetShrinkFactor(unsigned int i, unsigned int factor);

  /** Get the shrink factors. */
  itkGetConstReferenceMacro(ShrinkFactors, ShrinkFactorsType);

  virtual void
  GenerateOutputInformation() ITK_OVERRIDE;

  /** ShrinkDecimateImageFilter needs a larger input requested region than the output
   * requested region.  As such, ShrinkDecimateImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void
  GenerateInputRequestedRegion() ITK_OVERRIDE;


#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage::PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  /** End concept checking */
#endif

protected:
  ShrinkDecimateImageFilter();
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShrinkDecimateImageFilter);

  ShrinkFactorsType m_ShrinkFactors;

  /** Round different pixel types. */
  template <class TOutputType, class TInputType>
  typename EnableIfC<std::numeric_limits<TOutputType>::is_integer, TOutputType>::Type
  RoundIfInteger(TInputType input)
  {
    return Math::Round<TOutputType>(input);
  }

  // For Non-fundamental types numeric_limits is not specialized, and
  // is_integer defaults to false.
  template <class TOutputType, class TInputType>
  typename DisableIfC<std::numeric_limits<TOutputType>::is_integer, TOutputType>::Type
  RoundIfInteger(const TInputType & input, ...)
  {
    return static_cast<TOutputType>(input);
  }
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShrinkDecimateImageFilter.hxx"
#endif

#endif
