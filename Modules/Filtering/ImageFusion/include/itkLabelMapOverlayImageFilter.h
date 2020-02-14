/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkLabelMapOverlayImageFilter_h
#define itkLabelMapOverlayImageFilter_h

#include "itkLabelMapFilter.h"
#include "itkLabelOverlayFunctor.h"
#include "itkRGBPixel.h"

namespace itk
{

/**
 *\class LabelMapOverlayImageFilter
 * \brief Apply a colormap to a label map and superimpose it on an image.
 *
 * Apply a colormap to a label map and put it on top of the feature
 * image. The feature image is typically the image from which the labeling was
 * produced. Use the SetInput function to set the LabelMap, and the
 * SetFeatureImage function to set the feature image.
 *
 * The set of colors is a good selection of distinct colors. The opacity of
 * the label map can be defined by the user.
 * A background label produce a gray pixel with the same intensity
 * than the input one.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelOverlayImageFilter, LabelOverlayFunctor
 * \sa LabelMapToRGBImageFilter, LabelMapToBinaryImageFilter, LabelMapToLabelImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKImageFusion
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageFusion/ColorLabeledRegions,Color Labeled Regions In Image}
 * \endsphinx
 */
template <typename TLabelMap,
          typename TFeatureImage,
          typename TOutputImage = Image<RGBPixel<typename TFeatureImage::PixelType>, TFeatureImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT LabelMapOverlayImageFilter : public LabelMapFilter<TLabelMap, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelMapOverlayImageFilter);

  /** Standard class type aliases. */
  using Self = LabelMapOverlayImageFilter;
  using Superclass = LabelMapFilter<TLabelMap, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using LabelMapType = TLabelMap;
  using LabelMapPointer = typename LabelMapType::Pointer;
  using LabelMapConstPointer = typename LabelMapType::ConstPointer;
  using LabelMapRegionType = typename LabelMapType::RegionType;
  using LabelMapPixelType = typename LabelMapType::PixelType;
  using LabelObjectType = typename LabelMapType::LabelObjectType;
  using LabelType = typename LabelObjectType::LabelType;
  using LengthType = typename LabelObjectType::LengthType;

  using FeatureImageType = TFeatureImage;
  using FeatureImagePointer = typename FeatureImageType::Pointer;
  using FeatureImageConstPointer = typename FeatureImageType::ConstPointer;
  using FeatureImageRegionType = typename FeatureImageType::RegionType;
  using FeatureImagePixelType = typename FeatureImageType::PixelType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using IndexType = typename OutputImageType::IndexType;
  using SizeType = typename OutputImageType::SizeType;
  using RegionType = typename OutputImageType::RegionType;

  using FunctorType =
    typename Functor::LabelOverlayFunctor<FeatureImagePixelType, LabelMapPixelType, OutputImagePixelType>;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TLabelMap::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelMapOverlayImageFilter, ImageToImageFilter);

  /** Set the feature image */
  void
  SetFeatureImage(const TFeatureImage * input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput(1, const_cast<TFeatureImage *>(input));
  }

  /** Get the feature image */
  const FeatureImageType *
  GetFeatureImage()
  {
    return static_cast<FeatureImageType *>((this->ProcessObject::GetInput(1)));
  }

  /** Set the input image */
  void
  SetInput1(const TLabelMap * input)
  {
    this->SetInput(input);
  }

  /** Set the feature image */
  void
  SetInput2(const TFeatureImage * input)
  {
    this->SetFeatureImage(input);
  }

  /** Set/Get the opacity of the colored label image. The value must be
   * between 0 and 1
   */
  itkSetMacro(Opacity, double);
  itkGetConstReferenceMacro(Opacity, double);

  /** Set/Get the overlay functor - defaults to a reasonable set of colors.
   * This can be used to apply a different colormap.
   */
  virtual void
  SetFunctor(const FunctorType & functor)
  {
    if (m_Functor != functor)
    {
      m_Functor = functor;
      this->Modified();
    }
  }
  FunctorType &
  GetFunctor()
  {
    return m_Functor;
  }
  const FunctorType &
  GetFunctor() const
  {
    return m_Functor;
  }

protected:
  LabelMapOverlayImageFilter();
  ~LabelMapOverlayImageFilter() override = default;

  /** LabelMapOverlayImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** LabelMapOverlayImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  void
  GenerateData() override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

  // part of a compile error workaround for GCC 4.8.5-28 (Red Hat) from 20150623
  void
  SuperclassDynamicTGD(const OutputImageRegionType & outputRegion)
  {
    Superclass::DynamicThreadedGenerateData(outputRegion);
  }

  void
  ThreadedProcessLabelObject(LabelObjectType * labelObject) override;

  void
  GenerateOutputInformation() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  double      m_Opacity;
  FunctorType m_Functor;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelMapOverlayImageFilter.hxx"
#endif

#endif
