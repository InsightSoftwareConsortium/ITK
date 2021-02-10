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
#ifndef itkLabelMapContourOverlayImageFilter_h
#define itkLabelMapContourOverlayImageFilter_h

#include "itkLabelMapFilter.h"
#include "itkLabelOverlayFunctor.h"
#include "itkRGBPixel.h"

namespace itk
{

/**
 *\class LabelMapContourOverlayImageFilter
 * \brief Apply a colormap to the contours (outlines) of each object in a label map
 *        and superimpose it on top of the feature image.
 *
 * The feature image is typically the image from which the labeling was
 * produced. Use the SetInput function to set the LabelMap, and the
 * SetFeatureImage function to set the feature image.
 *
 * Apply a colormap to a label map and put it on top of the input image.
 * The set of colors is a good selection of distinct colors. The opacity of
 * the label map can be defined by the user.
 * A background label produce a gray pixel with the same intensity
 * than the input one.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \sa  LabelMapOverlayImageFilter, LabelOverlayImageFilter, LabelOverlayFunctor
 * \sa LabelMapToBinaryImageFilter, LabelMapToLabelImageFilter,
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKImageFusion
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageFusion/ColorBoundariesOfRegions,Color Boundaries Of Labeled Regions}
 * \endsphinx
 */
template <typename TLabelMap,
          typename TFeatureImage,
          typename TOutputImage = Image<RGBPixel<typename TFeatureImage::PixelType>, TFeatureImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT LabelMapContourOverlayImageFilter : public LabelMapFilter<TLabelMap, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelMapContourOverlayImageFilter);

  /** Standard class type aliases. */
  using Self = LabelMapContourOverlayImageFilter;
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
  static constexpr unsigned int LabelMapDimension = TLabelMap::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  enum
  {
    PLAIN = 0,
    CONTOUR = 1,
    SLICE_CONTOUR = 2
  };

  enum
  {
    HIGH_LABEL_ON_TOP = 0,
    LOW_LABEL_ON_TOP = 1
  };

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelMapContourOverlayImageFilter, ImageToImageFilter);

  /** Set the feature image */
  void
  SetFeatureImage(TFeatureImage * input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput(1, const_cast<TFeatureImage *>(input));
  }

  /** Get the feature image */
  FeatureImageType *
  GetFeatureImage()
  {
    return itkDynamicCastInDebugMode<FeatureImageType *>(const_cast<DataObject *>(this->ProcessObject::GetInput(1)));
  }

  /** Set the input image */
  void
  SetInput1(TLabelMap * input)
  {
    this->SetInput(input);
  }

  /** Set the feature image */
  void
  SetInput2(TFeatureImage * input)
  {
    this->SetFeatureImage(input);
  }

  /** Set/Get the opacity of the colored label image. The value must be
   * between 0 and 1
   */
  itkSetMacro(Opacity, double);
  itkGetConstReferenceMacro(Opacity, double);

  /** Set/Get the overlay type - CONTOUR is used by default.
   */
  itkSetMacro(Type, int);
  itkGetConstReferenceMacro(Type, int);

  /** Set/Get the object priority - HIGH_LABEL_ON_TOP by default.
   */
  itkSetMacro(Priority, int);
  itkGetConstReferenceMacro(Priority, int);

  /** Set/Get the object dilation radius - 0 by default.
   */
  itkSetMacro(DilationRadius, SizeType);
  itkGetConstReferenceMacro(DilationRadius, SizeType);

  /** Set/Get the contour thickness - 1 by default.
   */
  itkSetMacro(ContourThickness, SizeType);
  itkGetConstReferenceMacro(ContourThickness, SizeType);

  /** Set/Get the slice dimension - defaults to image dimension - 1.
   */
  itkSetMacro(SliceDimension, int);
  itkGetConstReferenceMacro(SliceDimension, int);

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
  LabelMapContourOverlayImageFilter();
  ~LabelMapContourOverlayImageFilter() override = default;

  /** LabelMapContourOverlayImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** LabelMapContourOverlayImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  void
  BeforeThreadedGenerateData() override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

  // part of a compile error workaround for GCC 4.8.5-28 (Red Hat) from 20150623
  void
  SuperclassDynamicTGD(const OutputImageRegionType & outputRegion)
  {
    Superclass::DynamicThreadedGenerateData(outputRegion);
  }

  void
  GenerateData() override;

  void
  ThreadedProcessLabelObject(LabelObjectType * labelObject) override;

  void
  GenerateOutputInformation() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  LabelMapType *
  GetLabelMap() override
  {
    return m_TempImage;
  }

private:
  double      m_Opacity;
  int         m_Type;
  int         m_Priority;
  SizeType    m_ContourThickness;
  SizeType    m_DilationRadius;
  int         m_SliceDimension;
  FunctorType m_Functor;

  LabelMapPointer m_TempImage;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelMapContourOverlayImageFilter.hxx"
#endif

#endif
