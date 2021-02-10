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
#ifndef itkLabelImageToShapeLabelMapFilter_h
#define itkLabelImageToShapeLabelMapFilter_h

#include "itkShapeLabelObject.h"
#include "itkLabelImageToLabelMapFilter.h"
#include "itkShapeLabelMapFilter.h"

namespace itk
{
/**
 *\class LabelImageToShapeLabelMapFilter
 * \brief Converts a label image to a label map and valuates the shape attributes
 *
 *  A convenient class that converts a label image to a label map and valuates the shape attribute at once.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ShapeLabelObject, LabelShapeOpeningImageFilter, LabelStatisticsOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 *
 * \sphinx
 * \sphinxexample{Filtering/LabelMap/ShapeAttributesForBinaryImage,Shape Attributes For Binary Image}
 * \sphinxexample{Filtering/LabelMap/ConvertImageWithLabelsToShapeLabelMap,Convert Image With Labeled Regions To
 * ShapeLabelMap} \endsphinx
 */
template <typename TInputImage,
          typename TOutputImage =
            LabelMap<ShapeLabelObject<typename TInputImage::PixelType, TInputImage::ImageDimension>>>
class ITK_TEMPLATE_EXPORT LabelImageToShapeLabelMapFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelImageToShapeLabelMapFilter);

  /** Standard class type aliases. */
  using Self = LabelImageToShapeLabelMapFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using LabelObjectType = typename OutputImageType::LabelObjectType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using LabelizerType = LabelImageToLabelMapFilter<InputImageType, OutputImageType>;
  using ShapeLabelFilterOutput = Image<typename OutputImageType::PixelType, Self::OutputImageDimension>;
  using LabelObjectValuatorType = ShapeLabelMapFilter<TOutputImage, ShapeLabelFilterOutput>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelImageToShapeLabelMapFilter, ImageToImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputEqualityComparableCheck, (Concept::EqualityComparable<InputImagePixelType>));
  itkConceptMacro(IntConvertibleToInputCheck, (Concept::Convertible<int, InputImagePixelType>));
  itkConceptMacro(InputOStreamWritableCheck, (Concept::OStreamWritable<InputImagePixelType>));
  // End concept checking
#endif

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<PixelType>::NonpositiveMin().
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

  /**
   * Set/Get whether the maximum Feret diameter should be computed or not.
   * Default value is false, because of the high computation time required.
   */
  itkSetMacro(ComputeFeretDiameter, bool);
  itkGetConstReferenceMacro(ComputeFeretDiameter, bool);
  itkBooleanMacro(ComputeFeretDiameter);

  /**
   * Set/Get whether the perimeter should be computed or not.
   * Default value is false, because of the high computation time required.
   */
  itkSetMacro(ComputePerimeter, bool);
  itkGetConstReferenceMacro(ComputePerimeter, bool);
  itkBooleanMacro(ComputePerimeter);

  /**
   * Set/Get whether the oriented bounding box should be
   * computed or not. Default value is false because of potential
   * memory consumption issues with sparse labels.
   */
  itkSetMacro(ComputeOrientedBoundingBox, bool);
  itkGetConstReferenceMacro(ComputeOrientedBoundingBox, bool);
  itkBooleanMacro(ComputeOrientedBoundingBox);


protected:
  LabelImageToShapeLabelMapFilter();
  ~LabelImageToShapeLabelMapFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** LabelImageToShapeLabelMapFilter needs the entire input be available.
   * Thus, it needs to provide an implementation of GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** LabelImageToShapeLabelMapFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  /** Single-threaded version of GenerateData.
   * This filter delegates to GrayscaleGeodesicErodeImageFilter. */
  void
  GenerateData() override;

private:
  OutputImagePixelType m_BackgroundValue;
  bool                 m_ComputeFeretDiameter;
  bool                 m_ComputePerimeter;
  bool                 m_ComputeOrientedBoundingBox;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelImageToShapeLabelMapFilter.hxx"
#endif

#endif
