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
#ifndef itkHoughTransform2DLinesImageFilter_h
#define itkHoughTransform2DLinesImageFilter_h


#include "itkImageToImageFilter.h"
#include "itkLineSpatialObject.h"

namespace itk
{
/**
 * \class HoughTransform2DLinesImageFilter
 * \brief Performs the Hough Transform to find 2D straight lines
 *        in a 2D image.
 *
 * This filter derives from ImageToImageFilter.
 * The input is an image, and all pixels above some threshold are those
 * to be extracted. The output is the image of the accumulator.
 * GetLines() returns a list of LinesSpatialObjects.
 *
 * Lines are parameterized in the form:
 *
 * \f$ R = x \cos(\theta) + y \sin(\theta) \f$
 * where \f$R\f$ is the perpendicular distance from the origin and
 * \f$\theta\f$ the angle with the normal.
 *
 * The output is the accumulator array:
 *  - The first dimension (X) represents the distance R from the corner
 *    to the line.
 *  - The second dimension (Y) represents the angle between the X axis
 *    and the normal to the line.
 *
 * The size of the array depends on the AngleAxisSize that could be set
 * (500 by default) for the angle axis. The distance axis depends on the
 * size of the diagonal of the input image.
 *
 * \ingroup ImageFeatureExtraction
 * \sa LineSpatialObject
 *
 * \ingroup ITKImageFeature
 *
 *
 */

template <typename TInputPixelType, typename TOutputPixelType>
class ITK_TEMPLATE_EXPORT HoughTransform2DLinesImageFilter
  : public ImageToImageFilter<Image<TInputPixelType, 2>, Image<TOutputPixelType, 2>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HoughTransform2DLinesImageFilter);

  /** Standard "Self" type alias. */
  using Self = HoughTransform2DLinesImageFilter;

  /** Input Image type alias */
  using InputImageType = Image<TInputPixelType, 2>;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;

  /** Output Image type alias */
  using OutputImageType = Image<TOutputPixelType, 2>;
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Smart pointer type alias support */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Line type alias */
  using LineType = LineSpatialObject<2>;
  using LinePointer = typename LineType::Pointer;
  using LinesListType = std::list<LinePointer>;
  using LinePointType = LineType::LinePointType;

  using LinesListSizeType = typename LinesListType::size_type;

  /** Standard "Superclass" type alias. */
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;

  /** Image index type alias */
  using IndexType = typename InputImageType::IndexType;

  /** Image pixel value type alias */
  using PixelType = typename InputImageType::PixelType;

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename InputImageType::RegionType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(HoughTransform2DLinesImageFilter, ImageToImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for evaluating the implicit function over the image. */
  void
  GenerateData() override;

  /** Set/Get the threshold above which the filter should consider
   * the point as a valid point. */
  itkSetMacro(Threshold, double);
  itkGetConstMacro(Threshold, double);

  /** Set/Get the resolution angle.
   * The Hough space describes (in the angle direction) [-PI,PI[
   * with a constant step AngleResolution. */
  itkSetMacro(AngleResolution, double);
  itkGetConstMacro(AngleResolution, double);

  /** Simplify the accumulator.
   * Performs the same iteration process as the Update() method, but finds
   * the maximum along the curve and then removes the curve. */
  void
  Simplify();

  /** Get the Simplified accumulator. */
  itkGetModifiableObjectMacro(SimplifyAccumulator, OutputImageType);

  /** Get the list of lines. This recomputes the lines. */
  LinesListType &
  GetLines();

  /** Set/Get the number of lines to extract */
  itkSetMacro(NumberOfLines, LinesListSizeType);
  itkGetConstMacro(NumberOfLines, LinesListSizeType);

  /** Set/Get the radius of the disc to remove from the accumulator
   * for each line found. */
  itkSetMacro(DiscRadius, double);
  itkGetConstMacro(DiscRadius, double);

  /** Set/Get the variance of the Gaussian blurring for the accumulator. */
  itkSetMacro(Variance, double);
  itkGetConstMacro(Variance, double);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(IntConvertibleToOutputCheck, (Concept::Convertible<int, TOutputPixelType>));
  itkConceptMacro(InputGreaterThanFloatCheck, (Concept::GreaterThanComparable<PixelType, float>));
  itkConceptMacro(OutputPlusIntCheck, (Concept::AdditiveOperators<TOutputPixelType, int>));
  // End concept checking
#endif

protected:
  HoughTransform2DLinesImageFilter();
  ~HoughTransform2DLinesImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** HoughTransform2DLinesImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** HoughTransform2DLinesImageFilter's output is the accumulator
   * array.  The size of the output is a function of the size of the
   * input and the AngleAxisSize. Since this output is a different
   * size than the input, it must provide an implementation of
   * GenerateOutputInformation.
   * \sa ProcessObject::GenerateOutputRequestedRegion() */
  void
  GenerateOutputInformation() override;

  /** HoughTransform2DLinesImageFilter must produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

private:
  double m_AngleResolution{ 500 };
  double m_Threshold{ 0 };

  OutputImagePointer m_SimplifyAccumulator;
  LinesListType      m_LinesList;
  LinesListSizeType  m_NumberOfLines{ 1 };
  double             m_DiscRadius{ 10 };
  double             m_Variance{ 5 };
  ModifiedTimeType   m_OldModifiedTime{ 0 };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHoughTransform2DLinesImageFilter.hxx"
#endif

#endif
