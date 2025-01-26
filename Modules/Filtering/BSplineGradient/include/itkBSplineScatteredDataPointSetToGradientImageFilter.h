/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkBSplineScatteredDataPointSetToGradientImageFilter_h
#define itkBSplineScatteredDataPointSetToGradientImageFilter_h

#include "itkCovariantVector.h"
#include "itkPointSet.h"
#include "itkPointSetToImageFilter.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkBSplineControlPointImageFunction.h"
#include "itkVector.h"
#include "itkVariableSizeMatrix.h"

namespace itk
{
/** \class BSplineScatteredDataPointSetToGradientImageFilter
 *
 * \brief Uses a B-spline approximation to a PointSet to calculate a gradient
 * image.
 *
 * This filter uses the BSplineScatteredDataPointSetToImageFilter to create a
 * B-spline approximation to the point set input, then uses the approximation to
 * calculate a gradient image.
 *
 * The point data on the input PointSet should be
 * \doxgygen{VariableLengthVector}'s, and an
 * \doxygen{CovariantVector} on will be placed on each output corresponding to
 * the components to of the point data vector.
 *
 * \sa BSplineScatteredDataPointSetToImageFilter
 * \sa VariableLengthVector
 * \sa CovariantVector
 * \sa GradientToImageFilter
 *
 * \ingroup BSplineGradient
 */
template <typename TInputPointSet, typename TOutputValueType>
class BSplineScatteredDataPointSetToGradientImageFilter
  : public PointSetToImageFilter<
      TInputPointSet,
      Image<CovariantVector<TOutputValueType, TInputPointSet::PointDimension>, TInputPointSet::PointDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BSplineScatteredDataPointSetToGradientImageFilter);

  /** Extract dimension from input image. */
  itkStaticConstMacro(PointSetDimension, unsigned int, TInputPointSet::PointDimension);
  itkStaticConstMacro(ImageDimension, unsigned int, TInputPointSet::PointDimension);

  /** Standard class type alias. */
  using Self = BSplineScatteredDataPointSetToGradientImageFilter;

  /** Convenient type alias for simplifying declarations. */
  using InputPointSetType = TInputPointSet;
  using InputPointSetPointer = typename InputPointSetType::Pointer;
  using OutputImageType = Image<CovariantVector<TOutputValueType, itkGetStaticConstMacro(ImageDimension)>,
                                itkGetStaticConstMacro(ImageDimension)>;
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Standard class type alias. */
  using Superclass = PointSetToImageFilter<InputPointSetType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(BSplineScatteredDataPointSetToGradientImageFilter);

  /** Image type alias support. */
  using InputPixelType = typename InputPointSetType::PixelType;
  itkStaticConstMacro(InputVectorDimension, unsigned int, InputPixelType::Dimension);

  using InputPointType = typename InputPointSetType::PointType;
  using OutputValueType = TOutputValueType;
  using OutputPixelType = CovariantVector<OutputValueType, ImageDimension>;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using InternalImageType = typename itk::Image<InputPixelType, ImageDimension>;

  /** Internal filter type. */
  using BSplineScatteredDataFilterType =
    typename itk::BSplineScatteredDataPointSetToImageFilter<InputPointSetType, InternalImageType>;
  using BSplineScatteredDataFilterPointerType = typename BSplineScatteredDataFilterType::Pointer;

  using ArrayType = typename BSplineScatteredDataFilterType::ArrayType;

  using BSplineControlPointImageFunctionType = BSplineControlPointImageFunction<InternalImageType>;

  void
  SetNumberOfLevels(unsigned int levels)
  {
    this->m_NumberOfLevels.Fill(levels);
    this->Modified();
  }
  itkSetMacro(NumberOfLevels, ArrayType);
  itkGetConstReferenceMacro(NumberOfLevels, ArrayType);

  void
  SetSplineOrder(unsigned int order)
  {
    this->m_SplineOrder.Fill(order);
    this->Modified();
  }
  itkSetMacro(SplineOrder, ArrayType);
  itkGetConstReferenceMacro(SplineOrder, ArrayType);

  itkSetMacro(NumberOfControlPoints, ArrayType);
  itkGetConstReferenceMacro(NumberOfControlPoints, ArrayType);

  /** Set/Get the internal BSplineScatteredDataPointSetToImageFilter that is
   * used to the create the parametric B-spline representation.  Most of the
   * settings for this filter involve setting the options on this object. */
  itkSetObjectMacro(BSplineScatteredDataFilter, BSplineScatteredDataFilterType);
  itkGetModifiableObjectMacro(BSplineScatteredDataFilter, BSplineScatteredDataFilterType);


  /** Type of the outputs. */
  using DataObjectPointerArray = ProcessObject::DataObjectPointerArray;

protected:
  /** This is not a CovariantVector, but a VariableSizeMatrix where every row
   * corresponds to components of the input data vector, and every column
   * corresponds to the gradient component in each direction. */
  using InternalGradientType = typename BSplineControlPointImageFunctionType::GradientType;

  BSplineScatteredDataPointSetToGradientImageFilter();
  ~BSplineScatteredDataPointSetToGradientImageFilter() override = default;

  void
  GenerateOutputInformation() override;

  void
  BeforeThreadedGenerateData() override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegion) override;

private:
  BSplineScatteredDataFilterPointerType m_BSplineScatteredDataFilter;

  ArrayType m_NumberOfControlPoints;
  ArrayType m_NumberOfLevels;
  ArrayType m_SplineOrder;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineScatteredDataPointSetToGradientImageFilter.hxx"
#endif

#endif
