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
#ifndef itkBSplineScatteredDataPointSetToGradientImageFilter_h
#define itkBSplineScatteredDataPointSetToGradientImageFilter_h

#include "itkCovariantVector.h"
#include "itkPointSet.h"
#include "itkPointSetToImageFilter.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"
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
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineScatteredDataPointSetToGradientImageFilter);

  /** Extract dimension from input image. */
  itkStaticConstMacro(PointSetDimension, unsigned int, TInputPointSet::PointDimension);
  itkStaticConstMacro(ImageDimension, unsigned int, TInputPointSet::PointDimension);

  /** Standard class typedefs. */
  typedef BSplineScatteredDataPointSetToGradientImageFilter Self;

  /** Convenient typedefs for simplifying declarations. */
  typedef TInputPointSet                      InputPointSetType;
  typedef typename InputPointSetType::Pointer InputPointSetPointer;
  typedef Image<CovariantVector<TOutputValueType, itkGetStaticConstMacro(ImageDimension)>,
                itkGetStaticConstMacro(ImageDimension)>
                                            OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;

  /** Standard class typedefs. */
  typedef PointSetToImageFilter<InputPointSetType, OutputImageType> Superclass;
  typedef SmartPointer<Self>                                        Pointer;
  typedef SmartPointer<const Self>                                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineScatteredDataPointSetToGradientImageFilter, PointSetToImageFilter);

  /** Image typedef support. */
  typedef typename InputPointSetType::PixelType InputPixelType;
  itkStaticConstMacro(InputVectorDimension, unsigned int, InputPixelType::Dimension);

  typedef typename InputPointSetType::PointType               InputPointType;
  typedef TOutputValueType                                    OutputValueType;
  typedef CovariantVector<OutputValueType, ImageDimension>    OutputPixelType;
  typedef typename OutputImageType::RegionType                OutputImageRegionType;
  typedef typename itk::Image<InputPixelType, ImageDimension> InternalImageType;

  /** Internal filter type. */
  typedef typename itk::BSplineScatteredDataPointSetToImageFilter<InputPointSetType, InternalImageType>
                                                           BSplineScatteredDataFilterType;
  typedef typename BSplineScatteredDataFilterType::Pointer BSplineScatteredDataFilterPointerType;

  typedef typename BSplineScatteredDataFilterType::ArrayType ArrayType;

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
  itkGetObjectMacro(BSplineScatteredDataFilter, BSplineScatteredDataFilterType);

  /** Type of the outputs. */
  typedef itk::ProcessObject::DataObjectPointerArray DataObjectPointerArray;

protected:
  /** This is not a CovariantVector, but a VariableSizeMatrix where every row
   * corresponds to components of the input data vector, and every column
   * corresponds to the gradient component in each direction. */
  typedef VariableSizeMatrix<float> InternalGradientType;

  BSplineScatteredDataPointSetToGradientImageFilter();
  virtual ~BSplineScatteredDataPointSetToGradientImageFilter() {}

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
