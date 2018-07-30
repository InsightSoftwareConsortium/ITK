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
#ifndef itkBSplineApproximationGradientImageFilter_h
#define itkBSplineApproximationGradientImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkCovariantVector.h"
#include "itkMesh.h"
#include "itkPointSet.h"

#include "itkBSplineScatteredDataPointSetToGradientImageFilter.h"
#include "itkImageToPointSetFilter.h"

namespace itk
{

/** \class BSplineApproximationGradientImageFilter
 *
 * \brief Uses a B-spline approximation to an Image to calculate a gradient
 * image.
 *
 * This is an ImageToImageFilter whose input image pixels should be of the type
 * itk::Vector and whose outputs will be of the type itk::CovariantVector.
 *
 * \sa BSplineScatteredDataPointSetToGradientImageFilter
 * \sa Vector
 * \sa CovariantVector
 *
 * \ingroup BSplineGradient
 */
template <typename TInputImage, typename TOutputValueType>
class ITK_TEMPLATE_EXPORT BSplineApproximationGradientImageFilter
  : public ImageToImageFilter<
      TInputImage,
      Image<CovariantVector<TOutputValueType, TInputImage::ImageDimension>, TInputImage::ImageDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineApproximationGradientImageFilter);

  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  typedef BSplineApproximationGradientImageFilter Self;

  typedef TInputImage                                                              InputImageType;
  typedef Image<CovariantVector<TOutputValueType, ImageDimension>, ImageDimension> OutputImageType;
  typedef typename OutputImageType::Pointer                                        OutputImagePointer;

  typedef ImageToImageFilter<InputImageType, OutputImageType> Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;

  itkNewMacro(Self);

  itkTypeMacro(BSplineApproximationGradientImageFilter, ImageToImageFilter);

  typedef TOutputValueType                                                 OutputValueType;
  typedef typename InputImageType::PixelType                               InputPixelType;
  typedef PointSet<InputPixelType, itkGetStaticConstMacro(ImageDimension)> PointSetType;

  itkStaticConstMacro(InputVectorDimension, unsigned int, InputPixelType::Dimension);

  /** Internal filter type */
  /** Internal filter type */
  typedef BSplineScatteredDataPointSetToGradientImageFilter<PointSetType, OutputValueType> PointSetToGradientFilterType;
  typedef typename PointSetToGradientFilterType::Pointer   PointSetToGradientFilterPointerType;
  typedef typename PointSetToGradientFilterType::ArrayType ArrayType;
  typedef Mesh<InputPixelType, ImageDimension>             MeshType;
  typedef ImageToPointSetFilter<InputImageType, MeshType>  ImageToPointSetFilterType;
  typedef typename ImageToPointSetFilterType::Pointer      ImageToPointSetFilterPointerType;

  typedef FixedArray<double, itkGetStaticConstMacro(ImageDimension)> ControlPointSpacingRatioType;

  /** Set/Get number of levels.  This is the number of levels used in the
   * BSplineScatteredDataPointSetToImageFilter for calculating the BSpline grid.
   * */
  void
  SetNumberOfLevels(const unsigned int levels)
  {
    this->m_NumberOfLevels.Fill(levels);
    this->Modified();
  }
  itkSetMacro(NumberOfLevels, ArrayType);
  itkGetConstReferenceMacro(NumberOfLevels, ArrayType);

  /** Set/Get the order of the BSpline in each direction. */
  void
  SetSplineOrder(const unsigned int order)
  {
    this->m_SplineOrder.Fill(order);
    this->Modified();
  }
  itkSetMacro(SplineOrder, ArrayType);
  itkGetConstReferenceMacro(SplineOrder, ArrayType);

  /** Set/Get the ratio of the approximating B-spline control point spacing to
   * to the input image spacing in each direction.  Note that this will result
   * in specifying the number of top level controls points in the
   * approximating B-spline filter according to the following expression
   *
   * c_size = floor( i_size / ( r * 2^(l-1) ) )
   *
   * c_size = number of control points in the top level
   * i_size = size of the image
   * r      = specified ratio
   * l      = number of levels
   *
   */
  void
  SetControlPointSpacingRatio(const double & ratio)
  {
    this->m_ControlPointSpacingRatio.Fill(ratio);
    this->Modified();
  }
  itkSetMacro(ControlPointSpacingRatio, ControlPointSpacingRatioType);
  itkGetConstReferenceMacro(ControlPointSpacingRatio, ControlPointSpacingRatioType);

protected:
  BSplineApproximationGradientImageFilter();
  virtual ~BSplineApproximationGradientImageFilter() {}

  /** Needs everything. */
  void
  GenerateInputRequestedRegion() override;

  /** This filter produces the LargestPossibleRegion. */
  void
  EnlargeOutputRequestedRegion(DataObject * data) override;

  void
  GenerateData() override;

private:
  ImageToPointSetFilterPointerType    m_ImageToPointSetFilter;
  PointSetToGradientFilterPointerType m_PointSetToGradientFilter;

  ArrayType m_NumberOfLevels;
  ArrayType m_SplineOrder;

  ControlPointSpacingRatioType m_ControlPointSpacingRatio;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineApproximationGradientImageFilter.hxx"
#endif

#endif
