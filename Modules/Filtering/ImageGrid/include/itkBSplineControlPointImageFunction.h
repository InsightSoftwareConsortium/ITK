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
#ifndef itkBSplineControlPointImageFunction_h
#define itkBSplineControlPointImageFunction_h

#include "itkImageFunction.h"

#include "itkBSplineKernelFunction.h"
#include "itkCoxDeBoorBSplineKernelFunction.h"
#include "itkFixedArray.h"
#include "itkImage.h"
#include "itkPointSet.h"
#include "itkVariableSizeMatrix.h"
#include "itkVector.h"
#include "itkVectorContainer.h"

namespace itk
{
/**
 * \class BSplineControlPointImageFunction
 *
 * \brief Evaluate a B-spline object given a grid of control points.
 *
 * \par  The output of the class itkBSplineScatteredDataPointSetToImageFilter
 * is a control point grid defining a B-spline object.  This class is used to
 * hold various routines meant to operate on that control point grid.  In
 * addition to specifying the control point grid as the input, the user
 * must also supply the spline order and the parametric domain (i.e. size,
 * domain, origin, spacing).
 *
 * Operations include
 *   1. Evaluation of the B-spline object at any point in the domain.
 *   2. Evaluation of the gradient of the B-spline object at any point in the
 *      domain.
 *   3. Evaluation of the Hessian of the B-spline object at any point in the
 *      domain.
 *
 * \author Nicholas J. Tustison
 *
 * \ingroup ITKImageGrid
 */
template <typename TInputImage, typename TCoordRep = double>
class ITK_TEMPLATE_EXPORT BSplineControlPointImageFunction
  : public ImageFunction<TInputImage, typename TInputImage::PixelType, TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BSplineControlPointImageFunction);

  using Self = BSplineControlPointImageFunction;
  using Superclass = ImageFunction<TInputImage, typename TInputImage::PixelType, TCoordRep>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineControlPointImageFunction, ImageFunction);

  /** Extract dimension from input image. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Image type alias support */
  using ControlPointLatticeType = TInputImage;
  using InputImageType = TInputImage;
  using CoordRepType = TCoordRep;
  using PixelType = typename InputImageType::PixelType;
  using RegionType = typename InputImageType::RegionType;
  using IndexType = typename InputImageType::IndexType;
  using PointType = typename Superclass::PointType;
  using InputImageRegionType = typename InputImageType::RegionType;

  using SpacingType = typename InputImageType::SpacingType;
  using OriginType = typename InputImageType::PointType;
  using SizeType = typename InputImageType::SizeType;

  /** Output type alias support */
  using OutputType = PixelType;
  using GradientType = VariableSizeMatrix<CoordRepType>;
  using HessianComponentType = VariableSizeMatrix<CoordRepType>;

  /** Other type alias */
  using ArrayType = FixedArray<unsigned, ImageDimension>;
  using RealImageType = Image<CoordRepType, ImageDimension>;
  using RealImagePointer = typename RealImageType::Pointer;
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;
  using RealType = float;

  /** Interpolation kernel type (default spline order = 3) */
  using KernelType = CoxDeBoorBSplineKernelFunction<3>;
  using KernelOrder0Type = BSplineKernelFunction<0>;
  using KernelOrder1Type = BSplineKernelFunction<1>;
  using KernelOrder2Type = BSplineKernelFunction<2>;
  using KernelOrder3Type = BSplineKernelFunction<3>;

  /**
   * Set the input image.  Note that the size, spacing, origin, and spline
   * order must be called prior to setting the input image.
   */
  void
  SetInputImage(const InputImageType *) override;

  /**
   * Set the spline order of the B-spline object for all parametric dimensions.
   * Default = 3.
   */
  void
  SetSplineOrder(const unsigned int);

  /**
   * Set the spline order array where each element of the array corresponds to
   * a single parametric dimension of the B-spline object.  Default = 3.
   */
  void
  SetSplineOrder(const ArrayType &);

  /**
   * Get the spline order array of the B-spline object.  Default = 3.
   */
  itkGetConstReferenceMacro(SplineOrder, ArrayType);

  /**
   * Set the boolean array indicating the periodicity of the B-spline object.
   * This array of 0/1 values defines whether a particular dimension of the
   * parametric space is to be considered periodic or not. For example, if you
   * are using interpolating along a 1D closed curve, the array type will have
   * size 1, and you should set the first element of this array to the value
   * "1". In the case that you were interpolating in a planar surface with
   * cylindrical topology, the array type will have two components, and you
   * should set to "1" the component that goes around the cylinder, and set to
   * "0" the component that goes from the top of the cylinder to the bottom.
   * This will indicate the periodicity of that parameter to the filter.
   * Internally, in order to make periodic the domain of the parameter, the
   * filter will reuse some of the points at the beginning of the domain as if
   * they were also located at the end of the domain. The number of points to
   * be reused will depend on the spline order. As a user, you don't need to
   * replicate the points, the filter will do this for you. */
  itkSetMacro(CloseDimension, ArrayType);

  /**
   * Get the boolean array indicating which dimensions are closed.
   */
  itkGetConstReferenceMacro(CloseDimension, ArrayType);

  /**
   * Set/Get the parametric spacing of the B-spline object domain.
   */
  itkSetMacro(Spacing, SpacingType);
  itkGetConstMacro(Spacing, SpacingType);

  /**
   * Set/Get the parametric origin of the B-spline object domain.
   */
  itkSetMacro(Origin, OriginType);
  itkGetConstMacro(Origin, OriginType);

  /**
   * Set/Get the parametric size of the B-spline object domain.
   */
  itkSetMacro(Size, SizeType);
  itkGetConstMacro(Size, SizeType);

  /**
   * Set/Get the epsilon used for B-splines.  The B-spline parametric domain in
   * 1-D is defined on the half-closed interval [a,b).  Extension to n-D is
   * defined similarly.  This presents some difficulty for defining the
   * the image domain to be co-extensive with the parametric domain.  We use
   * the B-spline epsilon to push the edge of the image boundary inside the
   * B-spline parametric domain.
   */
  itkSetMacro(BSplineEpsilon, RealType);
  itkGetConstMacro(BSplineEpsilon, RealType);

  /**
   * Evaluate the resulting B-spline object at a specified point in the
   * parametric domain.
   */
  OutputType
  EvaluateAtParametricPoint(const PointType &) const;

  /**
   * Evaluate the resulting B-spline object at a specified index in the
   * parametric domain.
   */
  OutputType
  EvaluateAtIndex(const IndexType &) const override;

  /**
   * Evaluate the resulting B-spline object at a specified continuous index in
   * the parametric domain.
   */
  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType &) const override;

  /**
   * Evaluate the resulting B-spline object at a specified internal parametric
   * point.  Note that the internal parameterization over each dimension of the
   * B-spline object is [0, 1).
   */
  OutputType
  Evaluate(const PointType &) const override;

  /**
   * Evaluate the gradient of the resulting B-spline object at a specified point
   * in the parametric domain.
   */
  GradientType
  EvaluateGradientAtParametricPoint(const PointType &) const;

  /**
   * Evaluate the gradient of the resulting B-spline object at a specified index
   * in the parametric domain.
   */
  GradientType
  EvaluateGradientAtIndex(const IndexType &) const;

  /**
   * Evaluate the gradient of the resulting B-spline object at a specified
   * continuous index in the parametric domain.
   */
  GradientType
  EvaluateGradientAtContinuousIndex(const ContinuousIndexType &) const;

  /**
   * Evaluate the gradient of the resulting B-spline object at a specified
   * internal parametric point.  Note that the internal parameterization over
   * each dimension of the B-spline object is [0, 1).
   */
  GradientType
  EvaluateGradient(const PointType &) const;

  /**
   * Evaluate the Hessian of the resulting B-spline object at a specified
   * point within the parametric domain.  Since the Hessian for a vector
   * function is a 3-tensor, one must specify the component.
   */
  HessianComponentType
  EvaluateHessianAtParametricPoint(const PointType &, const unsigned int) const;

  /**
   * Evaluate the Hessian of the resulting B-spline object at a specified
   * index within the parametric domain.  Since the Hessian for a vector
   * function is a 3-tensor, one must specify the component.
   */
  HessianComponentType
  EvaluateHessianAtIndex(const IndexType &, const unsigned int) const;

  /**
   * Evaluate the Hessian of the resulting B-spline object at a specified con-
   * tinuous index within the parametric domain.  Since the Hessian for a vector
   * function is a 3-tensor, one must specify the component.
   */
  HessianComponentType
  EvaluateHessianAtContinuousIndex(const ContinuousIndexType &, const unsigned int) const;

  /**
   * Evaluate the hessian of the resulting B-spline object at a specified
   * internal parametric point.  Note that the internal parameterization over
   * each dimension of the B-spline object is [0, 1).
   */
  HessianComponentType
  EvaluateHessian(const PointType &, const unsigned int) const;

protected:
  BSplineControlPointImageFunction();
  ~BSplineControlPointImageFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Parameters for the B-spline object domain */
  SizeType    m_Size;
  SpacingType m_Spacing;
  OriginType  m_Origin;

  ArrayType m_NumberOfControlPoints;
  ArrayType m_CloseDimension;
  ArrayType m_SplineOrder;

  RealImagePointer m_NeighborhoodWeightImage;

  typename KernelType::Pointer       m_Kernel[ImageDimension];
  typename KernelOrder0Type::Pointer m_KernelOrder0;
  typename KernelOrder1Type::Pointer m_KernelOrder1;
  typename KernelOrder2Type::Pointer m_KernelOrder2;
  typename KernelOrder3Type::Pointer m_KernelOrder3;

  CoordRepType m_BSplineEpsilon;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineControlPointImageFunction.hxx"
#endif

#endif
