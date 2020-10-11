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
#ifndef itkShapePriorMAPCostFunction_h
#define itkShapePriorMAPCostFunction_h

#include "itkShapePriorMAPCostFunctionBase.h"
#include "itkGaussianKernelFunction.h"

namespace itk
{
/** \class ShapePriorMAPCostFunction
 * \brief Represents the maximum aprior (MAP) cost function used
 * ShapePriorSegmentationLevelSetImageFilter to estimate the shape parameters.
 *
 * This class follows the shape and pose parameters estimation developed in [1].
 * Note that this class returns the negative log of the MAP function.
 * Using the negative function make this cost function compatible
 * with generic optimizers which seeks the minimum of a cost function.
 *
 * This class has two template parameters, the feature image type representing the
 * edge potential map and the pixel type used to
 * represent the output level set in the ShapePriorSegmentationLevelSetImageFilter.
 *
 * \sa ShapePriorSegmentationLevelSetImageFilter
 *
 * \par REFERENCES
 * \par
 * [1] Leventon, M.E. et al. "Statistical Shape Influence in Geodesic Active Contours", CVPR 2000.
 *
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKLevelSets
 */
template <typename TFeatureImage, typename TOutputPixel>
class ITK_TEMPLATE_EXPORT ShapePriorMAPCostFunction : public ShapePriorMAPCostFunctionBase<TFeatureImage, TOutputPixel>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ShapePriorMAPCostFunction);

  /** Standard class type aliases. */
  using Self = ShapePriorMAPCostFunction;
  using Superclass = ShapePriorMAPCostFunctionBase<TFeatureImage, TOutputPixel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShapePriorMAPCostFunction, ShapePriorMAPCostFunctionBase);

  /**  ParametersType type alias.
   *  It defines a position in the optimization search space. */
  using ParametersType = typename Superclass::ParametersType;

  /** Type of the feature image representing the edge potential map. */
  using FeatureImageType = typename Superclass::FeatureImageType;
  using FeatureImagePointer = typename Superclass::FeatureImagePointer;

  /** Type of the return measure value. */
  using MeasureType = typename Superclass::MeasureType;

  /** Dimension constant. */
  static constexpr unsigned int ImageDimension = TFeatureImage::ImageDimension;

  /** Type of pixel used to represent the level set. */
  using PixelType = typename Superclass::PixelType;

  /** Type of node used to represent the active region around the zero set. */
  using NodeType = typename Superclass::NodeType;

  /** Type of container used to store the level set nodes. */
  using NodeContainerType = typename Superclass::NodeContainerType;

  /** Type of the shape signed distance function. */
  using ShapeFunctionType = typename Superclass::ShapeFunctionType;

  /** Type of the array for storing shape parameter mean and standard deviation.
   * FIXME: should be templated.
   */
  using ArrayType = Array<double>;

  /** Set/Get the array of shape parameters mean. */
  itkSetMacro(ShapeParameterMeans, ArrayType);
  itkGetConstMacro(ShapeParameterMeans, ArrayType);

  /** Set/Get the array of shape parameters standard deviation. */
  itkSetMacro(ShapeParameterStandardDeviations, ArrayType);
  itkGetConstMacro(ShapeParameterStandardDeviations, ArrayType);

  /** Set/Get the weights for each term. Default is a vector of all ones.
   * The weights are applied to terms in the following order:
   * LogInsideTerm, LogGradientTerm, LogShapePriorTerm and
   * LogPosePriorTerm. */
  using WeightsType = FixedArray<double, 4>;
  itkSetMacro(Weights, WeightsType);
  itkGetConstReferenceMacro(Weights, WeightsType);

  /** Compute the inside term component of the MAP cost function.
   * In particular, the method sums the number of pixels inside
   * the current contour (defined by nodes of the active region
   * that are less than zero) which are outside the shape
   * specified by the input parameters. */
  MeasureType
  ComputeLogInsideTerm(const ParametersType & parameters) const override;

  /** Compute the gradient term component of the MAP cost function.
   * In particular, this method assume that ( 1 - FeatureImage ) approximates
   * a Gaussian (zero mean, unit variance) algon the normal of the evolving contour.
   * The gradient term is then given by a Laplacian of the goodness of fit of
   * the Gaussian. */
  MeasureType
  ComputeLogGradientTerm(const ParametersType & parameters) const override;

  /** Compute the shape prior component of the MAP cost function.
   * In particular, the method assumes that the shape parameters comes from
   * independent Gaussian distributions defined by the ShapeParameterMeans
   * and ShapeParameterVariances array. */
  MeasureType
  ComputeLogShapePriorTerm(const ParametersType & parameters) const override;

  /** Compute the pose prior component of the MAP cost function.
   * In particular, this method assumes that the pose parameters are
   * uniformly distributed and returns a constant of zero. */
  MeasureType
  ComputeLogPosePriorTerm(const ParametersType & parameters) const override;

  /** Initialize the cost function by making sure that all the components
   *  are present. */
  void
  Initialize() override;

protected:
  ShapePriorMAPCostFunction();
  ~ShapePriorMAPCostFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ArrayType   m_ShapeParameterMeans;
  ArrayType   m_ShapeParameterStandardDeviations;
  WeightsType m_Weights;

  typename GaussianKernelFunction<double>::Pointer m_GaussianFunction;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShapePriorMAPCostFunction.hxx"
#endif

#endif
