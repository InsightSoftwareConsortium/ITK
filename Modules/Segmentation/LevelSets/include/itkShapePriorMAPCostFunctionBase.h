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
#ifndef itkShapePriorMAPCostFunctionBase_h
#define itkShapePriorMAPCostFunctionBase_h

#include "itkSingleValuedCostFunction.h"
#include "itkLevelSet.h"
#include "itkShapeSignedDistanceFunction.h"

namespace itk
{
/** \class ShapePriorMAPCostFunctionBase
 * \brief Represents the base class of maximum aprior (MAP) cost function used
 * ShapePriorSegmentationLevelSetImageFilter to estimate the shape paramaeters.
 *
 * This class follows the shape and pose parameters estimation developed in [1].
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
template< typename TFeatureImage, typename TOutputPixel >
class ITK_TEMPLATE_EXPORT ShapePriorMAPCostFunctionBase:
  public SingleValuedCostFunction
{
public:
  /** Standard class typedefs. */
  typedef ShapePriorMAPCostFunctionBase Self;
  typedef SingleValuedCostFunction      Superclass;
  typedef SmartPointer< Self >          Pointer;
  typedef SmartPointer< const Self >    ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShapePriorMAPCostFunctionBase, SingleValuedCostFunction);

  /**  MeasureType typedef.
   *  It defines a type used to return the cost function value. */
  typedef typename Superclass::MeasureType MeasureType;

  /** DerivativeType typedef.
   *  It defines a type used to return the cost function derivative.  */
  typedef typename Superclass::DerivativeType DerivativeType;

  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef typename Superclass::ParametersType ParametersType;

  /** Type of the feature image representing the edge potential map. */
  typedef TFeatureImage                           FeatureImageType;
  typedef typename FeatureImageType::ConstPointer FeatureImagePointer;

  /** Dimension constant. */
  itkStaticConstMacro(ImageDimension, unsigned int, TFeatureImage::ImageDimension);

  /** Type of pixel used to represent the level set. */
  typedef TOutputPixel PixelType;

  /** Type of node used to represent the active region around the zero set. */
  typedef LevelSetNode< PixelType, itkGetStaticConstMacro(ImageDimension) > NodeType;

  /** Type of container used to store the level set nodes. */
  typedef VectorContainer< unsigned int, NodeType > NodeContainerType;
  typedef typename NodeContainerType::ConstPointer  NodeContainerPointer;

  /** Type of the shape signed distance function. */
  typedef ShapeSignedDistanceFunction< double,
                                       itkGetStaticConstMacro(ImageDimension) > ShapeFunctionType;
  typedef typename ShapeFunctionType::Pointer ShapeFunctionPointer;

  /** Set/Get the shape distance function. */
  itkSetObjectMacro(ShapeFunction, ShapeFunctionType);
  itkGetModifiableObjectMacro(ShapeFunction, ShapeFunctionType);

  /** Set/Get the active region. */
  itkSetConstObjectMacro(ActiveRegion, NodeContainerType);
  itkGetConstObjectMacro(ActiveRegion, NodeContainerType);

  /** Set/Get the feature image. */
  itkSetConstObjectMacro(FeatureImage, FeatureImageType);
  itkGetConstObjectMacro(FeatureImage, FeatureImageType);

  /** This method returns the value of the cost function corresponding
    * to the specified parameters.    */
  virtual MeasureType GetValue(const ParametersType & parameters) const ITK_OVERRIDE;

  /** This method returns the derivative of the cost function corresponding
    * to the specified parameters.   */
  virtual void GetDerivative(const ParametersType &, DerivativeType &) const ITK_OVERRIDE
  { itkExceptionMacro(<< "This function is currently not supported."); }

  /** Return the number of parameters. */
  virtual unsigned int GetNumberOfParameters(void) const ITK_OVERRIDE
  { return m_ShapeFunction->GetNumberOfParameters(); }

  /** Compute the inside term component of the MAP cost function.
   * Subclasses should override this function */
  virtual MeasureType ComputeLogInsideTerm(const ParametersType &) const = 0;

  /** Compute the gradient term component of the MAP cost function.
   * Subclasses should override this function */
  virtual MeasureType ComputeLogGradientTerm(const ParametersType &) const = 0;

  /** Compute the shape prior component of the MAP cost function.
   * Subclasses should override this function */
  virtual MeasureType ComputeLogShapePriorTerm(const ParametersType &) const = 0;

  /** Compute the pose prior component of the MAP cost function.
   * Subclasses should override this function */
  virtual MeasureType ComputeLogPosePriorTerm(const ParametersType &) const = 0;

  /** Initialize the cost function by making sure that all the components
   *  are present. */
  virtual void Initialize(void);

protected:
  ShapePriorMAPCostFunctionBase();
  virtual ~ShapePriorMAPCostFunctionBase() ITK_OVERRIDE {}

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  ShapeFunctionPointer m_ShapeFunction;
  NodeContainerPointer m_ActiveRegion;

  FeatureImagePointer m_FeatureImage;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShapePriorMAPCostFunctionBase);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapePriorMAPCostFunctionBase.hxx"
#endif

#endif
