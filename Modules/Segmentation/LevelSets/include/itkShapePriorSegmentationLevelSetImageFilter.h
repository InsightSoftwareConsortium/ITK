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
#ifndef itkShapePriorSegmentationLevelSetImageFilter_h
#define itkShapePriorSegmentationLevelSetImageFilter_h

#include "itkSegmentationLevelSetImageFilter.h"
#include "itkShapePriorSegmentationLevelSetFunction.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkShapePriorMAPCostFunctionBase.h"
#include "itkMath.h"

namespace itk
{
/**
 *
 * \class ShapePriorSegmentationLevelSetImageFilter
 *
 * \brief A base class which defines the API for implementing a level set
 * segmentation filter with statistical shape influence.
 *
 * \par OVERVIEW
 * This class extends the functionality of SegmentationLevelSetImageFilter
 * with an additional statistical shape influence term in the level set evolution as
 * developed in [1].
 *
 * \par TEMPLATE PARAMETERS
 * There are two required and one optional template parameter for these
 * filters.
 *
 * TInputImage is the image type of the initial model you will input to the
 * filter using SetInput() or SetInitialImage().
 *
 * TFeatureImage is the image type of the image from which the filter will
 * calculate the speed term for segmentation (see INPUTS).
 *
 * TOutputPixelType is the data type used for the output image phi, the implicit
 * level set image.  This should really only ever be set as float (default) or
 * double.
 *
 *
 * \par PARAMETERS
 *
 * \par
 * From a level set evolution point of view, the shape is represented by a
 * signed distance function from the shape encapsulated in a ShapeSignedDistanceFunction
 * object.
 *
 * \sa ShapeSignedDistanceFunction
 * \sa ShapePriorSegmentationLevelSetFunction
 *
 * \par REFERENCES
 * \par
 * [1] Leventon, M.E. et al. "Statistical Shape Influence in Geodesic Active Contours", CVPR 2000.
 *
 * \ingroup ITKLevelSets
 */
template< typename TInputImage,
          typename TFeatureImage,
          typename TOutputPixelType = float >
class ITK_TEMPLATE_EXPORT ShapePriorSegmentationLevelSetImageFilter:
  public SegmentationLevelSetImageFilter< TInputImage, TFeatureImage,
                                          TOutputPixelType >
{
public:

  /** Dimension of the input/level set image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard class typedefs */
  typedef ShapePriorSegmentationLevelSetImageFilter Self;
  typedef SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShapePriorSegmentationLevelSetImageFilter, SegmentationLevelSetImageFilter);

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType        ValueType;
  typedef typename Superclass::OutputImageType  OutputImageType;
  typedef typename Superclass::FeatureImageType FeatureImageType;

  /** Type of the output pixel. */
  typedef TOutputPixelType OutputPixelType;

  /** The level set function with shape prior type */
  typedef ShapePriorSegmentationLevelSetFunction< OutputImageType,
                                                  FeatureImageType > ShapePriorSegmentationFunctionType;

  /** The shape signed distance function type. */
  typedef typename ShapePriorSegmentationFunctionType::ShapeFunctionType ShapeFunctionType;
  typedef typename ShapeFunctionType::Pointer                            ShapeFunctionPointer;

  /** The type of the MAP estimate cost function. */
  typedef  ShapePriorMAPCostFunctionBase< TFeatureImage, TOutputPixelType > CostFunctionType;
  typedef  typename CostFunctionType::Pointer                               CostFunctionPointer;
  typedef  typename CostFunctionType::ParametersType                        ParametersType;

  /** Type of node used to represent the active region around the zero set. */
  typedef  typename CostFunctionType::NodeType          NodeType;
  typedef  typename CostFunctionType::NodeContainerType NodeContainerType;
  typedef  typename NodeContainerType::Pointer          NodeContainerPointer;

  /** The type of optimizer used to compute the MAP estimate of the shape and
    pose parameters. */
  typedef   SingleValuedNonLinearOptimizer  OptimizerType;
  typedef   typename OptimizerType::Pointer OptimizerPointer;

  /** Set/Get the shape signed distance function. */
  virtual void SetShapeFunction(ShapeFunctionType *s);
  itkGetModifiableObjectMacro(ShapeFunction, ShapeFunctionType);

  /** Set/Get the shape prior MAP cost function. */
  itkSetObjectMacro(CostFunction, CostFunctionType);
  itkGetModifiableObjectMacro(CostFunction, CostFunctionType);

  /** Set/Get the optimizer. */
  itkSetObjectMacro(Optimizer, OptimizerType);
  itkGetModifiableObjectMacro(Optimizer, OptimizerType);

  /** Set/Get the initial parameters. These are the initial parameters applied
   * to the ShapeFunction. The user should refer to the documentation of
   * the particular type of ShapeSignedDistanceFunction used to determine
   * the meaning of the parameters. */
  itkSetMacro(InitialParameters, ParametersType);
  itkGetConstMacro(InitialParameters, ParametersType);

  /** Set/Get the scaling of the shape prior term. */
  void SetShapePriorScaling(ValueType v)
  {
    if ( Math::NotExactlyEquals(v, m_ShapePriorSegmentationFunction->GetShapePriorWeight()) )
      {
      m_ShapePriorSegmentationFunction->SetShapePriorWeight(v);
      this->Modified();
      }
  }

  ValueType GetShapePriorScaling() const
  {
    return m_ShapePriorSegmentationFunction->GetShapePriorWeight();
  }

  /** Set the shape prior segmentation function. In general, this should only be called
   * by a subclass of this object. It is made public to allow itk::Command objects access. */
  virtual void SetShapePriorSegmentationFunction(ShapePriorSegmentationFunctionType *s);

  virtual ShapePriorSegmentationFunctionType * GetShapePriorSegmentationFunction()
  { return m_ShapePriorSegmentationFunction; }

  /** Get the current parameters. */
  itkGetConstReferenceMacro(CurrentParameters, ParametersType);

protected:
  virtual ~ShapePriorSegmentationLevelSetImageFilter() ITK_OVERRIDE {}
  ShapePriorSegmentationLevelSetImageFilter();

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Overrides parent implementation. MAP estimates of the shape and pose parameters
   is computed in this method. */
  virtual void InitializeIteration() ITK_OVERRIDE;

  /** Overridden from ProcessObject to set certain values before starting the
   * finite difference solver and then create an appropriate output */
  void GenerateData() ITK_OVERRIDE;

  /** Extract node of active region into a NodeContainer */
  void ExtractActiveRegion(NodeContainerType *ptr);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShapePriorSegmentationLevelSetImageFilter);

  ShapeFunctionPointer m_ShapeFunction;
  CostFunctionPointer  m_CostFunction;
  OptimizerPointer     m_Optimizer;
  ParametersType       m_InitialParameters;
  ParametersType       m_CurrentParameters;

  ShapePriorSegmentationFunctionType *m_ShapePriorSegmentationFunction;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapePriorSegmentationLevelSetImageFilter.hxx"
#endif

#endif
