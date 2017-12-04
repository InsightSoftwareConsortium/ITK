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
#ifndef itkPointSetToPointSetMetric_h
#define itkPointSetToPointSetMetric_h

#include "itkImageBase.h"
#include "itkTransform.h"
#include "itkMultipleValuedCostFunction.h"
#include "itkMacro.h"
#include "itkGradientRecursiveGaussianImageFilter.h"

namespace itk
{
/** \class PointSetToPointSetMetric
 * \brief Computes similarity between two point sets.
 *
 * This Class is templated over the type of the two point-sets.  It
 * expects a Transform to be plugged in.  This particular
 * class is the base class for a hierarchy of point-set to point-set metrics.
 *
 * This class computes a value that measures the similarity between the fixed point-set
 * and the transformed moving point-set.
 *
 * \ingroup RegistrationMetrics
 *
 * \ingroup ITKRegistrationCommon
 */

template< typename TFixedPointSet,  typename TMovingPointSet >
class ITK_TEMPLATE_EXPORT PointSetToPointSetMetric:public MultipleValuedCostFunction
{
public:

  /** Standard class typedefs. */
  typedef PointSetToPointSetMetric   Self;
  typedef MultipleValuedCostFunction Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Type used for representing point components  */
  typedef Superclass::ParametersValueType CoordinateRepresentationType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetToPointSetMetric, MultipleValuedCostFunction);

  /**  Type of the moving Pointset. */
  typedef TMovingPointSet                           MovingPointSetType;
  typedef typename TMovingPointSet::PixelType       MovingPointSetPixelType;
  typedef typename MovingPointSetType::ConstPointer MovingPointSetConstPointer;

  /**  Type of the fixed Pointset. */
  typedef TFixedPointSet                           FixedPointSetType;
  typedef typename FixedPointSetType::ConstPointer FixedPointSetConstPointer;

  /** Constants for the pointset dimensions */
  itkStaticConstMacro(MovingPointSetDimension, unsigned int,
                      TMovingPointSet::PointDimension);
  itkStaticConstMacro(FixedPointSetDimension, unsigned int,
                      TFixedPointSet::PointDimension);

  typedef typename FixedPointSetType::PointsContainer::ConstIterator     FixedPointIterator;
  typedef typename FixedPointSetType::PointDataContainer::ConstIterator  FixedPointDataIterator;

  typedef typename MovingPointSetType::PointsContainer::ConstIterator    MovingPointIterator;
  typedef typename MovingPointSetType::PointDataContainer::ConstIterator MovingPointDataIterator;

  /**  Type of the Transform Base class */
  typedef Transform< CoordinateRepresentationType,
                     itkGetStaticConstMacro(MovingPointSetDimension),
                     itkGetStaticConstMacro(FixedPointSetDimension) > TransformType;

  typedef typename TransformType::Pointer         TransformPointer;
  typedef typename TransformType::InputPointType  InputPointType;
  typedef typename TransformType::OutputPointType OutputPointType;
  typedef typename TransformType::ParametersType  TransformParametersType;
  typedef typename TransformType::JacobianType    TransformJacobianType;

  /**  Type of the measure. */
  typedef Superclass::MeasureType MeasureType;

  /**  Type of the derivative. */
  typedef Superclass::DerivativeType DerivativeType;

  /**  Type of the parameters. */
  typedef Superclass::ParametersType ParametersType;

  /** Get/Set the Fixed Pointset.  */
  itkSetConstObjectMacro(FixedPointSet, FixedPointSetType);
  itkGetConstObjectMacro(FixedPointSet, FixedPointSetType);

  /** Get/Set the Moving Pointset.  */
  itkSetConstObjectMacro(MovingPointSet, MovingPointSetType);
  itkGetConstObjectMacro(MovingPointSet, MovingPointSetType);

  /** Connect the Transform. */
  itkSetObjectMacro(Transform, TransformType);

  /** Get a pointer to the Transform.  */
  itkGetModifiableObjectMacro(Transform, TransformType);

  /** Set the parameters defining the Transform. */
  void SetTransformParameters(const ParametersType & parameters) const;

  /** Return the number of parameters required by the Transform */
  virtual unsigned int GetNumberOfParameters(void) const ITK_OVERRIDE
  { return m_Transform->GetNumberOfParameters(); }

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly     */
  virtual void Initialize(void);

protected:
  PointSetToPointSetMetric();
  virtual ~PointSetToPointSetMetric() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  FixedPointSetConstPointer m_FixedPointSet;

  MovingPointSetConstPointer m_MovingPointSet;

  mutable TransformPointer m_Transform;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PointSetToPointSetMetric);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSetToPointSetMetric.hxx"
#endif

#endif
