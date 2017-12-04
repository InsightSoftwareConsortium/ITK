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
#ifndef itkPointSetToImageRegistrationMethod_h
#define itkPointSetToImageRegistrationMethod_h

#include "itkProcessObject.h"
#include "itkImage.h"
#include "itkPointSetToImageMetric.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkDataObjectDecorator.h"

namespace itk
{
/** \class PointSetToImageRegistrationMethod
 * \brief Base class for PointSet to Image Registration Methods.
 *
 * This Class define the generic interface for a registration method.
 *
 * This class is templated over the type of the PointSet and the Image to be
 * registered. A generic Transform is used by this class. That allows to select
 * at run time the particular type of transformation that is to be applied for
 * registering the images.
 *
 * This class uses a generic Metric in order to compare the PointSet and the
 * Image. The final goal of the registration method is to find the set of
 * parameters of the Transformation that optimizes the metric.
 *
 * The registration method also supports a generic optimizer that can be
 * selected at run-time. The only restriction for the optimizer is that it
 * should be able to operate in single-valued cost functions given that the
 * metrics used to compare PointSet with Images provide a single value as
 * output.
 *
 * The terms FixedPointSet and MovingImage are used in this class to indicate
 * that the image is being mapped by the transform.
 *
 * This class uses the coordinate system of the Fixed PointSet as a reference
 * and searchs for a Transform that will map points from the space of the Fixed
 * PointSet to the space of the Moving image.
 *
 * For doing so, a Metric will be continuously applied to compare the Fixed
 * image with the Transformed Moving image. This process also requires to
 * interpolate values from the Moving image.
 *
 * This class requires the Transform, the Interpolator, the Metric, and the
 * Optimizer to be explicitly set.
 *
 * \ingroup RegistrationFilters
 * \ingroup ITKRegistrationCommon
 */
template< typename TFixedPointSet, typename TMovingImage >
class ITK_TEMPLATE_EXPORT PointSetToImageRegistrationMethod : public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef PointSetToImageRegistrationMethod Self;
  typedef ProcessObject                     Superclass;
  typedef SmartPointer< Self >              Pointer;
  typedef SmartPointer< const Self >        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetToImageRegistrationMethod, ProcessObject);

  /**  Type of the Fixed PointSet. */
  typedef          TFixedPointSet                  FixedPointSetType;
  typedef typename FixedPointSetType::ConstPointer FixedPointSetConstPointer;

  /**  Type of the Moving image. */
  typedef          TMovingImage                  MovingImageType;
  typedef typename MovingImageType::ConstPointer MovingImageConstPointer;

  /**  Type of the Metric. */
  typedef PointSetToImageMetric< FixedPointSetType, MovingImageType > MetricType;
  typedef typename MetricType::Pointer                                MetricPointer;

  /**  Type of the Transform. */
  typedef  typename MetricType::TransformType TransformType;
  typedef  typename TransformType::Pointer    TransformPointer;

  /** Type for the output: Using Decorator pattern for enabling
   *  the Transform to be passed in the data pipeline */
  typedef  DataObjectDecorator< TransformType >      TransformOutputType;
  typedef typename TransformOutputType::Pointer      TransformOutputPointer;
  typedef typename TransformOutputType::ConstPointer TransformOutputConstPointer;

  /**  Type of the Interpolator. */
  typedef  typename MetricType::InterpolatorType InterpolatorType;
  typedef  typename InterpolatorType::Pointer    InterpolatorPointer;

  /**  Type of the Optimizer. */
  typedef   SingleValuedNonLinearOptimizer OptimizerType;

  /** Type of the Transformation parameters This is the same type used to
   *  represent the search space of the optimization algorithm */
  typedef  typename MetricType::TransformParametersType ParametersType;

  /** Smart Pointer type to a DataObject. */
  typedef typename DataObject::Pointer DataObjectPointer;

  /** Set/Get the Fixed image. */
  itkSetConstObjectMacro(FixedPointSet, FixedPointSetType);
  itkGetConstObjectMacro(FixedPointSet, FixedPointSetType);

  /** Set/Get the Moving image. */
  itkSetConstObjectMacro(MovingImage, MovingImageType);
  itkGetConstObjectMacro(MovingImage, MovingImageType);

  /** Set/Get the Optimizer. */
  itkSetObjectMacro(Optimizer,  OptimizerType);
  itkGetModifiableObjectMacro(Optimizer, OptimizerType);

  /** Set/Get the Metric. */
  itkSetObjectMacro(Metric, MetricType);
  itkGetModifiableObjectMacro(Metric, MetricType);

  /** Set/Get the Transfrom. */
  itkSetObjectMacro(Transform, TransformType);
  itkGetModifiableObjectMacro(Transform, TransformType);

  /** Set/Get the Interpolator. */
  itkSetObjectMacro(Interpolator, InterpolatorType);
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Set/Get the initial transformation parameters. */
  virtual void SetInitialTransformParameters(const ParametersType & param);

  itkGetConstReferenceMacro(InitialTransformParameters, ParametersType);

  /** Get the last transformation parameters visited by
   * the Optimizer. */
  itkGetConstReferenceMacro(LastTransformParameters, ParametersType);

  /** Initialize by setting the interconnects between the components. */
  void Initialize();

  /** Returns the transform resulting from the registration process. */
  const TransformOutputType * GetOutput() const;

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

  virtual ModifiedTimeType GetMTime() const ITK_OVERRIDE;

#ifdef ITKV3_COMPATIBILITY
  /** Method that initiates the registration. */
  // StartRegistration is an old API from before
  // ImageRegistrationMethod was a subclass of ProcessObject.
  // Historically, one could call StartRegistration() instead of
  // calling Update().  However, when called directly by the user, the
  // inputs to ImageRegistrationMethod may not be up to date.  This
  // may cause an unexpected behavior.
  //
  // Since we cannot eliminate StartRegistration for backward
  // compatibility reasons, we check whether StartRegistration was
  // called directly or whether Update() (which in turn called
  // StartRegistration()).
  void StartRegistration(void) { this->Update(); }
#endif

protected:
  PointSetToImageRegistrationMethod();
  virtual ~PointSetToImageRegistrationMethod() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void  GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PointSetToImageRegistrationMethod);

  MetricPointer          m_Metric;
  OptimizerType::Pointer m_Optimizer;

  MovingImageConstPointer   m_MovingImage;
  FixedPointSetConstPointer m_FixedPointSet;

  TransformPointer    m_Transform;
  InterpolatorPointer m_Interpolator;

  ParametersType m_InitialTransformParameters;
  ParametersType m_LastTransformParameters;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSetToImageRegistrationMethod.hxx"
#endif

#endif
