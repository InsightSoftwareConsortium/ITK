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
#ifndef itkRegistrationParameterScalesFromShiftBase_h
#define itkRegistrationParameterScalesFromShiftBase_h

#include "itkRegistrationParameterScalesEstimator.h"

namespace itk
{

/** \class RegistrationParameterScalesFromShiftBase
 *  \brief Registration helper base class for estimating scales of
 * transform parameters from the maximum voxel shift caused by a parameter
 * change.
 *
 * Derived classes provide estimation using physical or index space.
 *
 * The scale of a parameter is estimated from the maximum voxel shift produced
 * from a small variation of this parameter. The maximization is done by checking
 * sample points within the metric's virtual domain. Sample points are generated
 * differently depending on the type of metric transform or metric type.
 * See RegistrationParameterScalesEstimator documentation.
 *
 * \sa RegistrationParameterScalesEstimator
 * \ingroup ITKOptimizersv4
 */
template < typename TMetric >
class ITK_TEMPLATE_EXPORT RegistrationParameterScalesFromShiftBase :
  public RegistrationParameterScalesEstimator< TMetric >
{
public:
  /** Standard class typedefs. */
  typedef RegistrationParameterScalesFromShiftBase        Self;
  typedef RegistrationParameterScalesEstimator< TMetric > Superclass;
  typedef SmartPointer<Self>                              Pointer;
  typedef SmartPointer<const Self>                        ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( RegistrationParameterScalesFromShiftBase, RegistrationParameterScalesEstimator );

  /** Type of scales */
  typedef typename Superclass::ScalesType                ScalesType;
  /** Type of parameters of the optimizer */
  typedef typename Superclass::ParametersType            ParametersType;
  typedef typename ParametersType::ValueType             ParametersValueType;
  /** Type of float */
  typedef typename Superclass::FloatType                 FloatType;

  typedef typename Superclass::VirtualPointType          VirtualPointType;
  typedef typename Superclass::VirtualIndexType          VirtualIndexType;
  typedef typename Superclass::MovingTransformType       MovingTransformType;
  typedef typename Superclass::FixedTransformType        FixedTransformType;
  typedef typename Superclass::JacobianType              JacobianType;
  typedef typename Superclass::VirtualImageConstPointer  VirtualImageConstPointer;

  /** Estimate parameter scales */
  virtual void EstimateScales(ScalesType &scales) ITK_OVERRIDE;

  /** Estimate the scale of a step */
  virtual FloatType EstimateStepScale(const ParametersType &step) ITK_OVERRIDE;

  /** Estimate the scales of local steps */
  virtual void EstimateLocalStepScales(const ParametersType &step,
    ScalesType &localStepScales) ITK_OVERRIDE;

  /** Set/get small parameter variation */
  itkSetMacro( SmallParameterVariation, ParametersValueType );
  itkGetConstMacro( SmallParameterVariation, ParametersValueType );

protected:
  RegistrationParameterScalesFromShiftBase();
  ~RegistrationParameterScalesFromShiftBase() ITK_OVERRIDE {};

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Compute the shift in voxels when deltaParameters is applied onto the
   * current parameters. */
  virtual FloatType ComputeMaximumVoxelShift(const ParametersType &deltaParameters);

  /** Compute the sample shifts.
   */
  virtual void ComputeSampleShifts(const ParametersType &deltaParameters, ScalesType &localShifts) = 0;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegistrationParameterScalesFromShiftBase);

  //A small variation of parameters
  ParametersValueType  m_SmallParameterVariation;

}; //class RegistrationParameterScalesFromShiftBase


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationParameterScalesFromShiftBase.hxx"
#endif

#endif /* itkRegistrationParameterScalesFromShiftBase_h */
