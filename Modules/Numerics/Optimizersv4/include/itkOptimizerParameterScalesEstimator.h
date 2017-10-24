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
#ifndef itkOptimizerParameterScalesEstimator_h
#define itkOptimizerParameterScalesEstimator_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkOptimizerParameters.h"

namespace itk
{

/** \class OptimizerParameterScalesEstimatorTemplate
 *  \brief OptimizerParameterScalesEstimatorTemplate is the base class offering a
 * empty method of estimating the parameter scales for optimizers.
 *
 * Its subclass RegistrationParameterScalesEstimator estimates scales for
 * registration optimizers.
 *
 * \ingroup ITKOptimizersv4
 */
template< typename TInternalComputationValueType=double >
class OptimizerParameterScalesEstimatorTemplate : public Object
{
public:
  /** Standard class typedefs. */
  typedef OptimizerParameterScalesEstimatorTemplate     Self;
  typedef Object                                        Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( OptimizerParameterScalesEstimatorTemplate, Object );

  /** Type of scales */
  typedef OptimizerParameters<TInternalComputationValueType> ScalesType;
  /** Type of parameters of the optimizer */
  typedef OptimizerParameters<TInternalComputationValueType> ParametersType;

  /** Type of float */
  typedef TInternalComputationValueType FloatType;

  /** Estimate parameter scales. */
  virtual void EstimateScales(ScalesType &scales) = 0;

  /** Estimate the scale of a step. */
  virtual FloatType EstimateStepScale(const ParametersType &step) = 0;

  /** Estimate the scales of local steps. */
  virtual void EstimateLocalStepScales(const ParametersType &step,
    ScalesType &localStepScales) = 0;

  /** Estimate the maximum size for steps. */
  virtual FloatType EstimateMaximumStepSize() = 0;

protected:
  OptimizerParameterScalesEstimatorTemplate(){};
  ~OptimizerParameterScalesEstimatorTemplate() ITK_OVERRIDE {};

  virtual void PrintSelf(std::ostream &os, Indent indent) const ITK_OVERRIDE
    {
    Superclass::PrintSelf(os,indent);
    }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(OptimizerParameterScalesEstimatorTemplate);

}; //class OptimizerParameterScalesEstimatorTemplate

/** This helps to meet backward compatibility */
typedef OptimizerParameterScalesEstimatorTemplate<double> OptimizerParameterScalesEstimator;

}  // namespace itk

#endif
