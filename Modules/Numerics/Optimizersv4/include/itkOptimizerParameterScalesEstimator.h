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
#ifndef __itkOptimizerParameterScalesEstimator_h
#define __itkOptimizerParameterScalesEstimator_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkObjectToObjectOptimizerBase.h"
#include "itkSingleValuedCostFunctionv4.h"

namespace itk
{

/** \class OptimizerParameterScalesEstimator
 *  \brief OptimizerParameterScalesEstimator is the base class offering a
 * empty method of estimating the parameter scales for optimizers.
 *
 * Its subclass RegistrationParameterScalesEstimator estimates scales for
 * registration optimizers.
 *
 * \ingroup ITKOptimizersv4
 */
class ITK_EXPORT OptimizerParameterScalesEstimator : public Object
{
public:
  /** Standard class typedefs. */
  typedef OptimizerParameterScalesEstimator     Self;
  typedef Object                                Superclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( OptimizerParameterScalesEstimator, Object );

  /** Type of scales */
  typedef ObjectToObjectOptimizerBase::ScalesType        ScalesType;
  /** Type of parameters of the optimizer */
  typedef ObjectToObjectOptimizerBase::ParametersType    ParametersType;
  /** Type of float */
  typedef double                                         FloatType;

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
  OptimizerParameterScalesEstimator(){};
  ~OptimizerParameterScalesEstimator(){};

  void PrintSelf(std::ostream &os, Indent indent) const
    {
    Superclass::PrintSelf(os,indent);
    }

private:
  OptimizerParameterScalesEstimator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

}; //class OptimizerParameterScalesEstimator

}  // namespace itk

#endif /* __itkOptimizerParameterScalesEstimator_h */
