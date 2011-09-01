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
#ifndef __itkRegistrationParameterScalesFromJacobian_h
#define __itkRegistrationParameterScalesFromJacobian_h

#include "itkRegistrationParameterScalesEstimator.h"

namespace itk
{

/** \class RegistrationParameterScalesFromJacobian
 *  \brief Implements a registration helper class for estimating scales of
 * transform parameters from Jacobian norms.
 *
 * Its input includes the fixed/moving images and transform objects,
 * which can be obtained from the metric object.
 *
 * The scale of a parameter is estimated from the averaged squared norm of
 * the Jacobian w.r.t the parameter. The averaging is done over a sampling
 * of the image domain. The sampling by default is a uniform random
 * distribution.
 *
 * \ingroup ITKHighDimensionalOptimizers
 */
template < class TMetric >
class ITK_EXPORT RegistrationParameterScalesFromJacobian :
  public RegistrationParameterScalesEstimator< TMetric >
{
public:
  /** Standard class typedefs. */
  typedef RegistrationParameterScalesFromJacobian         Self;
  typedef RegistrationParameterScalesEstimator< TMetric > Superclass;
  typedef SmartPointer<Self>                              Pointer;
  typedef SmartPointer<const Self>                        ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( RegistrationParameterScalesFromJacobian, RegistrationParameterScalesEstimator );

  /** Type of scales */
  typedef typename Superclass::ScalesType                ScalesType;
  /** Type of paramters of the optimizer */
  typedef typename Superclass::ParametersType            ParametersType;
  /** Type of float */
  typedef typename Superclass::FloatType                 FloatType;

  typedef typename Superclass::VirtualPointType          VirtualPointType;
  typedef typename Superclass::MovingTransformType       MovingTransformType;
  typedef typename Superclass::FixedTransformType        FixedTransformType;
  typedef typename Superclass::MovingJacobianType        MovingJacobianType;
  typedef typename Superclass::FixedJacobianType         FixedJacobianType;

  /** Estimate parameter scales */
  virtual void EstimateScales(ScalesType &scales);

protected:
  RegistrationParameterScalesFromJacobian();
  ~RegistrationParameterScalesFromJacobian(){};

  virtual void PrintSelf(std::ostream &os, Indent indent) const;

private:
  RegistrationParameterScalesFromJacobian(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

}; //class RegistrationParameterScalesFromJacobian


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationParameterScalesFromJacobian.hxx"
#endif

#endif /* __itkRegistrationParameterScalesFromJacobian_h */
