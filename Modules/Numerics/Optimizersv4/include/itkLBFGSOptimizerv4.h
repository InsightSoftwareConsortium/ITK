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
#ifndef itkLBFGSOptimizerv4_h
#define itkLBFGSOptimizerv4_h

#include "itkLBFGSOptimizerBasev4.h"
#include "vnl/algo/vnl_lbfgs.h"
#include "ITKOptimizersv4Export.h"

namespace itk
{
/** \class LBFGSOptimizerv4
 * \brief Wrap of the vnl_lbfgs algorithm for use in ITKv4 registration framework.
 *
 * \ingroup ITKOptimizersv4
 */

class ITKOptimizersv4_EXPORT LBFGSOptimizerv4:
    public LBFGSOptimizerBasev4< vnl_lbfgs >
{
public:
  /** Standard "Self" typedef. */
  typedef LBFGSOptimizerv4                  Self;
  typedef LBFGSOptimizerBasev4<vnl_lbfgs>   Superclass;
  typedef SmartPointer< Self >              Pointer;
  typedef SmartPointer< const Self >        ConstPointer;

  typedef Superclass::MetricType     MetricType;
  typedef Superclass::ParametersType ParametersType;
  typedef Superclass::ScalesType     ScalesType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LBFGSOptimizerv4, Superclass);

  /** Start optimization with an initial value. */
  virtual void StartOptimization(bool doOnlyInitialization = false) ITK_OVERRIDE;

  /** Plug in a Cost Function into the optimizer  */
  virtual void SetMetric(MetricType *metric) ITK_OVERRIDE;

  void VerboseOn();
  void VerboseOff();

  /** Set/Get the line search accuracy. This is a positive real number
   * with a default value of 0.9, which controls the accuracy of the line
   * search. If the function and gradient evalutions are inexpensive with
   * respect to the cost of the iterations it may be advantageous to set
   * the value to a small value (say 0.1).
   */
  void SetLineSearchAccuracy(double tol);

  itkGetConstMacro(LineSearchAccuracy, double);

  /** Set/Get the default step size. This is a positive real number
   * with a default value of 1.0 which determines the step size in the line
   * search.
   */
  void SetDefaultStepLength(double stp);

  itkGetConstMacro(DefaultStepLength, double);

protected:
  LBFGSOptimizerv4();
  virtual ~LBFGSOptimizerv4();
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** InternalParameters typedef. */
  typedef vnl_vector< double >  InternalParametersType;

  /** Internal optimizer type. */
  typedef   vnl_lbfgs           InternalOptimizerType;

private:
  LBFGSOptimizerv4(const Self &); //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  bool         m_Verbose;
  double       m_LineSearchAccuracy;
  double       m_DefaultStepLength;
};
} // end namespace itk
#endif
