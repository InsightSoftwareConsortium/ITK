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
#ifndef itkOptimizer_h
#define itkOptimizer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkArray.h"
#include "itkOptimizerParameters.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class Optimizer
 * \brief Generic representation for an optimization method.
 *
 *  This class is a base for a hierarchy of optimizers.
 *  It is not intended to be instantiated.
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT Optimizer : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(Optimizer);

  /** Standard class type aliases. */
  using Self = Optimizer;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Optimizer, Object);

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  using ParametersType = OptimizerParameters<double>;

  /**  Scale type.
   *  This array defines scale to be applied to parameters before
   *  being evaluated in the cost function. This allows to
   *  map to a more convenient space. In particular this is
   *  used to normalize parameter spaces in which some parameters
   *  have a different dynamic range.   */
  using ScalesType = Array<double>;

  /**  Set the position to initialize the optimization. */
  virtual void
  SetInitialPosition(const ParametersType & param);

  /** Get the position to initialize the optimization. */
  itkGetConstReferenceMacro(InitialPosition, ParametersType);

  /** Set current parameters scaling. */
  void
  SetScales(const ScalesType & scales);

  /** Get current parameters scaling. */
  itkGetConstReferenceMacro(Scales, ScalesType);
  itkGetConstReferenceMacro(InverseScales, ScalesType);

  /** Get current position of the optimization. */
  itkGetConstReferenceMacro(CurrentPosition, ParametersType);

  /** Start optimization. */
  virtual void
  StartOptimization()
  {}

  /** Get the reason for termination */
  virtual const std::string
  GetStopConditionDescription() const;

protected:
  Optimizer();
  ~Optimizer() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Set the current position. */
  virtual void
  SetCurrentPosition(const ParametersType & param);

  bool m_ScalesInitialized{ false };

  // Keep m_CurrentPosition as a protected var so that subclasses can
  // have fast access.  This is important when optimizing high-dimensional
  // spaces, e.g. bspline transforms.
  ParametersType m_CurrentPosition;

private:
  ParametersType m_InitialPosition;
  ScalesType     m_Scales;
  ScalesType     m_InverseScales;
};
} // end namespace itk

#endif
