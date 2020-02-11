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

#ifndef itkStoppingCriterionBase_h
#define itkStoppingCriterionBase_h

#include "itkObject.h"
#include "itkObjectFactoryBase.h"
#include "itkMacro.h"

namespace itk
{
/**
 * \class StoppingCriterionBase
 * \brief An abstract base class to represent a stopping criterion for an iterative
 *        algorithm.
 *
 * The main method is StoppingCriterionBase::IsSatisfied that must be
 * reimplemented in inheriting classes.
 *
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT StoppingCriterionBase : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(StoppingCriterionBase);

  using Self = StoppingCriterionBase;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(StoppingCriterionBase, Object);

  /** \return \c true if the stopping criterion is reached (and the algorithm
  must stop).
      \return \c fasle else. */
  virtual bool
  IsSatisfied() const = 0;
  virtual std::string
  GetDescription() const = 0;

protected:
  /** \brief Constructor */
  StoppingCriterionBase();
  /** \brief Destructor */
  ~StoppingCriterionBase() override;
};

} // namespace itk
#endif
