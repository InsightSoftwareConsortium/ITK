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
#ifndef itkErrorFunctionBase_h
#define itkErrorFunctionBase_h

#include "itkFunctionBase.h"
#include "itkArray.h"

namespace itk
{
namespace Statistics
{
/** \class ErrorFunctionBase
 * \brief This is the itkErrorFunctionBase class.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename TMeasurementVector, typename TTargetVector>
class ErrorFunctionBase : public FunctionBase<TMeasurementVector, TTargetVector>
{
public:
  /** Standard class typedefs. */
  typedef ErrorFunctionBase                               Self;
  typedef FunctionBase<TMeasurementVector, TTargetVector> Superclass;
  typedef SmartPointer<Self>                              Pointer;
  typedef SmartPointer<const Self>                        ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ErrorFunctionBase, FunctionBase);

  /** Error type */
  typedef TMeasurementVector ErrorVectorType;

  /** Output type */
  typedef TTargetVector OutputType;

  typedef Array<double> InternalVectorType;

  virtual OutputType Evaluate(const ErrorVectorType&) const ITK_OVERRIDE = 0;

  virtual InternalVectorType EvaluateDerivative(const ErrorVectorType&) const = 0;

protected:

  ErrorFunctionBase(){};
  ~ErrorFunctionBase() ITK_OVERRIDE {};

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(ErrorFunctionBase);
};

} // end namespace itk
} // end namespace Statistics
#endif
