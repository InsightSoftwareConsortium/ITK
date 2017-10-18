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
#ifndef itkTransferFunctionBase_h
#define itkTransferFunctionBase_h

#include "itkFunctionBase.h"
#include "itkArray.h"

namespace itk
{
namespace Statistics
{
/** \class TransferFunctionBase
 * \brief This is the itkTransferFunctionBase class.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename ScalarType>
class TransferFunctionBase : public FunctionBase<ScalarType, ScalarType>
{
public:

  /** Standard class typedefs. */
  typedef TransferFunctionBase                 Self;
  typedef FunctionBase<ScalarType, ScalarType> Superclass;
  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>             ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransferFunctionBase, FunctionBase);

  /** Input type */
  typedef ScalarType InputType;

  /** Output type */
  typedef ScalarType        OutputType;
  typedef Array<ScalarType> ArrayType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(const InputType& input) const ITK_OVERRIDE = 0;

  /** Evaluate the derivative at the specified input position */
  virtual OutputType EvaluateDerivative(const InputType& input) const = 0;

protected:
  TransferFunctionBase() {};
  ~TransferFunctionBase() ITK_OVERRIDE {};

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE
    {
    os << indent << "TransferFunctionBase(" << this << ")" << std::endl;
    Superclass::PrintSelf( os, indent );
    }

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(TransferFunctionBase);
};

} // end namespace Statistics
} // end namespace itk
#endif
