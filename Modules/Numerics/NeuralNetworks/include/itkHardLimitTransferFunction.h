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
#ifndef itkHardLimitTransferFunction_h
#define itkHardLimitTransferFunction_h

#include "itkTransferFunctionBase.h"

namespace itk
{
namespace Statistics
{
/** \class HardLimitTransferFunction
 * \brief This is the itkHardLimitTransferFunction class.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename ScalarType>
class ITK_TEMPLATE_EXPORT HardLimitTransferFunction : public TransferFunctionBase<ScalarType>
{
public:
  /** Standard class typedefs. */
  typedef HardLimitTransferFunction        Self;
  typedef TransferFunctionBase<ScalarType> Superclass;
  typedef SmartPointer<Self>               Pointer;
  typedef SmartPointer<const Self>         ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(HardLimitTransferFunction, TransferFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Evaluate at the specified input position */
  virtual ScalarType Evaluate(const ScalarType& input) const;

  /** Evaluate the derivative at the specified input position */
  virtual ScalarType EvaluateDerivative(const ScalarType& input) const;

protected:

  HardLimitTransferFunction();
  virtual ~HardLimitTransferFunction();

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

};//class

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkHardLimitTransferFunction.hxx"
#endif


#endif
