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
#ifndef itkMultiquadricRadialBasisFunction_h
#define itkMultiquadricRadialBasisFunction_h

#include "itkTransferFunctionBase.h"

namespace itk
{
namespace Statistics
{
/** \class MultiquadricRadialBasisFunction
 * \brief This is the itkMultiquadricRadialBasisFunction class.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename ScalarType>
class ITK_TEMPLATE_EXPORT MultiquadricRadialBasisFunction : public TransferFunctionBase<ScalarType>
{
public:

  /** Standard class typedefs. */
  typedef MultiquadricRadialBasisFunction  Self;
  typedef TransferFunctionBase<ScalarType> Superclass;
  typedef SmartPointer<Self>               Pointer;
  typedef SmartPointer<const Self>         ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiquadricRadialBasisFunction, TransferFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Evaluate at the specified input position */
  virtual ScalarType Evaluate(const ScalarType& input) const; //{return 0;};

  /** Evaluate the derivative at the specified input position */
  virtual ScalarType EvaluateDerivative(const ScalarType& ) const{return 0;};

  itkSetMacro(Radius,ScalarType);
  itkGetConstReferenceMacro(Radius,ScalarType);
  itkSetMacro(Center,ScalarType);
  itkGetConstReferenceMacro(Center,ScalarType);

protected:

  MultiquadricRadialBasisFunction();
  virtual ~MultiquadricRadialBasisFunction();

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

private:

  ScalarType m_Radius;
  ScalarType m_Center;

};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMultiquadricRadialBasisFunction.hxx"
#endif


#endif
