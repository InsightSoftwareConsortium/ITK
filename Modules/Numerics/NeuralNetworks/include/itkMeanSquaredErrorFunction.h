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
#ifndef itkMeanSquaredErrorFunction_h
#define itkMeanSquaredErrorFunction_h

#include "itkErrorFunctionBase.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"

namespace itk
{
namespace Statistics
{
/** \class MeanSquaredErrorFunction_
 * \brief This is the itkMeanSquaredErrorFunction_ class.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename TMeasurementVector, typename ScalarType>
class ITK_TEMPLATE_EXPORT MeanSquaredErrorFunction : public ErrorFunctionBase<TMeasurementVector, ScalarType>
{
public:

  /** Standard class typedefs. */
  typedef MeanSquaredErrorFunction                          Self;
  typedef ErrorFunctionBase<TMeasurementVector, ScalarType> Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;
  typedef typename Superclass::ErrorVectorType              ErrorVectorType;
  typedef typename Superclass::InternalVectorType           InternalVectorType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeanSquaredErrorFunction, FunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Evaluate at the specified Error position */
  virtual ScalarType Evaluate(const TMeasurementVector& Errors) const;

  virtual InternalVectorType EvaluateDerivative(const TMeasurementVector& Errors) const;

protected:

  MeanSquaredErrorFunction();
  virtual ~MeanSquaredErrorFunction();

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMeanSquaredErrorFunction.hxx"
#endif


#endif
