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
#ifndef itkSquaredDifferenceErrorFunction_h
#define itkSquaredDifferenceErrorFunction_h

#include "itkErrorFunctionBase.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"

namespace itk
{
namespace Statistics
{
/** \class SquaredDifferenceErrorFunction
 * \brief This is the itkSquaredDifferenceErrorFunction class.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename TMeasurementVector, typename ScalarType>
class ITK_TEMPLATE_EXPORT SquaredDifferenceErrorFunction : public ErrorFunctionBase<TMeasurementVector, ScalarType>
{
public:

  /** Standard class typedefs. */
  typedef SquaredDifferenceErrorFunction                    Self;
  typedef ErrorFunctionBase<TMeasurementVector, ScalarType> Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  typedef typename Superclass::ErrorVectorType    ErrorVectorType;
  typedef typename Superclass::InternalVectorType InternalVectorType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SquaredDifferenceErrorFunction, ErrorFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Evaluate at the specified Error position */
  virtual ScalarType Evaluate(const TMeasurementVector& Errors) const ITK_OVERRIDE;

  /** Evaluate derivatives */
  virtual InternalVectorType EvaluateDerivative(const TMeasurementVector& Errors) const ITK_OVERRIDE;

protected:

  SquaredDifferenceErrorFunction();
  virtual ~SquaredDifferenceErrorFunction() ITK_OVERRIDE;

  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SquaredDifferenceErrorFunction);

};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkSquaredDifferenceErrorFunction.hxx"
#endif


#endif
