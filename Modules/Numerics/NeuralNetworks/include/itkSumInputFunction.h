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
#ifndef itkSumInputFunction_h
#define itkSumInputFunction_h

#include "itkInputFunctionBase.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"

namespace itk
{
namespace Statistics
{
/** \class SumInputFunction
 * \brief This is the itkSumInputFunction class.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename TMeasurementVector, typename ScalarType>
class ITK_TEMPLATE_EXPORT SumInputFunction : public InputFunctionBase<TMeasurementVector, ScalarType>
{
public:

  /** Standard class typedefs. */
  typedef SumInputFunction                                  Self;
  typedef InputFunctionBase<TMeasurementVector, ScalarType> Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SumInputFunction, FunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Evaluate at the specified input position */
  virtual ScalarType Evaluate(const TMeasurementVector& input) const ITK_OVERRIDE;

  virtual void SetSize(unsigned int n) ITK_OVERRIDE;

protected:

  SumInputFunction();
  virtual ~SumInputFunction() ITK_OVERRIDE;

  unsigned int m_Size;

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkSumInputFunction.hxx"
#endif

#endif
