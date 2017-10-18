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
#ifndef itkSigmoidTransferFunction_h
#define itkSigmoidTransferFunction_h

#include "itkTransferFunctionBase.h"

namespace itk
{
namespace Statistics
{

/** \class SigmoidTransferFunction
 * \brief This is the itkSigmoidTransferFunction class.
 *
 * \ingroup ITKNeuralNetworks
 */
template<typename TScalar>
class ITK_TEMPLATE_EXPORT SigmoidTransferFunction : public TransferFunctionBase<TScalar>
{
public:

  /** Standard class typedefs. */
  typedef SigmoidTransferFunction          Self;
  typedef TransferFunctionBase<TScalar>    Superclass;
  typedef SmartPointer<Self>               Pointer;
  typedef SmartPointer<const Self>         ConstPointer;

  typedef TScalar                                        ScalarType;
  typedef typename NumericTraits< ScalarType >::RealType RealType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SigmoidTransferFunction, TransferFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set/Get macros */
  itkSetMacro(Alpha,RealType);
  itkGetMacro(Alpha,RealType);
  itkSetMacro(Beta,RealType);
  itkGetMacro(Beta,RealType);
  itkSetMacro(OutputMinimum,ScalarType);
  itkGetMacro(OutputMinimum,ScalarType);
  itkSetMacro(OutputMaximum,ScalarType);
  itkGetMacro(OutputMaximum,ScalarType);

  /** Evaluate at the specified input position */
  virtual ScalarType Evaluate(const ScalarType& input) const ITK_OVERRIDE;

  /** Evaluate the derivative at the specified input position */
  virtual ScalarType EvaluateDerivative(const ScalarType& input) const ITK_OVERRIDE;

protected:

  SigmoidTransferFunction();
  virtual ~SigmoidTransferFunction() ITK_OVERRIDE;

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

private:

  RealType   m_Alpha;
  RealType   m_Beta;
  ScalarType m_OutputMinimum;
  ScalarType m_OutputMaximum;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkSigmoidTransferFunction.hxx"
#endif

#endif
