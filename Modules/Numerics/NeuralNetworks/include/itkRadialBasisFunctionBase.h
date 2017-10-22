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
#ifndef itkRadialBasisFunctionBase_h
#define itkRadialBasisFunctionBase_h

#include "itkFunctionBase.h"
#include "itkArray.h"

namespace itk
{
namespace Statistics
{
/** \class RadialBasisFunctionBase
 * \brief This is the itkRadialBasisFunctionBase class.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename ScalarType>
class RadialBasisFunctionBase : public FunctionBase<ScalarType,ScalarType>
{
public:

  /** Standard class typedefs. */
  typedef RadialBasisFunctionBase             Self;
  typedef FunctionBase<ScalarType,ScalarType> Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RadialBasisFunctionBase, FunctionBase);

  /** Input/Output types */
  typedef Array<ScalarType> ArrayType;

  ///** Evaluate at the specified input position */
  virtual ScalarType Evaluate(const ScalarType& input) const ITK_OVERRIDE =0;

  /** Evaluate the derivative at the specified input position */
  virtual ScalarType EvaluateDerivative(const ScalarType& dist, const ArrayType& input,
                                                    char mode,int element_id=0) const = 0;

  itkSetMacro(Radius,ScalarType);
  itkGetConstMacro(Radius, ScalarType );

  itkSetMacro(Center,ArrayType);
  itkGetConstMacro(Center, ArrayType );

protected:

  RadialBasisFunctionBase()
    {
    m_Radius = 0;
    }
  ~RadialBasisFunctionBase() ITK_OVERRIDE {};

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE
    {
    os << indent << "RadialBasisFunctionBase(" << this << ")" << std::endl;
    Superclass::PrintSelf( os, indent );
    }

private:

  ArrayType  m_Center;
  ScalarType m_Radius;

  ITK_DISALLOW_COPY_AND_ASSIGN(RadialBasisFunctionBase);
};

} // end namespace Statistics
} // end namespace itk

#endif
