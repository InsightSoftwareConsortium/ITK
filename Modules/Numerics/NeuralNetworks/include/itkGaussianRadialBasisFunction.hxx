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
#ifndef itkGaussianRadialBasisFunction_hxx
#define itkGaussianRadialBasisFunction_hxx

#include "itkGaussianRadialBasisFunction.h"

#include <cmath>

namespace itk
{
namespace Statistics
{

/** Constructor */
template<typename ScalarType>
GaussianRadialBasisFunction< ScalarType>
::GaussianRadialBasisFunction()
{
}

/** Destructor */
template<typename ScalarType>
GaussianRadialBasisFunction< ScalarType>
::~GaussianRadialBasisFunction()
{
}

/** Evaluate function */
template<typename ScalarType>
ScalarType
GaussianRadialBasisFunction< ScalarType>
::Evaluate(const ScalarType& input) const
{
  ScalarType val;
  ScalarType radius = Superclass::GetRadius();
  val = std::exp(-0.5*std::pow(input,2)/std::pow(radius,2));
  return val;
}

/** Evaluate derivative function */
template<typename ScalarType>
ScalarType
GaussianRadialBasisFunction< ScalarType>
::EvaluateDerivative(const ScalarType& dist,const ArrayType& input,
                          char mode,int element_id) const
{
  ScalarType val = 0;
  ScalarType radius=Superclass::GetRadius();
  ArrayType center = Superclass::GetCenter();
  if(mode=='u') //w.r.t centers
    {
    ScalarType temp1= std::pow(radius,2);
    val=Evaluate(dist)
                      *(input.GetElement(element_id)-center.GetElement(element_id))/temp1;
    }
  else if(mode=='s') // w.r.t radius
    {
    val=Evaluate(dist)*std::pow(dist,2)/std::pow(radius,3);
    }
  return val;
}

/** Print the object */
template<typename ScalarType>
void
GaussianRadialBasisFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "GaussianRadialBasisFunction(" << this << ")" << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif
