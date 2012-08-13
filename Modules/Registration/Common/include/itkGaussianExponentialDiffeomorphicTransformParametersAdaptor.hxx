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
#ifndef __itkGaussianExponentialDiffeomorphicTransformParametersAdaptor_hxx
#define __itkGaussianExponentialDiffeomorphicTransformParametersAdaptor_hxx

#include "itkGaussianExponentialDiffeomorphicTransformParametersAdaptor.h"

namespace itk
{

template<class TTransform>
GaussianExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::GaussianExponentialDiffeomorphicTransformParametersAdaptor() :
  m_GaussianSmoothingVarianceForTheVelocityField( 1.75 ),
  m_GaussianSmoothingVarianceForTheVelocityFieldSetTime( 0 )
{
}

template<class TTransform>
GaussianExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::~GaussianExponentialDiffeomorphicTransformParametersAdaptor()
{
}

template<class TTransform>
void
GaussianExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::SetGaussianSmoothingVarianceForTheVelocityField( ScalarType variance )
{
  this->m_GaussianSmoothingVarianceForTheVelocityFieldSetTime = this->GetMTime();
  if( this->m_GaussianSmoothingVarianceForTheVelocityField != variance )
    {
    itkDebugMacro( "Setting GaussianSmoothingVarianceForTheVelocityField to " << variance );
    this->m_GaussianSmoothingVarianceForTheVelocityField = variance;
    this->Modified();
    }
}

template<class TTransform>
void
GaussianExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::AdaptTransformParameters()
{
  Superclass::AdaptTransformParameters();

  if( this->m_GaussianSmoothingVarianceForTheVelocityFieldSetTime > 0 )
    {
    this->m_Transform->SetGaussianSmoothingVarianceForTheVelocityField(
      this->m_GaussianSmoothingVarianceForTheVelocityField );
    }
}


template <class TTransform>
void
GaussianExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  if( this->m_GaussianSmoothingVarianceForTheVelocityFieldSetTime > 0 )
    {
    os << indent << "Gaussian smoothing parameters: " << std::endl;
    if( this->m_GaussianSmoothingVarianceForTheVelocityFieldSetTime > 0 )
      {
      os << indent << "m_GaussianSmoothingVarianceForTheVelocityField: "
         << this->m_GaussianSmoothingVarianceForTheVelocityField
         << std::endl;
      }
    }
}

}  // namespace itk

#endif
