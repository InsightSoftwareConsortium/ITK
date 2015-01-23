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
#ifndef itkDiffusionTensor3D_hxx
#define itkDiffusionTensor3D_hxx

#include "itkDiffusionTensor3D.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * Default Constructor
 */
template< typename T >
DiffusionTensor3D< T >
::DiffusionTensor3D()
{}

/**
 * Constructor with initialization
 */
template< typename T >
DiffusionTensor3D< T >
::DiffusionTensor3D(const Superclass & r):SymmetricSecondRankTensor< T, 3 >(r)
{}

/**
 * Constructor with initialization
 */
template< typename T >
DiffusionTensor3D< T >
::DiffusionTensor3D(const ComponentType & r):SymmetricSecondRankTensor< T, 3 >(r)
{}

/**
 * Constructor with initialization
 */
template< typename T >
DiffusionTensor3D< T >
::DiffusionTensor3D(const ComponentArrayType r):SymmetricSecondRankTensor< T, 3 >(r)
{}

/**
 * Assignment Operator
 */
template< typename T >
DiffusionTensor3D< T > &
DiffusionTensor3D< T >
::operator=(const ComponentType & r)
{
  Superclass::operator=(r);
  return *this;
}

/**
 * Assignment Operator
 */
template< typename T >
DiffusionTensor3D< T > &
DiffusionTensor3D< T >
::operator=(const ComponentArrayType r)
{
  Superclass::operator=(r);
  return *this;
}

/**
 * Assignment Operator
 */
template< typename T >
DiffusionTensor3D< T > &
DiffusionTensor3D< T >
::operator=(const Superclass & r)
{
  Superclass::operator=(r);
  return *this;
}

/**
 * Get the Trace, specialized version for 3D.
 *
 * Note that the indices are related to the fact
 * that we store only the upper-right triangle of
 * the matrix. Like
 *
 *       | 0  1  2  |
 *       | X  3  4  |
 *       | X  X  5  |
 *
 * The trace is therefore the sum of the components
 * M[0], M[3] and M[5].
 *
 */
template< typename T >
typename DiffusionTensor3D< T >::AccumulateValueType
DiffusionTensor3D< T >
::GetTrace() const
{
  AccumulateValueType trace = ( *this )[0];

  trace += ( *this )[3];
  trace += ( *this )[5];
  return trace;
}

/**
 *  Compute the value of fractional anisotropy
 */
template< typename T >
typename DiffusionTensor3D< T >::RealValueType
DiffusionTensor3D< T >
::GetFractionalAnisotropy() const
{
  // Computed as
  // FA = std::sqrt(1.5*sum(sum(N.*N))/sum((sum(D.*D))))
  // where N = D - ((1/3)*trace(D)*eye(3,3))
  // equation (28) in
  // http://lmi.bwh.harvard.edu/papers/pdfs/2002/westinMEDIA02.pdf
  const RealValueType isp   = this->GetInnerScalarProduct();

  if ( isp > 0.0 )
    {
    const RealValueType trace = this->GetTrace();
    const RealValueType anisotropy = 3.0 * isp - trace * trace;
    // sometimes anisotropy has been reported to be a small negative
    // number, and then std::sqrt returns NaN.  If it is a small
    // negative number, the obvious thing is to round to zero. If
    // it is a larger negative number, I'm not sure what the proper
    // result would be.  In either case, returning zero makes as much
    // sense in those cases as any other number.
    if(anisotropy > 0.0)
      {
      const RealValueType fractionalAnisotropy =
        static_cast< RealValueType >( std::sqrt( anisotropy / ( 2.0 * isp ) ) );
      return fractionalAnisotropy;
      }
    }

  return 0.0;
}

/**
 *  Compute the value of relative anisotropy
 */
template< typename T >
typename DiffusionTensor3D< T >::RealValueType
DiffusionTensor3D< T >
::GetRelativeAnisotropy() const
{
  const RealValueType trace = this->GetTrace();
  const RealValueType isp   = this->GetInnerScalarProduct();

  // Avoid negative trace and traces small enough to look like a division by
  // zero.
  if ( trace < NumericTraits< RealValueType >::min() )
    {
    return NumericTraits< RealValueType >::ZeroValue();
    }

  const RealValueType anisotropy = 3.0 * isp - trace * trace;

  if ( anisotropy  < NumericTraits< RealValueType >::ZeroValue() )
    {
    return NumericTraits< RealValueType >::ZeroValue();
    }

  const RealValueType relativeAnisotropySquared =
    static_cast< RealValueType >( anisotropy / ( std::sqrt(3.0) * trace ) );

  const RealValueType relativeAnisotropy =
    static_cast< RealValueType >( std::sqrt(relativeAnisotropySquared) );

  return relativeAnisotropy;
}

/**
 *  Compute the inner scalar product
 */
template< typename T >
typename DiffusionTensor3D< T >::RealValueType
DiffusionTensor3D< T >
::GetInnerScalarProduct() const
{
  const RealValueType xx = ( *this )[0];
  const RealValueType xy = ( *this )[1];
  const RealValueType xz = ( *this )[2];
  const RealValueType yy = ( *this )[3];
  const RealValueType yz = ( *this )[4];
  const RealValueType zz = ( *this )[5];

  return ( xx * xx + yy * yy + zz * zz + 2.0 * ( xy * xy + xz * xz + yz * yz ) );
}
} // end namespace itk

#endif
