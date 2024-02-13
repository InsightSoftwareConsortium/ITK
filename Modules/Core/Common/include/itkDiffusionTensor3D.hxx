/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

#include "itkNumericTraits.h"

namespace itk
{

template <typename T>
DiffusionTensor3D<T>::DiffusionTensor3D(const Superclass & r)
  : SymmetricSecondRankTensor<T, 3>(r)
{}

template <typename T>
DiffusionTensor3D<T>::DiffusionTensor3D(const ComponentType & r)
  : SymmetricSecondRankTensor<T, 3>(r)
{}

template <typename T>
DiffusionTensor3D<T>::DiffusionTensor3D(const ComponentArrayType r)
  : SymmetricSecondRankTensor<T, 3>(r)
{}

template <typename T>
DiffusionTensor3D<T> &
DiffusionTensor3D<T>::operator=(const ComponentType & r)
{
  Superclass::operator=(r);
  return *this;
}

template <typename T>
DiffusionTensor3D<T> &
DiffusionTensor3D<T>::operator=(const ComponentArrayType r)
{
  Superclass::operator=(r);
  return *this;
}

template <typename T>
DiffusionTensor3D<T> &
DiffusionTensor3D<T>::operator=(const Superclass & r)
{
  Superclass::operator=(r);
  return *this;
}

template <typename T>
auto
DiffusionTensor3D<T>::GetTrace() const -> AccumulateValueType
{
  AccumulateValueType trace = (*this)[0];

  trace += (*this)[3];
  trace += (*this)[5];
  return trace;
}

template <typename T>
auto
DiffusionTensor3D<T>::GetFractionalAnisotropy() const -> RealValueType
{
  // Computed as
  // FA = std::sqrt(1.5*sum(sum(N.*N))/sum((sum(D.*D))))
  // where N = D - ((1/3)*trace(D)*eye(3,3))
  // equation (28) in
  // http://lmi.bwh.harvard.edu/papers/pdfs/2002/westinMEDIA02.pdf
  const RealValueType isp = this->GetInnerScalarProduct();

  if (isp > 0.0)
  {
    const RealValueType trace = this->GetTrace();
    const RealValueType anisotropy = 3.0 * isp - trace * trace;
    // sometimes anisotropy has been reported to be a small negative
    // number, and then std::sqrt returns NaN.  If it is a small
    // negative number, the obvious thing is to round to zero. If
    // it is a larger negative number, I'm not sure what the proper
    // result would be.  In either case, returning zero makes as much
    // sense in those cases as any other number.
    if (anisotropy > 0.0)
    {
      const auto fractionalAnisotropy = static_cast<RealValueType>(std::sqrt(anisotropy / (2.0 * isp)));
      return fractionalAnisotropy;
    }
  }

  return 0.0;
}

template <typename T>
auto
DiffusionTensor3D<T>::GetRelativeAnisotropy() const -> RealValueType
{
  const RealValueType trace = this->GetTrace();
  const RealValueType isp = this->GetInnerScalarProduct();

  // Avoid negative trace and traces small enough to look like a division by
  // zero.
  if (trace < NumericTraits<RealValueType>::min())
  {
    return RealValueType{};
  }

  const RealValueType anisotropy = 3.0 * isp - trace * trace;

  if (anisotropy < RealValueType{})
  {
    return RealValueType{};
  }

  const auto relativeAnisotropySquared = static_cast<RealValueType>(anisotropy / (std::sqrt(3.0) * trace));

  const auto relativeAnisotropy = static_cast<RealValueType>(std::sqrt(relativeAnisotropySquared));

  return relativeAnisotropy;
}

template <typename T>
auto
DiffusionTensor3D<T>::GetInnerScalarProduct() const -> RealValueType
{
  const RealValueType xx = (*this)[0];
  const RealValueType xy = (*this)[1];
  const RealValueType xz = (*this)[2];
  const RealValueType yy = (*this)[3];
  const RealValueType yz = (*this)[4];
  const RealValueType zz = (*this)[5];

  return (xx * xx + yy * yy + zz * zz + 2.0 * (xy * xy + xz * xz + yz * yz));
}
} // end namespace itk

#endif
