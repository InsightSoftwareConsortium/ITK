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
#ifndef itkVnlFFTCommon_hxx
#define itkVnlFFTCommon_hxx

#include "itkVnlFFTCommon.h"

namespace itk
{

template< typename TSizeValue >
bool
VnlFFTCommon
::IsDimensionSizeLegal(TSizeValue n)
{
  int ifac = 2;

  for ( int l = 1; l <= 3; l++ )
    {
    for (; n % ifac == 0; )
      {
      n /= ifac;
      }
    ifac += l;
    }
  return ( n == 1 ); // return false if decomposition failed
}

template< typename TImage >
VnlFFTCommon::VnlFFTTransform< TImage >
::VnlFFTTransform(const typename TImage::SizeType & s)
{
  for( unsigned int i=0; i < TImage::ImageDimension; i++ )
    {
    Base::factors_[TImage::ImageDimension - i - 1].resize(s[i]);
    }
}

} // end namespace itk

#endif // itkVnlFFTCommon_hxx
