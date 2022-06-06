/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkInd2Sub_h
#define itkInd2Sub_h
#include <itkFixedArray.h>
#include <iostream>
#include <itkIndex.h>
#include <itkSize.h>

namespace itk
{
/**
 * @brief Return SubIndex [i,j,k,...] from linear index and matrix size.
 * Based on Ind2Sub from Matlab.
 * @note Indices range from 0 to ns - 1
 * Linear index cannot exceed the accumulated product of sizes, throwing runtime errors.
 *
 * @tparam N Dimension of the matrix
 * @param linear_index linear index, for example in 3D linear_index = i + nx(j + ny*k)
 * @param ns FixedArray containing sizes per dimension (nx,ny,...)
 *
 * @return FixedArray with subindexes: [i,j,k, ...
 */
template <unsigned int VN>
FixedArray<unsigned int, VN>
Ind2Sub(const unsigned int & linear_index, const FixedArray<unsigned int, VN> & ns)
{
  for (unsigned int d = 0; d < VN; ++d)
  {
    if (ns[d] == 0)
      throw std::runtime_error("itk::Ind2Sub: input size cannot be zero");
  }

  // accumulative product.
  FixedArray<unsigned int, VN> cumprod;
  unsigned int                 accum = 1;
  cumprod[0] = accum;
  for (unsigned int d = 1; d < VN; ++d)
  {
    accum *= ns[d - 1];
    cumprod[d] = accum;
  }
  unsigned int max_index = accum * ns[VN - 1] - 1;
  if (linear_index > max_index)
    throw std::runtime_error("itk::Ind2Sub: input index is incompatible with the given size");

  FixedArray<unsigned int, VN> out;
  unsigned int                 temp_index(linear_index);
  // loop well defined. it will go to > N after --0
  for (unsigned int i = VN - 1; i < VN; --i)
  {
    unsigned int rem = (temp_index) % cumprod[i];
    out[i] = (temp_index - rem) / cumprod[i];
    temp_index = rem;
  }

  return out;
}

/**
 * @brief Return SubIndex [i,j,k,...] from linear index and matrix size.
 * \sa Ind2Sub
 */
template <unsigned int VN>
itk::Index<VN>
Ind2Sub(const unsigned int & linear_index, const itk::Size<VN> & ns)
{
  FixedArray<unsigned int, VN> ns_array;
  for (unsigned int d = 0; d < VN; ++d)
  {
    ns_array[d] = ns[d];
  }
  FixedArray<unsigned int, VN> out_array = Ind2Sub<VN>(linear_index, ns_array);
  itk::Index<VN>               out_index;
  for (unsigned int d = 0; d < VN; ++d)
  {
    out_index[d] = out_array[d];
  }

  return out_index;
}
} // end namespace itk
#endif
