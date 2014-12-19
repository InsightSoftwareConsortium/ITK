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
#ifndef itkCrossHelper_h
#define itkCrossHelper_h

#include "itkNumericTraits.h"

namespace itk
{
/** \class Cross
 * \brief Compute the cross product of two vectors of dimension 3,
 *        independently of the type of the values of vector's elements.
 *
 *  FIXME: Strictly speaking, the Cross product should not return a Vector, but
 *  a CovariantVector, since it behaves differently under Affine
 *  Transformations.
 *
 * \ingroup ITKCommon
 */
template< typename TVector >
class CrossHelper
{
public:
  typedef TVector                        VectorType;
  typedef typename VectorType::ValueType ValueType;

  itkStaticConstMacro (Dimension, unsigned int, VectorType::Dimension);

  /**
   * \param[in] iU
   * \param[in] iV
   * \return \f$ \boldsymbol{iU} \cdot \boldsymbol{iV} \f$
   */
  VectorType operator()(const VectorType & iU,
                        const VectorType & iV) const
  {
    VectorType oCross;

    if ( Dimension > 2 )
      {
      oCross[0] = iU[1] * iV[2] - iV[1] * iU[2];
      oCross[1] = iV[0] * iU[2] - iU[0] * iV[2];
      oCross[2] = iU[0] * iV[1] - iV[0] * iU[1];

      if (Dimension > 3)
        {
        for ( unsigned int dim = 3; dim < Dimension; dim++ )
          {
          oCross[dim] = 0.0;
          }
        }
      }
    else
      {
      oCross.Fill(0.);
      }

    return oCross;
  }
};
}

#endif
