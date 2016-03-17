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
#ifndef itkLabelObjectLineComparator_h
#define itkLabelObjectLineComparator_h

namespace itk
{
namespace Functor
{

  /** \class LabelObjectLineComparator
 *  \brief Performs a comparison of l1 < l2.  Returns true if l1 is strictly less
 *  than l2.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelObjectLine
 * \ingroup LabeledImageObject
 * \ingroup ITKLabelMap
 */
template< typename TLabelObjectLine >
class LabelObjectLineComparator
{
public:
  bool operator()(TLabelObjectLine const & l1, TLabelObjectLine const & l2) const
  {
    const typename TLabelObjectLine::IndexType & idx1 = l1.GetIndex();
    const typename TLabelObjectLine::IndexType & idx2 = l2.GetIndex();

    for ( int i = TLabelObjectLine::ImageDimension - 1; i >= 0; i-- )
      {
      if ( idx1[i] < idx2[i] )
        {
        return true;
        }
      else if ( idx1[i] > idx2[i] )
        {
        return false;
        }
      }
    return l1.GetLength() < l2.GetLength();
  }
};

} // end namespace Functor
} // end namespace itk

#endif
