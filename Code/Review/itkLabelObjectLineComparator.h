/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelObjectLineComparator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelObjectLineComparator_h
#define __itkLabelObjectLineComparator_h

namespace itk
{
/** \class LabelObjectLineComparator
 *  \brief Performs a comparison of l1 < l2.  Returns true if l1 is strictly less
 *  than l2.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelObjectLine
 * \ingroup LabeledImageObject
 */
namespace Functor
{
template< class TLabelObjectLine >
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
}
}

#endif
