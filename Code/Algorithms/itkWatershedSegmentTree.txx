/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedSegmentTree.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedSegmentTree_txx
#define __itkWatershedSegmentTree_txx

namespace itk
{
namespace watershed
{
//template <class TScalarType>
//void  SegmentTree<TScalarType>
//::PrintDeque()
//{
//  std::cout << "Deque has " << this->Size() << " entries. " << std::endl;
//  Iterator it = this->Begin();
//  while ( it != this->End() )
//    {
//      std::cout << (*it).from << " -> "
//                << (*it).to << " " << (*it).saliency
//                << std::endl;
//      ++it;
//    }
//}

template< class TScalarType >
void
SegmentTree< TScalarType >
::Initialize()
{
  //
  // We don't modify ourselves because the "ReleaseData" methods depend upon
  // no modification when initialized.
  //

  // Call the superclass method
  Superclass::Initialize();

  // Clear the segment tree
  this->Clear();
}

template< class TScalarType >
void
SegmentTree< TScalarType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace watershed
} // end namespace itk

#endif
