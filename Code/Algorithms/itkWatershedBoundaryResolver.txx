/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedBoundaryResolver.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedBoundaryResolver_txx
#define __itkWatershedBoundaryResolver_txx
#include "itkWatershedBoundaryResolver.h"

#include "itkImageRegionIterator.h"

namespace itk
{
namespace watershed
{

template <class TPixelType, unsigned int TDimension>
void BoundaryResolver<TPixelType, TDimension>
::GenerateData()
{
  //
  // NOTE: This method does not yet support arbitrary connectivity
  //       across the boundary.  It does support arbitrary dimensions.
  //       See itkWatershedSegmenter for details. City-block style connectivity
  //       is assumed (ie 4-, 6- for 2d, 3d). jc 11/14/01
  //
  typedef typename BoundaryType::face_t   FaceType;
  typename BoundaryType::IndexType idxA;
  typename BoundaryType::IndexType idxB;

  EquivalencyTableType::Pointer equivTable = this->GetEquivalencyTable();

  typename BoundaryType::Pointer boundaryA = this->GetBoundaryA();
  typename BoundaryType::Pointer boundaryB = this->GetBoundaryB();

  idxA.first = this->GetFace();
  idxB.first = this->GetFace();
  idxA.second = 1;  // HIGH face matches
  idxB.second = 0;  // the LOW face.

  // Initialize current minLabel
  // Initialize the current boundsMin
  // For each flat region frA in boundaryA:
  //  For each offset offA in frA:
  //   Lookup the flat region in boundaryB into which it flows: frB
  //     If frB.bounds_min < boundsMin then boundsMin =frB.bounds_min
  //      and minLabel = frB.minLabel
  //  Create equivalencies for all offA.labels, the frA label, and
  //  the minLabel

  //
  // For each boundary pixel in boundaryA then in boundary B:
  //  1) Does A flow to B? or B flow to A?
  //  2) If (1) then create equivalency to the label
  //     into which this pixel flows.
  //
  // This constructs equivalencies between both flat regions and
  // non-flat regions.
  //
  ImageRegionIterator<FaceType> itA(boundaryA->GetFace(idxA),
                               boundaryA->GetFace(idxA)->GetRequestedRegion());

  ImageRegionIterator<FaceType> itB(boundaryB->GetFace(idxB),
                               boundaryB->GetFace(idxB)->GetRequestedRegion());

  for (itA = itA.Begin(), itB = itB.Begin(); !itA.IsAtEnd(); ++itA, ++itB )
    {
      if ( itA.Get().flow != SegmenterType::NULL_FLOW )
        {
          equivTable->Add(itA.Get().label, itB.Get().label);
        }
      if ( itB.Get().flow != SegmenterType::NULL_FLOW )
        {
          equivTable->Add(itA.Get().label, itB.Get().label);
        }
    }

  equivTable->Flatten();
}
  
// ------------------------------------------------------------
// --------------------PIPELINE METHODS------------------------
// ------------------------------------------------------------
template <class TPixelType, unsigned int TDimension>
void BoundaryResolver<TPixelType, TDimension>
::GenerateOutputRequestedRegion(DataObject *output)
{
}

template<class TPixelType, unsigned int TDimension>
typename BoundaryResolver<TPixelType, TDimension>::DataObjectPointer
BoundaryResolver<TPixelType, TDimension>
::MakeOutput(unsigned int idx)
{
  return static_cast<DataObject*>(EquivalencyTable::New().GetPointer());
}

template<class TPixelType, unsigned int TDimension>
void 
BoundaryResolver<TPixelType, TDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


} // end namespace watershed
} // end namespace itk

#endif

