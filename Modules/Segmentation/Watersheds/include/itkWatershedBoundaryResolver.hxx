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
#ifndef itkWatershedBoundaryResolver_hxx
#define itkWatershedBoundaryResolver_hxx
#include "itkWatershedBoundaryResolver.h"

#include "itkImageRegionIterator.h"

namespace itk
{
namespace watershed
{
template< typename TPixelType, unsigned int TDimension >
void BoundaryResolver< TPixelType, TDimension >
::GenerateData()
{
  //
  // NOTE: This method does not yet support arbitrary connectivity
  //       across the boundary.  It does support arbitrary dimensions.
  //       See itkWatershedSegmenter for details. City-block style connectivity
  //       is assumed (ie 4-, 6- for 2d, 3d). jc 11/14/01
  //
  typedef typename BoundaryType::face_t FaceType;
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
  ImageRegionIterator< FaceType > itA( boundaryA->GetFace(idxA),
                                       boundaryA->GetFace(idxA)->GetRequestedRegion() );

  ImageRegionIterator< FaceType > itB( boundaryB->GetFace(idxB),
                                       boundaryB->GetFace(idxB)->GetRequestedRegion() );

  for ( itA.GoToBegin(), itB.GoToBegin(); !itA.IsAtEnd(); ++itA, ++itB )
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
template< typename TPixelType, unsigned int TDimension >
void BoundaryResolver< TPixelType, TDimension >
::GenerateOutputRequestedRegion(DataObject *)
{}

template< typename TPixelType, unsigned int TDimension >
typename BoundaryResolver< TPixelType, TDimension >::DataObjectPointer
BoundaryResolver< TPixelType, TDimension >
::MakeOutput(DataObjectPointerArraySizeType)
{
  return EquivalencyTable::New().GetPointer();
}

template< typename TPixelType, unsigned int TDimension >
void
BoundaryResolver< TPixelType, TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Face = " << m_Face << std::endl;
}
} // end namespace watershed
} // end namespace itk

#endif
