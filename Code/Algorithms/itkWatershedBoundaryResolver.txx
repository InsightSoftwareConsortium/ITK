/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedBoundaryResolver.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkWatershedBoundaryResolver_txx
#define __itkWatershedBoundaryResolver_txx

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
BoundaryResolver<TPixelType, TDimension>::DataObjectPointer
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

