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
#ifndef itkContourExtractor2DImageFilter_hxx
#define itkContourExtractor2DImageFilter_hxx

#include "itkConstShapedNeighborhoodIterator.h"
#include "itkProgressReporter.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkContourExtractor2DImageFilter.h"

namespace itk
{
// Constructor
template< typename TInputImage >
ContourExtractor2DImageFilter< TInputImage >
::ContourExtractor2DImageFilter()
{
  this->m_ContourValue = NumericTraits< InputRealType >::ZeroValue();
  this->m_ReverseContourOrientation = false;
  this->m_VertexConnectHighPixels = false;
  this->m_UseCustomRegion = false;
  this->m_NumberOfContoursCreated = 0;
}

// Destructor
template< typename TInputImage >
ContourExtractor2DImageFilter< TInputImage >
::~ContourExtractor2DImageFilter()
{}

template< typename TInputImage >
void
ContourExtractor2DImageFilter< TInputImage >
::GenerateData()
{
  // Make sure the structures for containing, looking up, and numbering the
  // growing contours are empty and ready.
  m_Contours.clear();
  m_ContourStarts.clear();
  m_ContourEnds.clear();
  m_NumberOfContoursCreated = 0;

  // Set up an iterator to "march the squares" across the image.
  // We associate each 2px-by-2px square with the pixel in the upper left of
  // that square. We then iterate across the image, examining these 2x2 squares
  // and building the contour. By iterating the upper-left pixel of our
  // "current square" across every pixel in the image except those on the
  // bottom row and rightmost column, we have visited every valid square in the
  // image.

  InputRegionType region = this->GetInput()->GetRequestedRegion();
  typename InputRegionType::SizeType shrunkSize = region.GetSize();
  shrunkSize[0] -= 1;
  shrunkSize[1] -= 1;
  InputRegionType shrunkRegion(region.GetIndex(), shrunkSize);

  // Set up a progress reporter
  ProgressReporter progress( this, 0, shrunkRegion.GetNumberOfPixels() );

  // A 1-pixel radius sets up a neighborhood with the following indices:
  // 0 1 2
  // 3 4 5
  // 6 7 8
  // We are interested only in the square of 4,5,7,8 which is the 2x2 square
  // with the center pixel at the top-left. So we only activate the
  // coresponding offsets, and only query pixels 4, 5, 7, and 8 with the
  // iterator's GetPixel method.
  typedef ConstShapedNeighborhoodIterator< InputImageType > SquareIterator;
  typename SquareIterator::RadiusType radius = { { 1, 1 } };
  SquareIterator  it(radius, this->GetInput(), shrunkRegion);
  InputOffsetType none  = { { 0, 0 } };
  InputOffsetType right = { { 1, 0 } };
  InputOffsetType down  = { { 0, 1 } };
  InputOffsetType diag  = { { 1, 1 } };
  it.ActivateOffset(none);
  it.ActivateOffset(right);
  it.ActivateOffset(down);
  it.ActivateOffset(diag);

  for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    // There are sixteen different possible square types, diagramed below.
    // A + indicates that the vertex is above the contour value, and a -
    // indicates that the vertex is below or equal to the contour value.
    // The vertices of each square are here numbered:
    // 01
    // 23
    // and treated as a binary value with the bits in that order. Thus each
    // square can be so numbered:
    //  0--   1+-   2-+   3++   4--   5+-   6-+   7++
    //   --    --    --    --    +-    +-    +-    +-
    //
    //  8--   9+-  10-+  11++  12--  13+-  14-+  15++
    //   -+    -+    -+    -+    ++    ++    ++    ++
    //
    // The position of the line segment that cuts through (or doesn't, in case
    // 0 and 15) each square is clear, except in cases  6 and 9. In this case,
    // where the segments are placed is determined by
    // m_VertexConnectHighPixels. If m_VertexConnectHighPixels is false, then
    // lines like are drawn through square 6, and lines like are drawn through
    // square 9. Otherwise, the situation is reversed.
    // Finally, recall that we draw the lines so that (moving from tail to
    // head) the lower-valued pixels are on the left of the line. So, for
    // example, case 1 entails a line slanting from the middle of the top of
    // the square to the middle of the left side of the square.

    // (1) Determine what number square we are currently inspecting. Remember
    // that as far as the neighborhood iterator is concerned, our square
    // 01    is numbered as    45
    // 23                      78

    InputPixelType v0, v1, v2, v3;
    v0 = it.GetPixel(4);
    v1 = it.GetPixel(5);
    v2 = it.GetPixel(7);
    v3 = it.GetPixel(8);
    InputIndexType index = it.GetIndex();
    unsigned char  squareCase = 0;
    if ( v0 > m_ContourValue ) { squareCase += 1; }
    if ( v1 > m_ContourValue ) { squareCase += 2; }
    if ( v2 > m_ContourValue ) { squareCase += 4; }
    if ( v3 > m_ContourValue ) { squareCase += 8; }

    // Set up macros to find the ContinuousIndex where the contour intersects
    // one of the sides of the square.  Normally macros should, of course, be
    // eschewed, but since this is an inner loop not calling the function four
    // times when two would do is probably worth while. Plus, copy-pasting
    // these into the switch below is even worse.  InterpolateContourPosition
    // takes the values at two vertices, the index of the first vertex, and the
    // offset between the two vertices.
    #define TOP_     this->InterpolateContourPosition(v0, v1, index, right)
    #define BOTTOM_  this->InterpolateContourPosition(v2, v3, index + down, right)
    #define LEFT_    this->InterpolateContourPosition(v0, v2, index,       down)
    #define RIGHT_   this->InterpolateContourPosition(v1, v3, index + right, down)

    // (2) Add line segments to the growing contours as defined by the cases.
    // AddSegment takes a "from" vertex and a "to" vertex, and adds it to the
    // a growing contour, creates a new contour, or merges two together.
    switch ( squareCase )
      {
      case 0: // no line
        break;
      case 1:  // top to left
        this->AddSegment(TOP_, LEFT_);
        break;
      case 2: // right to top
        this->AddSegment(RIGHT_, TOP_);
        break;
      case 3: // right to left
        this->AddSegment(RIGHT_, LEFT_);
        break;
      case 4: // left to bottom
        this->AddSegment(LEFT_, BOTTOM_);
        break;
      case 5: // top to bottom
        this->AddSegment(TOP_, BOTTOM_);
        break;
      case 6:
        if ( m_VertexConnectHighPixels )
          {
          // left to top
          this->AddSegment(LEFT_, TOP_);
          // right to bottom
          this->AddSegment(RIGHT_, BOTTOM_);
          }
        else
          {
          // right to top
          this->AddSegment(RIGHT_, TOP_);
          // left to bottom
          this->AddSegment(LEFT_, BOTTOM_);
          }
        break;
      case 7: // right to bottom
        this->AddSegment(RIGHT_, BOTTOM_);
        break;
      case 8: // bottom to right
        this->AddSegment(BOTTOM_, RIGHT_);
        break;
      case 9:
        if ( m_VertexConnectHighPixels )
          {
          // top to right
          this->AddSegment(TOP_, RIGHT_);
          // bottom to left
          this->AddSegment(BOTTOM_, LEFT_);
          }
        else
          {
          // top to left
          this->AddSegment(TOP_, LEFT_);
          // bottom to right
          this->AddSegment(BOTTOM_, RIGHT_);
          }
        break;
      case 10: // bottom to top
        this->AddSegment(BOTTOM_, TOP_);
        break;
      case 11: // bottom to left
        this->AddSegment(BOTTOM_, LEFT_);
        break;
      case 12: // left to right
        this->AddSegment(LEFT_, RIGHT_);
        break;
      case 13: // top to right
        this->AddSegment(TOP_, RIGHT_);
        break;
      case 14: // left to top
        this->AddSegment(LEFT_, TOP_);
        break;
      case 15: // no line
        break;
      } // switch squareCase
    progress.CompletedPixel();
    } // iteration

  // Now create the outputs paths from the deques we've been using.
  this->FillOutputs();
  m_Contours.clear();
  m_ContourStarts.clear();
  m_ContourEnds.clear();
  m_NumberOfContoursCreated = 0;
}

template< typename TInputImage >
inline
typename ContourExtractor2DImageFilter< TInputImage >::VertexType
ContourExtractor2DImageFilter< TInputImage >
::InterpolateContourPosition(InputPixelType fromValue, InputPixelType toValue,
                             InputIndexType fromIndex, InputOffsetType toOffset)
{
  VertexType output;

  // Now calculate the fraction of the way from 'from' to 'to' that the contour
  // crosses. Interpolate linearly: y = v0 + (v1 - v0) * x, and solve for the
  // x that gives y = m_ContourValue: x = (m_ContourValue - v0) / (v1 - v0).
  // This assumes that v0 and v1 are separated by exactly ONE unit. So the to
  // Offset. value must have exactly one component 1 and the other component 0.
  // Also this assumes that fromValue and toValue are different. Otherwise we
  // can't interpolate anything!
  itkAssertOrThrowMacro( ( fromValue != toValue ), "source and destination are the same" );

  itkAssertOrThrowMacro(
    ( ( toOffset[0] == 0 && toOffset[1] == 1 ) || ( toOffset[0] == 1 && toOffset[1] == 0 ) ),
    "toOffset has unexpected values");

  double x = ( m_ContourValue - static_cast< InputRealType >( fromValue ) )
             / ( toValue - static_cast< InputRealType >( fromValue ) );

  output[0] = fromIndex[0] + x * toOffset[0];
  output[1] = fromIndex[1] + x * toOffset[1];

  return output;
}

template< typename TInputImage >
void
ContourExtractor2DImageFilter< TInputImage >
::AddSegment(VertexType from, VertexType to)
{
  if ( from == to )
    {
    // Arc is degenerate: ignore, and the from/two point will be connected
    // later by other squares. Degeneracy happens when (and only when) a square
    // has exactly one vertex that is the contour value, and the rest are above
    // that value.
    return;
    }

  // Try to find an existing contour that starts where the new segment ends.
  VertexMapIterator newTail = m_ContourStarts.find(to);
  // Try to find an existing contour that ends where the new segment starts.
  VertexMapIterator newHead = m_ContourEnds.find(from);

  if ( newTail != m_ContourStarts.end() && newHead != m_ContourEnds.end() )
    {
    // We need to connect these two contours. The act of connecting them will
    // add the needed arc.
    ContourRef tail = newTail->second;
    itkAssertOrThrowMacro( ( tail->front() == to ), "End doesn't match Beginning" );
    ContourRef head = newHead->second;
    itkAssertOrThrowMacro( ( head->back() == from ), "Beginning doesn't match End" );
    if ( head == tail )
      {
      // We've closed a contour. Add the end point, and remove from the maps
      head->push_back(to);
      m_ContourStarts.erase(newTail);
      // erase the front of tail. Because head and tail are the same contour,
      // don't worry about erasing the front of head!
      m_ContourEnds.erase(newHead); // erase the end of head/tail.
      }
    else
      {
      // We have found two distinct contours that need to be joined.  Careful
      // here: we want to keep the first segment in the list when merging so
      // that contours are always returned in top-to-bottom, right-to-left
      // order (with regard to the image pixel found to be inside the contour).
      if ( tail->m_ContourNumber > head->m_ContourNumber )
        {
        // if tail was created later than head...
        // Copy tail to the end of head and remove
        // tail from everything.
        head->insert( head->end(), tail->begin(), tail->end() );

        // Now remove 'tail' from the list and the maps because it has been
        // subsumed.
        m_ContourStarts.erase(newTail);
        int erased = m_ContourEnds.erase( tail->back() );
        // There should be exactly one entry in the hash for that endpoint
        if ( erased != 1 )
          {
          itkWarningMacro (
            << "There should be exactly one entry in the hash for that endpoint, but there are " << erased);
          }
        m_Contours.erase(tail); // remove from the master list

        // Now remove the old end of 'head' from the ends map and add
        // the new end.
        m_ContourEnds.erase(newHead);
        m_ContourEnds.insert( VertexContourRefPair(head->back(), head) );
        }
      else
        {
        // Copy head to the beginning of tail and remove
        // head from everything.
        tail->insert( tail->begin(), head->begin(), head->end() );

        // Now remove 'head' from the list and the maps because
        // it has been subsumed.
        m_ContourEnds.erase(newHead);
        int erased = m_ContourStarts.erase( head->front() );
        if ( erased != 1 )
          {
          itkWarningMacro (
            << "There should be exactly one entry in the hash for that endpoint, but there are " << erased);
          }
        m_Contours.erase(head); // remove from the master list

        // Now remove the old start of 'tail' from the starts map and
        // add the new start.
        m_ContourStarts.erase(newTail);
        m_ContourStarts.insert( VertexContourRefPair(tail->front(), tail) );
        }
      }
    }
  else if ( newTail == m_ContourStarts.end() && newHead == m_ContourEnds.end() )
    {
    // No contours found: add a new one.
    // Make it on the heap. It will be copied into m_Contours.
    ContourType contour;

    // Add the endpoints
    contour.push_front(from);
    contour.push_back(to);
    contour.m_ContourNumber = m_NumberOfContoursCreated++;
    // Add the contour to the end of the list and get a reference to it.
    m_Contours.push_back(contour);

    // recall that end() is an iterator to one past the back!
    ContourRef newContour = --m_Contours.end();
    // add the endpoints and an iterator pointing to the contour
    // in the list to the maps.
    m_ContourStarts.insert( VertexContourRefPair(from, newContour) );
    m_ContourEnds.insert( VertexContourRefPair(to, newContour) );
    }
  else if ( newTail != m_ContourStarts.end() && newHead == m_ContourEnds.end() )
    {
    // Found a single contour to which the new arc should be prepended.
    ContourRef tail = newTail->second;
    itkAssertOrThrowMacro( ( tail->front() == to ), "End doesn't match Beginning" );
    tail->push_front(from);
    // erase the old start of this contour
    m_ContourStarts.erase(newTail);
    // Now add the new start of this contour.
    m_ContourStarts.insert( VertexContourRefPair(from, tail) );
    }
  else if ( newTail == m_ContourStarts.end() && newHead != m_ContourEnds.end() )
    {
    // Found a single contour to which the new arc should be appended.
    ContourRef head = newHead->second;
    itkAssertOrThrowMacro( ( head->back() == from ), "Beginning doesn't match End" );
    head->push_back(to);
    // erase the old end of this contour
    m_ContourEnds.erase(newHead);
    // Now add the new start of this contour.
    m_ContourEnds.insert( VertexContourRefPair(to, head) );
    }
}

template< typename TInputImage >
void
ContourExtractor2DImageFilter< TInputImage >
::FillOutputs()
{
  this->SetNumberOfIndexedOutputs( m_Contours.size() );
  int i = 0;
  for ( ContourRef it = m_Contours.begin(); it != m_Contours.end(); it++, i++ )
    {
    OutputPathPointer output = this->GetOutput(i);
    if ( output.IsNull() )
      {
      // Static cast is OK because we know PathSource will make its templated
      // class type
      output = static_cast< OutputPathType * >( this->MakeOutput(i).GetPointer() );
      this->SetNthOutput( i, output.GetPointer() );
      }
    typename VertexListType::Pointer path =
      const_cast< VertexListType * >( output->GetVertexList() );
    path->Initialize();
    path->reserve( it->size() ); // use std::vector version of 'reserve()'
    //instead of VectorContainer::Reserve() to work around
    // the fact that the latter is essentially std::vector::resize(),
    // which is not what we want.

    // Now put all the points from the contour deque into the path and
    // mark output as modified

    typedef typename ContourType::const_iterator ConstIteratorType;
    if ( m_ReverseContourOrientation )
      {
      ConstIteratorType itC = ( *it ).end();
      do
        {
        itC--;
        path->push_back(*itC);
        }
      while ( itC != ( *it ).begin() );
      }
    else
      {
      ConstIteratorType itC = ( *it ).begin();
      while ( itC != ( *it ).end() )
        {
        path->push_back(*itC);
        itC++;
        }
      }
    output->Modified();
    }
}

template< typename TInputImage >
void
ContourExtractor2DImageFilter< TInputImage >
::SetRequestedRegion(const InputRegionType region)
{
  itkDebugMacro("setting RequestedRegion to " << region);
  m_UseCustomRegion = true;
  if ( this->m_RequestedRegion != region )
    {
    this->m_RequestedRegion = region;
    this->Modified();
    }
}

template< typename TInputImage >
void
ContourExtractor2DImageFilter< TInputImage >
::ClearRequestedRegion()
{
  itkDebugMacro("Clearing RequestedRegion.");
  if ( this->m_UseCustomRegion == true )
    {
    this->m_UseCustomRegion = false;
    this->Modified();
    }
}

template< typename TInputImage >
void
ContourExtractor2DImageFilter< TInputImage >
::GenerateInputRequestedRegion()
{
  InputImageType *input = const_cast< InputImageType * >( this->GetInput() );

  if ( !input ) { return; }

  if ( m_UseCustomRegion )
    {
    InputRegionType requestedRegion = m_RequestedRegion;
    if ( requestedRegion.Crop( input->GetLargestPossibleRegion() ) )
      {
      input->SetRequestedRegion(requestedRegion);
      return;
      }
    else
      {
      // Couldn't crop the region (requested region is outside the largest
      // possible region).  Throw an exception.

      // store what we tried to request (prior to trying to crop)
      input->SetRequestedRegion(requestedRegion);

      // build an exception
      InvalidRequestedRegionError e(__FILE__, __LINE__);
      e.SetLocation(ITK_LOCATION);
      e.SetDescription(
        "Requested region is outside the largest possible region.");
      e.SetDataObject(input);
      throw e;
      }
    }
  else
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}

/**
 * Standard "PrintSelf" method
 */
template< typename TInputImage >
void
ContourExtractor2DImageFilter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ReverseContourOrientation: " << m_ReverseContourOrientation
     << std::endl;
  os << indent << "VertexConnectHighPixels: " << m_VertexConnectHighPixels
     << std::endl;
  os << indent << "UseCustomRegion: " << m_UseCustomRegion << std::endl;
  os << indent << "NumericTraits: " << m_UseCustomRegion << std::endl;
  os << indent << "NumberOfContoursCreated: " << m_NumberOfContoursCreated
     << std::endl;
  if ( m_UseCustomRegion )
    {
    os << indent << "Custom region: " << m_RequestedRegion << std::endl;
    }

  typedef typename NumericTraits< InputRealType >::PrintType InputRealPrintType;

  os << indent << "Contour value: "
     << static_cast< InputRealPrintType >( m_ContourValue )
     << std::endl;
}
} // end namespace itk

#endif
