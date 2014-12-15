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
#ifndef itkAnchorErodeDilateLine_hxx
#define itkAnchorErodeDilateLine_hxx

#include "itkAnchorErodeDilateLine.h"

namespace itk
{
template< typename TInputPix, typename TCompare >
AnchorErodeDilateLine< TInputPix, TCompare >
::AnchorErodeDilateLine()
{
  m_Size = 2;
}

template< typename TInputPix, typename TCompare >
void
AnchorErodeDilateLine< TInputPix, TCompare >
::DoLine(std::vector<TInputPix> & buffer, std::vector<TInputPix> & inbuffer, unsigned bufflength)
{
  // TCompare will be < for erosions
  // TFunction2 will be <=

  // the initial version will adopt the methodology of loading a line
  // at a time into a buffer vector, carrying out the opening or
  // closing, and then copy the result to the output. Hopefully this
  // will improve cache performance when working along non raster
  // directions.
  if ( bufflength <= m_Size / 2 )
    {
    // No point doing anything fancy - just look for the extreme value
    // This is important when operating near the corner of images with
    // angled structuring elements
    InputImagePixelType Extreme = inbuffer[0];
    for ( unsigned i = 0; i < bufflength; i++ )
      {
      if ( StrictCompare(Extreme, inbuffer[i]) )
        {
        Extreme = inbuffer[i];
        }
      }

    for ( unsigned i = 0; i < bufflength; i++ )
      {
      buffer[i] = Extreme;
      }
    return;
    }

  int middle = (int)m_Size / 2;

  int                 outLeftP = 0, outRightP = (int)bufflength - 1;
  int                 inLeftP = 0, inRightP = (int)bufflength - 1;
  InputImagePixelType Extreme;
  HistogramType histo;
  if ( bufflength <= m_Size )
    {
    // basically a standard histogram method
    // Left border, first half of structuring element
    Extreme = inbuffer[inLeftP];
    histo.AddPixel(Extreme);
    for ( int i = 0; ( i < middle ); i++ )
      {
      ++inLeftP;
      histo.AddPixel(inbuffer[inLeftP]);
      if ( StrictCompare(inbuffer[inLeftP], Extreme) )
        {
        Extreme = inbuffer[inLeftP];
        }
      }
    buffer[outLeftP] = Extreme;

    // Second half of SE
    for ( int i = 0; i < (int)m_Size - middle - 1; i++ )
      {
      ++inLeftP;
      ++outLeftP;
      if ( inLeftP < (int)bufflength )
        {
        histo.AddPixel(inbuffer[inLeftP]);
        if ( StrictCompare(inbuffer[inLeftP], Extreme) )
          {
          Extreme = inbuffer[inLeftP];
          }
        }
      buffer[outLeftP] = Extreme;
      }
    // now finish
    outLeftP++;
    int left = 0;
    for (; outLeftP < (int)bufflength; outLeftP++, left++ )
      {
      histo.RemovePixel(inbuffer[left]);
      Extreme = histo.GetValue();
      buffer[outLeftP] = Extreme;
      }
    return;
    }

  // Left border, first half of structuring element
  Extreme = inbuffer[inLeftP];
  histo.AddPixel(Extreme);
  for ( int i = 0; ( i < middle ); i++ )
    {
    ++inLeftP;
    histo.AddPixel(inbuffer[inLeftP]);
    if ( StrictCompare(inbuffer[inLeftP], Extreme) )
      {
      Extreme = inbuffer[inLeftP];
      }
    }
  buffer[outLeftP] = Extreme;

  // Second half of SE
  for ( int i = 0; i < (int)m_Size - middle - 1; i++ )
    {
    ++inLeftP;
    ++outLeftP;
    histo.AddPixel(inbuffer[inLeftP]);
    if ( StrictCompare(inbuffer[inLeftP], Extreme) )
      {
      Extreme = inbuffer[inLeftP];
      }
    buffer[outLeftP] = Extreme;
    }
  // Use the histogram until we find a new minimum
  while ( ( inLeftP < inRightP ) && Compare(Extreme, inbuffer[inLeftP + 1]) )
    {
    ++inLeftP;
    ++outLeftP;

    histo.RemovePixel(inbuffer[inLeftP - (int)m_Size]);
    histo.AddPixel(inbuffer[inLeftP]);
    Extreme = histo.GetValue();
    buffer[outLeftP] = Extreme;
    }
  Extreme = buffer[outLeftP];

  while ( StartLine(buffer,
                    inbuffer,
                    Extreme,
                    outLeftP,
                    outRightP,
                    inLeftP,
                    inRightP,
                    middle) )
      {}

  FinishLine(buffer,
             inbuffer,
             Extreme,
             outLeftP,
             outRightP,
             inLeftP,
             inRightP,
             middle);
}

template< typename TInputPix, typename TCompare >
bool
AnchorErodeDilateLine< TInputPix, TCompare >
::StartLine(std::vector<TInputPix> & buffer,
            std::vector<TInputPix> & inbuffer,
            InputImagePixelType & Extreme,
            int & outLeftP,
            int & itkNotUsed(outRightP),
            int & inLeftP,
            int & inRightP,
            int itkNotUsed(middle)
            )
{
  // This returns true to indicate return to startLine label in pseudo
  // code, and false to indicate finshLine
  int currentP = inLeftP + 1;
  int sentinel;

  while ( ( currentP < inRightP ) && Compare(inbuffer[currentP], Extreme) )
    {
    Extreme = inbuffer[currentP];
    ++outLeftP;
    buffer[outLeftP] = Extreme;
    ++currentP;
    }
  inLeftP = currentP - 1;

  sentinel = inLeftP + (int)m_Size;
  if ( sentinel > inRightP )
    {
    // finish
    return ( false );
    }
  ++outLeftP;
  buffer[outLeftP] = Extreme;

  // ran m_Size pixels ahead
  ++currentP;
  while ( currentP < sentinel )
    {
    if ( Compare(inbuffer[currentP], Extreme) )
      {
      Extreme = inbuffer[currentP];
      ++outLeftP;
      buffer[outLeftP] = Extreme;
      inLeftP = currentP;
      return ( true );
      }
    ++currentP;
    ++outLeftP;
    buffer[outLeftP] = Extreme;
    }
  // We didn't find a smaller (for erosion) value in the segment of
  // reach of inLeftP. currentP is the first position outside the
  // reach of inLeftP
  HistogramType histo;
  if ( Compare(inbuffer[currentP], Extreme) )
    {
    Extreme = inbuffer[currentP];
    ++outLeftP;
    buffer[outLeftP] = Extreme;
    inLeftP = currentP;
    return ( true );
    }
  else
    {
    // Now we need a histogram
    // Initialise it
    ++outLeftP;
    ++inLeftP;
    for ( int aux = inLeftP; aux <= currentP; ++aux )
      {
      histo.AddPixel(inbuffer[aux]);
      }
    Extreme = histo.GetValue();
    buffer[outLeftP] = Extreme;
    }

  while ( currentP < inRightP )
    {
    ++currentP;
    if ( Compare(inbuffer[currentP], Extreme) )
      {
      // Found a new extrem
      Extreme = inbuffer[currentP];
      ++outLeftP;
      buffer[outLeftP] = Extreme;
      inLeftP = currentP;
      return ( true );
      }
    else
      {
      // update histogram
      histo.AddPixel(inbuffer[currentP]);
      histo.RemovePixel(inbuffer[inLeftP]);
      // find extreme
      Extreme = histo.GetValue();
      ++inLeftP;
      ++outLeftP;
      buffer[outLeftP] = Extreme;
      }
    }
  return ( false );
}

template< typename TInputPix, typename TCompare >
void
AnchorErodeDilateLine< TInputPix, TCompare >
::FinishLine(std::vector<TInputPix> & buffer,
             std::vector<TInputPix> & inbuffer,
             InputImagePixelType & Extreme,
             int & outLeftP,
             int & outRightP,
             int & itkNotUsed(inLeftP),
             int & inRightP,
             int middle
             )
{
  // Handles the right border.
  // First half of the structuring element
  HistogramType histo;
  Extreme = inbuffer[inRightP];
  histo.AddPixel(Extreme);

  for ( int i = 0; i < middle; i++ )
    {
    --inRightP;
    histo.AddPixel(inbuffer[inRightP]);
    if ( StrictCompare(inbuffer[inRightP], Extreme) )
      {
      Extreme = inbuffer[inRightP];
      }
    }
  buffer[outRightP] = Extreme;
  // second half of SE
  for ( int i = 0; ( i < (int)m_Size - middle - 1 ) && ( outLeftP < outRightP ); i++ )
    {
    --inRightP;
    --outRightP;
    histo.AddPixel(inbuffer[inRightP]);
    if ( StrictCompare(inbuffer[inRightP], Extreme) )
      {
      Extreme = inbuffer[inRightP];
      }
    buffer[outRightP] = Extreme;
    }

  while ( outLeftP < outRightP )
    {
    --inRightP;
    --outRightP;
    histo.RemovePixel(inbuffer[inRightP + (int)m_Size]);
    histo.AddPixel(inbuffer[inRightP]);
    if ( StrictCompare(inbuffer[inRightP], Extreme) )
      {
      Extreme = inbuffer[inRightP];
      }
    Extreme = histo.GetValue();
    buffer[outRightP] = Extreme;
    }
}

template< typename TInputPix, typename TCompare >
void
AnchorErodeDilateLine< TInputPix, TCompare >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "Size: " << m_Size << std::endl;
}
} // end namespace itk

#endif
