/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnchorErodeDilateLine.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAnchorErodeDilateLine_txx
#define __itkAnchorErodeDilateLine_txx

#include "itkAnchorErodeDilateLine.h"

namespace itk
{
template< class TInputPix, class TFunction1, class TFunction2 >
AnchorErodeDilateLine< TInputPix, TFunction1, TFunction2 >
::AnchorErodeDilateLine()
{
  m_Size = 2;
  // create a histogram
  if ( UseVectorBasedHistogram() )
    {
    m_Histo = new VHistogram;
    }
  else
    {
    m_Histo = new MHistogram;
    }
}

template< class TInputPix, class TFunction1, class TFunction2 >
void
AnchorErodeDilateLine< TInputPix, TFunction1, TFunction2 >
::DoLine(InputImagePixelType *buffer, InputImagePixelType *inbuffer, unsigned bufflength)
{
  // TFunction1 will be < for erosions
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
      if ( m_TF1(Extreme, inbuffer[i]) )
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
  m_Histo->Reset();
  if ( bufflength <= m_Size )
    {
    // basically a standard histogram method
    // Left border, first half of structuring element
    Extreme = inbuffer[inLeftP];
    m_Histo->AddPixel(Extreme);
    for ( int i = 0; ( i < middle ); i++ )
      {
      ++inLeftP;
      assert(inLeftP >= 0);
      assert(inLeftP < (int)bufflength);
      m_Histo->AddPixel(inbuffer[inLeftP]);
      if ( m_TF1(inbuffer[inLeftP], Extreme) )
        {
        Extreme = inbuffer[inLeftP];
        }
      }
    assert(outLeftP >= 0);
    assert(outLeftP < (int)bufflength);
    buffer[outLeftP] = Extreme;

    // Second half of SE
    for ( int i = 0; i < (int)m_Size - middle - 1; i++ )
      {
      ++inLeftP;
      ++outLeftP;
      if ( inLeftP < (int)bufflength )
        {
        assert(inLeftP >= 0);
        assert(inLeftP < (int)bufflength);
        m_Histo->AddPixel(inbuffer[inLeftP]);
        if ( m_TF1(inbuffer[inLeftP], Extreme) )
          {
          Extreme = inbuffer[inLeftP];
          }
        }
      assert(outLeftP >= 0);
      assert(outLeftP < (int)bufflength);
      buffer[outLeftP] = Extreme;
      }
    // now finish
    outLeftP++;
    int left = 0;
    for (; outLeftP < (int)bufflength; outLeftP++, left++ )
      {
      m_Histo->RemovePixel(inbuffer[left]);
      Extreme = m_Histo->GetValue();
      assert(outLeftP >= 0);
      assert(outLeftP < (int)bufflength);
      buffer[outLeftP] = Extreme;
      }

    return;
    }

  // Left border, first half of structuring element
  Extreme = inbuffer[inLeftP];
  m_Histo->AddPixel(Extreme);
  for ( int i = 0; ( i < middle ); i++ )
    {
    ++inLeftP;
    assert(inLeftP >= 0);
    assert(inLeftP < (int)bufflength);
    m_Histo->AddPixel(inbuffer[inLeftP]);
    if ( m_TF1(inbuffer[inLeftP], Extreme) )
      {
      Extreme = inbuffer[inLeftP];
      }
    }
  assert(outLeftP >= 0);
  assert(outLeftP < (int)bufflength);
  buffer[outLeftP] = Extreme;

  // Second half of SE
  for ( int i = 0; i < (int)m_Size - middle - 1; i++ )
    {
    ++inLeftP;
    ++outLeftP;
    assert(inLeftP >= 0);
    assert(inLeftP < (int)bufflength);
    m_Histo->AddPixel(inbuffer[inLeftP]);
    if ( m_TF1(inbuffer[inLeftP], Extreme) )
      {
      Extreme = inbuffer[inLeftP];
      }
    assert(outLeftP >= 0);
    assert(outLeftP < (int)bufflength);
    buffer[outLeftP] = Extreme;
    }
  // Use the histogram until we find a new minimum
  assert(inLeftP >= 0);
  assert( ( inLeftP + 1 ) < (int)bufflength );
  while ( ( inLeftP < inRightP ) && m_TF2(Extreme, inbuffer[inLeftP + 1]) )
    {
    ++inLeftP;
    ++outLeftP;
    assert( ( inLeftP - (int)m_Size ) >= 0 );
    assert( ( inLeftP - (int)m_Size ) < (int)bufflength );

    m_Histo->RemovePixel(inbuffer[inLeftP - (int)m_Size]);
    assert(inLeftP >= 0);
    assert(inLeftP < (int)bufflength);
    m_Histo->AddPixel(inbuffer[inLeftP]);
    Extreme = m_Histo->GetValue();
    assert(outLeftP >= 0);
    assert(outLeftP < (int)bufflength);
    buffer[outLeftP] = Extreme;
    }
  assert(outLeftP > 0);
  assert(outLeftP < (int)bufflength);
  Extreme = buffer[outLeftP];

  while ( StartLine(buffer,
                    inbuffer,
                    Extreme,
                    *m_Histo,
                    outLeftP,
                    outRightP,
                    inLeftP,
                    inRightP,
                    middle,
                    bufflength) )
      {}

  FinishLine(buffer,
             inbuffer,
             Extreme,
             *m_Histo,
             outLeftP,
             outRightP,
             inLeftP,
             inRightP,
             middle,
             bufflength);
}

template< class TInputPix, class TFunction1, class TFunction2 >
bool
AnchorErodeDilateLine< TInputPix, TFunction1, TFunction2 >
::StartLine(InputImagePixelType *buffer,
            InputImagePixelType *inbuffer,
            InputImagePixelType & Extreme,
            Histogram & histo,
            int & outLeftP,
            int & itkNotUsed(outRightP),
            int & inLeftP,
            int & inRightP,
            int itkNotUsed(middle),
            unsigned
#ifndef NDEBUG
            bufflength
#endif
            )
{
  // This returns true to indicate return to startLine label in pseudo
  // code, and false to indicate finshLine
  int currentP = inLeftP + 1;
  int sentinel;

  assert(currentP >= 0);
  assert(inRightP <= (int)bufflength);
  while ( ( currentP < inRightP ) && m_TF2(inbuffer[currentP], Extreme) )
    {
    assert(currentP >= 0);
    assert(currentP < (int)bufflength);
    Extreme = inbuffer[currentP];
    ++outLeftP;
    assert(outLeftP >= 0);
    assert(outLeftP < (int)bufflength);
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
  assert(outLeftP >= 0);
  assert(outLeftP < (int)bufflength);
  buffer[outLeftP] = Extreme;

  // ran m_Size pixels ahead
  ++currentP;
  while ( currentP < sentinel )
    {
    assert(currentP >= 0);
    assert(currentP < (int)bufflength);
    if ( m_TF2(inbuffer[currentP], Extreme) )
      {
      Extreme = inbuffer[currentP];
      ++outLeftP;
      assert(outLeftP >= 0);
      assert(outLeftP < (int)bufflength);
      buffer[outLeftP] = Extreme;
      inLeftP = currentP;
      return ( true );
      }
    ++currentP;
    ++outLeftP;
    assert(outLeftP >= 0);
    assert(outLeftP < (int)bufflength);
    buffer[outLeftP] = Extreme;
    }
  // We didn't find a smaller (for erosion) value in the segment of
  // reach of inLeftP. currentP is the first position outside the
  // reach of inLeftP
  assert(currentP >= 0);
  assert(currentP < (int)bufflength);
  if ( m_TF2(inbuffer[currentP], Extreme) )
    {
    Extreme = inbuffer[currentP];
    ++outLeftP;
    assert(outLeftP >= 0);
    assert(outLeftP < (int)bufflength);
    buffer[outLeftP] = Extreme;
    inLeftP = currentP;
    return ( true );
    }
  else
    {
    // Now we need a histogram
    // Initialise it
    histo.Reset();
    ++outLeftP;
    ++inLeftP;
    for ( int aux = inLeftP; aux <= currentP; ++aux )
      {
      assert(aux >= 0);
      assert(aux < (int)bufflength);
      histo.AddPixel(inbuffer[aux]);
      }
    Extreme = histo.GetValue();
    assert(outLeftP >= 0);
    assert(outLeftP < (int)bufflength);
    buffer[outLeftP] = Extreme;
    }

  while ( currentP < inRightP )
    {
    ++currentP;
    assert(currentP >= 0);
    assert(currentP < (int)bufflength);
    if ( m_TF2(inbuffer[currentP], Extreme) )
      {
      // Found a new extrem
      Extreme = inbuffer[currentP];
      ++outLeftP;
      assert(outLeftP >= 0);
      assert(outLeftP < (int)bufflength);
      buffer[outLeftP] = Extreme;
      inLeftP = currentP;
      return ( true );
      }
    else
      {
      // update histogram
      assert(currentP >= 0);
      assert(currentP < (int)bufflength);
      histo.AddPixel(inbuffer[currentP]);
      assert(inLeftP >= 0);
      assert(inLeftP < (int)bufflength);
      histo.RemovePixel(inbuffer[inLeftP]);
      // find extreme
      Extreme = histo.GetValue();
      ++inLeftP;
      ++outLeftP;
      assert(outLeftP >= 0);
      assert(outLeftP < (int)bufflength);
      buffer[outLeftP] = Extreme;
      }
    }
  return ( false );
}

template< class TInputPix, class TFunction1, class TFunction2 >
void
AnchorErodeDilateLine< TInputPix, TFunction1, TFunction2 >
::FinishLine(InputImagePixelType *buffer,
             InputImagePixelType *inbuffer,
             InputImagePixelType & Extreme,
             Histogram & histo,
             int & outLeftP,
             int & outRightP,
             int & itkNotUsed(inLeftP),
             int & inRightP,
             int middle,
             unsigned
#ifndef NDEBUG
             bufflength
#endif
             )
{
  // Handles the right border.
  // First half of the structuring element
  histo.Reset();
  assert(inRightP >= 0);
  assert(inRightP < (int)bufflength);
  Extreme = inbuffer[inRightP];
  histo.AddPixel(Extreme);

  for ( int i = 0; i < middle; i++ )
    {
    --inRightP;
    assert(inRightP >= 0);
    assert(inRightP < (int)bufflength);
    histo.AddPixel(inbuffer[inRightP]);
    if ( m_TF1(inbuffer[inRightP], Extreme) )
      {
      Extreme = inbuffer[inRightP];
      }
    }
  assert(outRightP >= 0);
  assert(outRightP < (int)bufflength);
  buffer[outRightP] = Extreme;
  // second half of SE
  for ( int i = 0; ( i < (int)m_Size - middle - 1 ) && ( outLeftP < outRightP ); i++ )
    {
    --inRightP;
    --outRightP;
    assert(inRightP >= 0);
    assert(inRightP < (int)bufflength);
    histo.AddPixel(inbuffer[inRightP]);
    if ( m_TF1(inbuffer[inRightP], Extreme) )
      {
      Extreme = inbuffer[inRightP];
      }
    assert(outRightP >= 0);
    assert(outRightP < (int)bufflength);
    buffer[outRightP] = Extreme;
    }

  while ( outLeftP < outRightP )
    {
    --inRightP;
    --outRightP;
    assert( ( inRightP + (int)m_Size ) >= 0 );
    assert( ( inRightP + (int)m_Size ) < (int)bufflength );
    assert(inRightP >= 0);
    assert(inRightP < (int)bufflength);
    histo.RemovePixel(inbuffer[inRightP + (int)m_Size]);
    histo.AddPixel(inbuffer[inRightP]);
    if ( m_TF1(inbuffer[inRightP], Extreme) )
      {
      Extreme = inbuffer[inRightP];
      }
    Extreme = histo.GetValue();
    assert(outRightP >= 0);
    assert(outRightP < (int)bufflength);
    buffer[outRightP] = Extreme;
    }
}

template< class TInputPix, class TFunction1, class TFunction2 >
void
AnchorErodeDilateLine< TInputPix, TFunction1, TFunction2 >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "Size: " << m_Size << std::endl;
}
} // end namespace itk

#endif
