/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnchorOpenCloseLine.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAnchorOpenCloseLine_txx
#define __itkAnchorOpenCloseLine_txx

#include "itkAnchorOpenCloseLine.h"

namespace itk
{
template< class TInputPix, class THistogramCompare, class TFunction1, class TFunction2 >
AnchorOpenCloseLine< TInputPix, THistogramCompare, TFunction1, TFunction2 >
::AnchorOpenCloseLine()
{
  m_Size = 2;
  if ( UseVectorBasedHistogram() )
    {
    m_Histo = new VHistogram;
    }
  else
    {
    m_Histo = new MHistogram;
    }
}

template< class TInputPix, class THistogramCompare, class TFunction1, class TFunction2 >
void
AnchorOpenCloseLine< TInputPix, THistogramCompare, TFunction1, TFunction2 >
::DoLine(InputImagePixelType *buffer, unsigned bufflength)
{
  // TFunction1 will be >= for openings
  // TFunction2 will be <=
  // TFunction3 will be >

  // the initial version will adopt the methodology of loading a line
  // at a time into a buffer vector, carrying out the opening or
  // closing, and then copy the result to the output. Hopefully this
  // will improve cache performance when working along non raster
  // directions.
  if ( bufflength <= m_Size / 2 )
    {
    // No point doing anything fancy - just look for the extreme value
    // This is important for angled structuring elements
    InputImagePixelType Extreme = buffer[0];
    for ( unsigned i = 0; i < bufflength; i++ )
      {
      if ( m_TF1(Extreme, buffer[i]) )
        {
        Extreme = buffer[i];
        }
      }

    for ( unsigned i = 0; i < bufflength; i++ )
      {
      buffer[i] = Extreme;
      }
    return;
    }

  m_Histo->Reset();
  // start the real work - everything here will be done with index
  // arithmetic rather than pointer arithmetic
  unsigned outLeftP = 0, outRightP = bufflength - 1;
  // left side
  assert(outLeftP >= 0);
  assert(outLeftP < bufflength);
  while ( ( outLeftP < outRightP ) && m_TF1(buffer[outLeftP], buffer[outLeftP + 1]) )
    {
    ++outLeftP;
    }
  while ( ( outLeftP < outRightP ) && m_TF2(buffer[outRightP - 1], buffer[outRightP]) )
    {
    --outRightP;
    }
  InputImagePixelType Extreme;
  while ( StartLine(buffer, Extreme, *m_Histo, outLeftP, outRightP, bufflength) )
      {}

  FinishLine(buffer, Extreme, outLeftP, outRightP, bufflength);
  // this section if to make the edge behaviour the same as the more
  // traditional approaches. It isn't part of the core anchor method.
  // Note that the index calculations include some extra factors that
  // relate to the padding at either end to allow users to set
  // borders.
  // compat
  // fix left border
  Extreme = buffer[m_Size / 2 + 1];
  for ( int i = m_Size / 2; i >= 0; i-- )
    {
    assert(i >= 0);
    if ( m_TF1(Extreme, buffer[i]) )
      {
      Extreme = buffer[i];
      }
//    std::cout << i << " " << (int)Extreme << " " << (int)buffer[i] <<
// std::endl;
    buffer[i] = Extreme;
    }
  // fix right border
  Extreme = buffer[bufflength - m_Size / 2 - 2];
  for ( int i = (int)bufflength - m_Size / 2 - 1; i < (int)bufflength; i++ )
    {
    assert(i < (int)bufflength);
    if ( m_TF1(Extreme, buffer[i]) )
      {
      Extreme = buffer[i];
      }
//    std::cout << (int)Extreme << " " << (int)buffer[i] << std::endl;
    buffer[i] = Extreme;
    }
}

template< class TInputPix, class THistogramCompare, class TFunction1, class TFunction2 >
bool
AnchorOpenCloseLine< TInputPix, THistogramCompare, TFunction1, TFunction2 >
::StartLine(InputImagePixelType *buffer,
            InputImagePixelType & Extreme,
            Histogram & histo,
            unsigned & outLeftP,
            unsigned & outRightP,
            unsigned
#ifndef NDEBUG
            bufflength
#endif
            )
{
  // This returns true to indicate return to startLine label in pseudo
  // code, and false to indicate finshLine
  Extreme = buffer[outLeftP];
  unsigned currentP = outLeftP + 1;
  unsigned sentinel, endP;

  while ( ( currentP < outRightP ) && m_TF2(buffer[currentP], Extreme) )
    {
    assert(currentP >= 0);
    assert(currentP < bufflength);
    Extreme = buffer[currentP];
    ++outLeftP;
    ++currentP;
    }

  sentinel = outLeftP + m_Size;
  if ( sentinel > outRightP )
    {
    // finish
    return ( false );
    }
  ++currentP;
  // ran m_Size pixels ahead
  while ( currentP < sentinel )
    {
    assert(currentP >= 0);
    assert(currentP < bufflength);
    if ( m_TF2(buffer[currentP], Extreme) )
      {
      endP = currentP;
#if 1
      for ( unsigned PP = outLeftP + 1; PP < endP; ++PP )
        {
        assert(PP >= 0);
        assert(PP < bufflength);
        buffer[PP] = Extreme;
        }
#else
      outLeftP++;
      while ( outLeftP < endP )
        {
        buffer[outLeftP] = Extreme; outLeftP++;
        }
#endif
      outLeftP = currentP;
      return ( true );
      }
    ++currentP;
    }
  // We didn't find a smaller (for opening) value in the segment of
  // reach of outLeftP. currentP is the first position outside the
  // reach of outLeftP
  assert(currentP >= 0);
  assert(currentP < bufflength);
  if ( m_TF2(buffer[currentP], Extreme) )
    {
    endP = currentP;
#if 1
    for ( unsigned PP = outLeftP + 1; PP < endP; ++PP )
      {
      assert(PP >= 0);
      assert(PP < bufflength);
      buffer[PP] = Extreme;
      }
#else
    outLeftP++;
    while ( outLeftP < endP )
      {
      buffer[outLeftP] = Extreme; outLeftP++;
      }

#endif
    outLeftP = currentP;
    return ( true );
    }
  else
    {
    // Now we need a histogram
    // Initialise it
    histo.Reset();
    outLeftP++;
    for ( unsigned aux = outLeftP; aux <= currentP; ++aux )
      {
      assert(aux >= 0);
      assert(aux < bufflength);
      histo.AddPixel(buffer[aux]);
      }
    // find the minimum value. The version
    // in the paper assumes integer pixel types and initializes the
    // search to the current extreme. Hopefully the latter is an
    // optimization step.
    Extreme = histo.GetValue();
    assert(outLeftP >= 0);
    assert(outLeftP < bufflength);
    histo.RemovePixel(buffer[outLeftP]);
    buffer[outLeftP] = Extreme;
    histo.AddPixel(Extreme);
    }

  while ( currentP < outRightP )
    {
    ++currentP;
    assert(currentP >= 0);
    assert(currentP < bufflength);
    if ( m_TF2(buffer[currentP], Extreme) )
      {
      // Found a new extrem
      endP = currentP;
#if 1
      for ( unsigned PP = outLeftP + 1; PP < endP; PP++ )
        {
        assert(PP >= 0);
        assert(PP < bufflength);
        buffer[PP] = Extreme;
        }
#else
      outLeftP++;
      while ( outLeftP < endP )
        {
        buffer[outLeftP] = Extreme; outLeftP++;
        }

#endif
      outLeftP = currentP;
      return ( true );
      }
    else
      {
      /* histogram update */
      assert(currentP >= 0);
      assert(currentP < bufflength);
      assert(outLeftP >= 0);
      assert(outLeftP < bufflength);
      histo.AddPixel(buffer[currentP]);
      histo.RemovePixel(buffer[outLeftP]);
      Extreme = histo.GetValue();
      ++outLeftP;
      assert(outLeftP >= 0);
      assert(outLeftP < bufflength);
      histo.RemovePixel(buffer[outLeftP]);
      buffer[outLeftP] = Extreme;
      histo.AddPixel(Extreme);
      }
    }
  // Finish the line
  while ( outLeftP < outRightP )
    {
    assert(outLeftP >= 0);
    assert(outLeftP < bufflength);
    histo.RemovePixel(buffer[outLeftP]);
    Extreme = histo.GetValue();
    ++outLeftP;
    assert(outLeftP >= 0);
    assert(outLeftP < bufflength);
    histo.RemovePixel(buffer[outLeftP]);
    buffer[outLeftP] = Extreme;
    histo.AddPixel(Extreme);
    }
  return ( false );
}

template< class TInputPix, class THistogramCompare, class TFunction1, class TFunction2 >
void
AnchorOpenCloseLine< TInputPix,  THistogramCompare, TFunction1, TFunction2 >
::FinishLine(InputImagePixelType *buffer,
             InputImagePixelType & Extreme,
             unsigned & outLeftP,
             unsigned & outRightP,
             unsigned
#ifndef NDEBUG
             bufflength
#endif
             )
{
  while ( outLeftP < outRightP )
    {
    assert(outLeftP >= 0);
    assert(outLeftP < bufflength);
    assert(outRightP >= 0);
    assert(outRightP < bufflength);
    if ( m_TF2(buffer[outLeftP], buffer[outRightP]) )
      {
      Extreme = buffer[outRightP];
      --outRightP;
      assert(outRightP >= 0);
      assert(outRightP < bufflength);
      if ( !m_TF2(buffer[outRightP], Extreme) )
        {
        buffer[outRightP] = Extreme;
        }
      }
    else
      {
      Extreme = buffer[outLeftP];
      ++outLeftP;
      assert(outLeftP >= 0);
      assert(outLeftP < bufflength);
      if ( !m_TF2(buffer[outLeftP], Extreme) )
        {
        buffer[outLeftP] = Extreme;
        }
      }
    }
}

template< class TInputPix, class THistogramCompare, class TFunction1, class TFunction2 >
void
AnchorOpenCloseLine< TInputPix, THistogramCompare, TFunction1, TFunction2 >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "Size: " << m_Size << std::endl;
}
} // end namespace itk

#endif
