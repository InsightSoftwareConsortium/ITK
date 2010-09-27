/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmLookupTable.h"
#include <vector>
#include <string.h>

namespace gdcm
{

class LookupTableInternal
{
public:
  LookupTableInternal():RGB()
  {
    Length[0] = Length[1] = Length[2] = 0;
    Subscript[0] = Subscript[1] = Subscript[2] = 0;
    BitSize[0] = BitSize[1] = BitSize[2] = 0;
  }
  unsigned int Length[3]; // In DICOM the length is specified on a short
  // but 65536 is expressed as 0 ...
  unsigned short Subscript[3];
  unsigned short BitSize[3];
  std::vector<unsigned char> RGB;
};

LookupTable::LookupTable()
{
  Internal = new LookupTableInternal;
  BitSample = 0;
  IncompleteLUT = false;
}

LookupTable::~LookupTable()
{
  delete Internal;
}

bool LookupTable::Initialized() const
{
  bool b1 = BitSample != 0;
  bool b2 =
    Internal->BitSize[0] != 0 &&
    Internal->BitSize[1] != 0 &&
    Internal->BitSize[2] != 0;
  return b1 && b2;
}

void LookupTable::Clear()
{
  BitSample = 0;
  IncompleteLUT = false;
  delete Internal;
  Internal = new LookupTableInternal;
}

void LookupTable::Allocate( unsigned short bitsample )
{
  if( bitsample == 8 )
    {
    Internal->RGB.resize( 256 * 3 );
    }
  else if ( bitsample == 16 )
    {
    Internal->RGB.resize( 65536 * 2 * 3 );
    }
  else
    {
    gdcmAssertAlwaysMacro(0);
    }
  BitSample = bitsample;
}

void LookupTable::InitializeLUT(LookupTableType type, unsigned short length,
  unsigned short subscript, unsigned short bitsize)
{
  if( bitsize != 8 && bitsize != 16 )
    {
    return;
    }
  assert( type >= RED && type <= BLUE );
  assert( subscript == 0 );
  assert( bitsize == 8 || bitsize == 16 );
  if( length == 0 )
    {
    Internal->Length[type] = 65536;
    }
  else
    {
    if( length != 256 )
      {
      IncompleteLUT = true;
      }
    Internal->Length[type] = length;
    }
  Internal->Subscript[type] = subscript;
  Internal->BitSize[type] = bitsize;
}

unsigned int LookupTable::GetLUTLength(LookupTableType type) const
{
  return Internal->Length[type];
}

void LookupTable::SetLUT(LookupTableType type, const unsigned char *array,
  unsigned int length)
{
  (void)length;
  //if( !Initialized() ) return;
  if( !Internal->Length[type] )
    {
    gdcmDebugMacro( "Need to set length first" );
    return;
    }

  if( !IncompleteLUT )
    {
    assert( Internal->RGB.size() == 3*Internal->Length[type]*(BitSample/8) );
    }
  // Too funny: 05115014-mr-siemens-avanto-syngo-with-palette-icone.dcm
  // There is pseudo PALETTE_COLOR LUT in the Icon, if one look carefully the LUT values
  // goes like this: 0, 1, 2, 3, 4, 5, 6, 7 ...
  if( BitSample == 8 )
    {
    const unsigned int mult = Internal->BitSize[type]/8;
    assert( Internal->Length[type]*mult == length );
    unsigned int offset = 0;
    if( mult == 2 )
      {
      offset = 1;
      }
    for( unsigned int i = 0; i < Internal->Length[type]; ++i)
      {
      assert( i*mult+offset < length );
      assert( 3*i+type < Internal->RGB.size() );
      Internal->RGB[3*i+type] = array[i*mult+offset];
      }
    }
  else if( BitSample == 16 )
    {
    assert( Internal->Length[type]*(BitSample/8) == length );
    uint16_t *uchar16 = (uint16_t*)&Internal->RGB[0];
    const uint16_t *array16 = (uint16_t*)array;
    for( unsigned int i = 0; i < Internal->Length[type]; ++i)
      {
      assert( 2*i < length );
      assert( 2*(3*i+type) < Internal->RGB.size() );
      uchar16[3*i+type] = array16[i];
      }
    }
}

void LookupTable::GetLUT(LookupTableType type, unsigned char *array, unsigned int &length) const
{
  if( BitSample == 8 )
    {
    const unsigned int mult = Internal->BitSize[type]/8;
    length = Internal->Length[type]*mult;
    unsigned int offset = 0;
    if( mult == 2 )
      {
      offset = 1;
      }
    for( unsigned int i = 0; i < Internal->Length[type]; ++i)
      {
      assert( i*mult+offset < length );
      assert( 3*i+type < Internal->RGB.size() );
      array[i*mult+offset] = Internal->RGB[3*i+type];
      }
    }
  else if( BitSample == 16 )
    {
    length = Internal->Length[type]*(BitSample/8);
    uint16_t *uchar16 = (uint16_t*)&Internal->RGB[0];
    uint16_t *array16 = (uint16_t*)array;
    for( unsigned int i = 0; i < Internal->Length[type]; ++i)
      {
      assert( 2*i < length );
      assert( 2*(3*i+type) < Internal->RGB.size() );
      array16[i] = uchar16[3*i+type];
      }
    }
}

void LookupTable::GetLUTDescriptor(LookupTableType type, unsigned short &length,
  unsigned short &subscript, unsigned short &bitsize) const
{
  assert( type >= RED && type <= BLUE );
  if( Internal->Length[type] == 65536 )
    {
    length = 0;
    }
  else
    {
    length = Internal->Length[type];
    }
  subscript = Internal->Subscript[type];
  bitsize = Internal->BitSize[type];

  // postcondition
  assert( subscript == 0 );
  assert( bitsize == 8 || bitsize == 16 );
}

void LookupTable::InitializeRedLUT(unsigned short length,
unsigned short subscript,
unsigned short bitsize)
  {
  InitializeLUT(RED, length, subscript, bitsize);
  }
void LookupTable::InitializeGreenLUT(unsigned short length,
  unsigned short subscript,
  unsigned short bitsize)
{
  InitializeLUT(GREEN, length, subscript, bitsize);
}
void LookupTable::InitializeBlueLUT(unsigned short length,
  unsigned short subscript,
  unsigned short bitsize)
{
  InitializeLUT(BLUE, length, subscript, bitsize);
}

void LookupTable::SetRedLUT(const unsigned char *red, unsigned int length)
{
  SetLUT(RED, red, length);
}

void LookupTable::SetGreenLUT(const unsigned char *green, unsigned int length)
{
  SetLUT(GREEN, green, length);
}

void LookupTable::SetBlueLUT(const unsigned char *blue, unsigned int length)
{
  SetLUT(BLUE, blue, length);
}

void LookupTable::Decode(std::istream &is, std::ostream &os) const
{
  if ( BitSample == 8 )
    {
    unsigned char idx;
    unsigned char rgb[3];
    while( !is.eof() )
      {
      //is.Get(c);
      is.read( (char*)(&idx), 1);
      // FIXME
      if( is.eof() )
        {
        break;
        }
      if( IncompleteLUT )
        {
        assert( idx < Internal->Length[RED] );
        assert( idx < Internal->Length[GREEN] );
        assert( idx < Internal->Length[BLUE] );
        }
      rgb[RED]   = Internal->RGB[3*idx+RED];
      rgb[GREEN] = Internal->RGB[3*idx+GREEN];
      rgb[BLUE]  = Internal->RGB[3*idx+BLUE];
      os.write((char*)rgb, 3 );
      }
    }
  else if ( BitSample == 16 )
    {
    const uint16_t *rgb16 = (uint16_t*)&Internal->RGB[0];
    while( !is.eof() )
      {
      unsigned short idx;
      unsigned short rgb[3];
      is.read( (char*)(&idx), 2);
      //is.Get(c);
      // FIXME
      if( is.eof() ) break;
      if( IncompleteLUT )
        {
        assert( idx < Internal->Length[RED] );
        assert( idx < Internal->Length[GREEN] );
        assert( idx < Internal->Length[BLUE] );
        }
      rgb[RED]   = rgb16[3*idx+RED];
      rgb[GREEN] = rgb16[3*idx+GREEN];
      rgb[BLUE]  = rgb16[3*idx+BLUE];
      os.write((char*)rgb, 3*2);
      }
    }
}

const unsigned char *LookupTable::GetPointer() const
{
  if ( BitSample == 8 )
    {
    return &Internal->RGB[0];
    }
  return 0;
}

bool LookupTable::GetBufferAsRGBA(unsigned char *rgba) const
{
  bool ret = false;
  if ( BitSample == 8 )
    {
    std::vector<unsigned char>::const_iterator it = Internal->RGB.begin();
    for(; it != Internal->RGB.end() ;)
      {
      // RED
      *rgba++ = *it++;
      // GREEN
      *rgba++ = *it++;
      // BLUE
      *rgba++ = *it++;
      // ALPHA
      *rgba++ = 255;
      }
    ret = true;
    }
  else if ( BitSample == 16 )
    {
/*
    assert( Internal->Length[type]*(BitSample/8) == length );
    uint16_t *uchar16 = (uint16_t*)&Internal->RGB[0];
    const uint16_t *array16 = (uint16_t*)array;
    for( unsigned int i = 0; i < Internal->Length[type]; ++i)
      {
      assert( 2*i < length );
      assert( 2*(3*i+type) < Internal->RGB.size() );
      uchar16[3*i+type] = array16[i];
      std::cout << i << " -> " << array16[i] << "\n";
      }

    ret = true;
*/
    //std::vector<unsigned char>::const_iterator it = Internal->RGB.begin();
    uint16_t *uchar16 = (uint16_t*)&Internal->RGB[0];
    uint16_t *rgba16 = (uint16_t*)rgba;
    size_t s = Internal->RGB.size();
    s /= 2;
    s /= 3;
    memset(rgba,0,Internal->RGB.size() * 4 / 3); // FIXME
    for(size_t i = 0; i < s; ++i)
      {
      // RED
      *rgba16++ = *uchar16++;
      // GREEN
      *rgba16++ = *uchar16++;
      // BLUE
      *rgba16++ = *uchar16++;
      // ALPHA
      *rgba16++ = 255*255;
      }

    ret = true;
    }
  return ret;
}

bool LookupTable::WriteBufferAsRGBA(const unsigned char *rgba)
{
  bool ret = false;
  if ( BitSample == 8 )
    {
    std::vector<unsigned char>::iterator it = Internal->RGB.begin();
    for(; it != Internal->RGB.end() ;)
      {
      // RED
      *it++ = *rgba++;
      // GREEN
      *it++ = *rgba++;
      // BLUE
      *it++ = *rgba++;
      // ALPHA
      rgba++; // = 255;
      }
    ret = true;
    }
  else if ( BitSample == 16 )
    {
    //assert( Internal->Length[type]*(BitSample/8) == length );
    uint16_t *uchar16 = (uint16_t*)&Internal->RGB[0];
    const uint16_t *rgba16 = (uint16_t*)rgba;
    size_t s = Internal->RGB.size();
    s /= 2;
    s /= 3;
    assert( s == 65536 );

    for( unsigned int i = 0; i < s /*i < Internal->Length[type]*/; ++i)
      {
      //assert( 2*i < length );
      //assert( 2*(3*i+type) < Internal->RGB.size() );
      //uchar16[3*i+type] = array16[i];
      //std::cout << i << " -> " << array16[i] << "\n";
      // RED
      *uchar16++ = *rgba16++;
      // GREEN
      *uchar16++ = *rgba16++;
      // BLUE
      *uchar16++ = *rgba16++;
      //
      rgba16++; // = *rgba16++;
      }

    ret = true;
    //ret = false;
    }
  return ret;
}

} // end namespace gdcm
