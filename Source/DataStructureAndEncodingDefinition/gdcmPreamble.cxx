/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmPreamble.h"
#include <string.h> // memset

namespace gdcm
{

Preamble::Preamble():Internal(nullptr)
{
  Create();
}

Preamble::~Preamble()
{
  delete[] Internal;
}

std::istream &Preamble::Read(std::istream &is)
{
  // \precondition: we are at beginning of Preamble
  gdcmAssertAlwaysMacro( !IsEmpty() /*&& is.tellg() == 0*/ );
  if( is.read(Internal, 128+4) )
    {
    if( Internal[128+0] == 'D'
      && Internal[128+1] == 'I'
      && Internal[128+2] == 'C'
      && Internal[128+3] == 'M')
      {
      return is;
      }
    }

  // else reset everything !
  delete[] Internal;
  Internal = nullptr;
  throw Exception( "Not a DICOM V3 file (No Preamble)" );

  // \postcondition we are after the Preamble (or at the beginning of file if none)
}

void Preamble::Valid()
{
  if( !Internal ) Internal = new char[128+4];
  memset( Internal, 0, 128 );
  memcpy( Internal+128, "DICM", 4);
}

void Preamble::Create()
{
  if( !Internal ) Internal = new char[128+4];
  memset( Internal, 0, 128 );
  memcpy( Internal+128, "DICM", 4);
}

void Preamble::Remove()
{
  delete[] Internal;
  Internal = nullptr; // important
}

// \precondition we are at the beginning of file
std::ostream const &Preamble::Write(std::ostream &os) const
{
//  assert ( os.tellg()+0 == 0 );
  if( Internal )
    {
    os.write( Internal, 128+4);
    }

  // \postcondition a valid Preamble has been written to stream
  return os;
}

void Preamble::Clear()
{
}

void Preamble::Print(std::ostream &) const
{
}

} // end namespace gdcm
