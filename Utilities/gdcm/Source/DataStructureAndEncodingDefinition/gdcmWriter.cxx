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
#include "gdcmWriter.h"
#include "gdcmFileMetaInformation.h"
#include "gdcmDataSet.h"
#include "gdcmTrace.h"

#include "gdcmSwapper.h"
#include "gdcmDataSet.h"
#include "gdcmExplicitDataElement.h"
#include "gdcmImplicitDataElement.h"
#include "gdcmValue.h"

#include "gdcmValue.h"
#include "gdcmItem.h"
#include "gdcmSequenceOfItems.h"
#include "gdcmParseException.h"

#include "gdcmDeflateStream.h"

namespace gdcm
{

Writer::~Writer()
{
  if (Ofstream) {
    Ofstream->close();
    delete Ofstream;
  }
}

bool Writer::Write()
{
  if( !Stream )
    {
    gdcmErrorMacro( "No Filename" );
    return false;
    }

  //assert( F );
  //F->Write( Stream );
std::ostream &os = *Stream;
FileMetaInformation &Header = F->GetHeader();
DataSet &DS = F->GetDataSet();

if( DS.IsEmpty() )
  {
  gdcmErrorMacro( "DS empty" );
  return false;
  }

  // Should I check that 0002,0002 / 0008,0016 and 0002,0003 / 0008,0018 match ?

  try
    {
    if( CheckFileMetaInformation )
      {
      FileMetaInformation duplicate( Header );
      try
        {
        duplicate.FillFromDataSet( DS );
        }
      catch(gdcm::Exception &ex)
        {
        gdcmErrorMacro( "Could not recreate the File Meta Header, please report:" << ex.what() );
        return false;
        }
      duplicate.Write(os);
      }
    else
      {
      Header.Write(os);
      }
    }
  catch( std::exception &ex)
    {
    assert(0);
    // File such as PICKER-16-MONO2-No_DicomV3_Preamble.dcm
    // are a pain to rewrite since the metaheader was declared as implicit
    // we have to do a look up the in the dictionary to find out VR for those element
    // this is too much work, and should be up to the user to convert the meta to something
    // legal ! Write them as implicit for now
    gdcmWarningMacro( "File written will not be legal: " << ex.what() );
    if( Header.GetPreamble().IsEmpty() )
      {
      os.seekp(0, std::ios::beg);
      }
    else
      {
      os.seekp(128+4, std::ios::beg);
      }
      Header.DataSet::Write<ImplicitDataElement,SwapperNoOp>(os);
    }

  const TransferSyntax &ts = Header.GetDataSetTransferSyntax();
  if( !ts.IsValid() )
    {
    gdcmErrorMacro( "Invalid Transfer Syntax" );
    return false;
    }

  if( ts == TransferSyntax::DeflatedExplicitVRLittleEndian )
    {
    //gzostream gzos(os.rdbuf());
{
    zlib_stream::zip_ostream gzos( os );
    assert( ts.GetNegociatedType() == TransferSyntax::Explicit );
    DS.Write<ExplicitDataElement,SwapperNoOp>(gzos);
    //gzos.flush();
}

    return os;
    }

try
{
  if( ts.GetSwapCode() == SwapCode::BigEndian )
    {
    //US-RGB-8-epicard.dcm is big endian
    if( ts.GetNegociatedType() == TransferSyntax::Implicit )
      {
      // There is no such thing as Implicit Big Endian... oh well
      // LIBIDO-16-ACR_NEMA-Volume.dcm
      DS.Write<ImplicitDataElement,SwapperDoOp>(os);
      }
    else
      {
      assert( ts.GetNegociatedType() == TransferSyntax::Explicit );
      DS.Write<ExplicitDataElement,SwapperDoOp>(os);
      }
    }
  else // LittleEndian
    {
    if( ts.GetNegociatedType() == TransferSyntax::Implicit )
      {
      DS.Write<ImplicitDataElement,SwapperNoOp>(os);
      }
    else
      {
      assert( ts.GetNegociatedType() == TransferSyntax::Explicit );
      DS.Write<ExplicitDataElement,SwapperNoOp>(os);
      }
    }
}
catch(std::exception &ex)
{
  gdcmErrorMacro( ex.what() );
  return false;
}
catch(...)
{
  gdcmErrorMacro( "what the hell" );
  return false;
}



  // FIXME : call this function twice...
  if (Ofstream)
  {
  Ofstream->close();
  delete Ofstream;
  Ofstream = NULL;
  Stream = NULL;
  }

  return true;
}

} // end namespace gdcm
