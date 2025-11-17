/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
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

#include "gdcmExplicitDataElement.h"
#include "gdcmImplicitDataElement.h"
#include "gdcmSwapper.h"
#include "gdcmValue.h"

#include "gdcmItem.h"
#include "gdcmSequenceOfItems.h"
#include "gdcmParseException.h"

#include "gdcmDeflateStream.h"

namespace gdcm
{

Writer::Writer():Stream(nullptr),Ofstream(nullptr),F(new File),CheckFileMetaInformation(true),WriteDataSetOnly(false)
{
}

Writer::~Writer()
{
  if (Ofstream)
    {
    delete Ofstream;
    Ofstream = nullptr;
    Stream = nullptr;
    }
}

bool Writer::Write()
{
  if( !Stream || !*Stream )
    {
    gdcmErrorMacro( "No Filename" );
    return false;
    }

  std::ostream &os = *Stream;
  FileMetaInformation &Header = F->GetHeader();
  DataSet &DS = F->GetDataSet();

  if( DS.IsEmpty() )
    {
    gdcmErrorMacro( "DS empty" );
    return false;
    }

  // Should I check that 0002,0002 / 0008,0016 and 0002,0003 / 0008,0018 match ?

  if( !WriteDataSetOnly )
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
        (void)ex;  //to avoid unreferenced variable warning on release
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

  const TransferSyntax &ts = Header.GetDataSetTransferSyntax();
  if( !ts.IsValid() )
    {
    gdcmErrorMacro( "Invalid Transfer Syntax" );
    return false;
    }

  if( ts == TransferSyntax::DeflatedExplicitVRLittleEndian )
    {
    //gzostream gzos(os.rdbuf());
      try {
      zlib_stream::zip_ostream gzos( os );
      gdcm_assert( ts.GetNegociatedType() == TransferSyntax::Explicit );
      DS.Write<ExplicitDataElement,SwapperNoOp>(gzos);
      //gzos.flush();
      } catch (...){
		  return false;
	  }

    return !os.fail();
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
        gdcm_assert( ts.GetNegociatedType() == TransferSyntax::Explicit );
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
        gdcm_assert( ts.GetNegociatedType() == TransferSyntax::Explicit );
        DS.Write<ExplicitDataElement,SwapperNoOp>(os);
        }
      }
    }
  catch(std::exception &ex)
    {
	(void)ex;  //to avoid unreferenced variable warning on release
    gdcmErrorMacro( ex.what() );
    return false;
    }
  catch(...)
    {
    gdcmErrorMacro( "what the hell" );
    return false;
    }

  os.flush();
  if (Ofstream)
    {
    Ofstream->close();
    }

  return !os.fail();
}

void Writer::SetFileName(const char *utf8path)
{
    //std::cerr << "Stream: " << filename << std::endl;
    //std::cerr << "Ofstream: " << Ofstream << std::endl;
    if (Ofstream)
      {
      if (Ofstream->is_open())
        {
        Ofstream->close();
        }
      delete Ofstream;
      }
    Ofstream = new std::ofstream();
    if (utf8path && *utf8path)
      {
#ifdef _MSC_VER
      const std::wstring uncpath = System::ConvertToUNC(utf8path);
      Ofstream->open(uncpath.c_str(), std::ios::out | std::ios::binary);
#else
      Ofstream->open(utf8path, std::ios::out | std::ios::binary);
#endif
      }
    Stream = Ofstream;
  }

} // end namespace gdcm
