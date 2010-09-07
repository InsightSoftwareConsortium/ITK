//
// (C) Jan de Vaan 2007-2009, all rights reserved. See the accompanying "License.txt" for licensed use.
//

#include "stdafx.h"
#include "interface.h"
#include "header.h"


JLS_ERROR CheckInput(const void* pdataCompressed, size_t cbyteCompressed, const void* pdataUncompressed, size_t cbyteUncompressed, const JlsParamaters* pparams)
{
  if (pparams == NULL)
    return InvalidJlsParameters;

  if (cbyteCompressed == 0)
    return InvalidJlsParameters;

  if (pdataCompressed == NULL)
    return InvalidJlsParameters;

  if (pdataUncompressed == NULL)
    return InvalidJlsParameters;

  if (pparams->bitspersample < 6 || pparams->bitspersample > 16)
    return ParameterValueNotSupported;

  if (pparams->width < 1 || pparams->width > 65535)
    return ParameterValueNotSupported;

  if (pparams->height < 1 || pparams->height > 65535)
    return ParameterValueNotSupported;

  int bytesperline = pparams->bytesperline < 0 ? -pparams->bytesperline : pparams->bytesperline;

  if (cbyteUncompressed < size_t(bytesperline * pparams->height))
    return InvalidJlsParameters;

  switch (pparams->components)
  {
    case 4: return pparams->ilv == ILV_SAMPLE ? ParameterValueNotSupported : OK;
    case 3: return OK;
    case 1: return pparams->ilv != ILV_NONE ? ParameterValueNotSupported : OK;
    case 0: return InvalidJlsParameters;

    default: return pparams->ilv != ILV_NONE ? ParameterValueNotSupported : OK;
  }
}


extern "C"
{

CHARLS_IMEXPORT JLS_ERROR JpegLsEncode(void* pdataCompressed, size_t cbyteBuffer, size_t* pcbyteWritten, const void* pdataUncompressed, size_t cbyteUncompressed, const JlsParamaters* pparams)
{
  JlsParamaters info = *pparams;
  if(info.bytesperline == 0)
  {
    info.bytesperline = info.width * ((info.bitspersample + 7)/8);
    if (info.ilv != ILV_NONE)
    {
      info.bytesperline *= info.components;
    }
  }

  JLS_ERROR parameterError = CheckInput(pdataCompressed, cbyteBuffer, pdataUncompressed, cbyteUncompressed, &info);

  if (parameterError != OK)
    return parameterError;

  if (pcbyteWritten == NULL)
    return InvalidJlsParameters;

  Size size = Size(info.width, info.height);
  LONG cbit = info.bitspersample;
  JLSOutputStream stream;

  stream.Init(size, info.bitspersample, info.components);

  if (info.colorTransform != 0)
  {
    stream.AddColorTransform(info.colorTransform);
  }

  if (info.ilv == ILV_NONE)
  {
    LONG cbyteComp = size.cx*size.cy*((cbit +7)/8);
    for (LONG icomp = 0; icomp < info.components; ++icomp)
    {
      const BYTE* pbyteComp = static_cast<const BYTE*>(pdataUncompressed) + icomp*cbyteComp;
      stream.AddScan(pbyteComp, &info);
    }
  }
  else
  {
    stream.AddScan(pdataUncompressed, &info);
  }


  stream.Write((BYTE*)pdataCompressed, cbyteBuffer);

  *pcbyteWritten = stream.GetBytesWritten();
  return OK;
}

CHARLS_IMEXPORT JLS_ERROR JpegLsDecode(void* pdataUncompressed, size_t cbyteUncompressed, const void* pdataCompressed, size_t cbyteCompressed, JlsParamaters* info)
{
  JLSInputStream reader((BYTE*)pdataCompressed, cbyteCompressed);

  if(info != NULL)
  {
     reader.SetInfo(info);
  }

  try
  {
    reader.Read(pdataUncompressed, cbyteUncompressed);
    return OK;
  }
  catch (JlsException& e)
  {
    return e._error;
  }
}


CHARLS_IMEXPORT JLS_ERROR JpegLsVerifyEncode(const void* pdataUncompressed, size_t cbyteUncompressed, const void* pdataCompressed, size_t cbyteBuffer)
{
  JlsParamaters params = JlsParamaters();

  JLS_ERROR error = JpegLsReadHeader(pdataCompressed, cbyteBuffer, &params);
  if (error != OK)
    return error;

  error = CheckInput(pdataCompressed, cbyteBuffer, pdataUncompressed, cbyteUncompressed, &params);

  if (error != OK)
    return error;

  Size size = Size(params.width, params.height);
  LONG cbit = params.bitspersample;

  JLSOutputStream stream;

  stream.Init(size, params.bitspersample, params.components);

  if (params.ilv == ILV_NONE)
  {
    LONG cbyteComp = size.cx*size.cy*((cbit +7)/8);
    for (LONG icomp = 0; icomp < params.components; ++icomp)
    {
      const BYTE* pbyteComp = static_cast<const BYTE*>(pdataUncompressed) + icomp*cbyteComp;
      stream.AddScan(pbyteComp, &params);
    }
  }
  else
  {
    stream.AddScan(pdataUncompressed, &params);
  }

  std::vector<BYTE> rgbyteCompressed;
  rgbyteCompressed.resize(cbyteBuffer + 16);
  memcpy(&rgbyteCompressed[0], pdataCompressed, cbyteBuffer);


  stream.EnableCompare(true);
  stream.Write(&rgbyteCompressed[0], cbyteBuffer);

  return OK;
}


CHARLS_IMEXPORT JLS_ERROR JpegLsReadHeader(const void* pdataCompressed, size_t cbyteCompressed, JlsParamaters* pparams)
{
  try
  {
    JLSInputStream reader((BYTE*)pdataCompressed, cbyteCompressed);
    reader.ReadHeader();
    JlsParamaters info = reader.GetMetadata();
    *pparams = info;
    return OK;
  }
  catch (JlsException& e)
  {
    return e._error;
  }

}

}
