//
// (C) Jan de Vaan 2007-2009, all rights reserved. See the accompanying "License.txt" for licensed use.
//

#include "stdafx.h"
#include "streams.h"
#include "header.h"


#include <math.h>

#include <vector>
#include <stdio.h>
#include <iostream>

#include "util.h"

#include "decoderstrategy.h"
#include "encoderstrategy.h"
#include "context.h"
#include "contextrunmode.h"
#include "lookuptable.h"


signed char* JlsContext::_tableC = CreateTableC();


// used to determine how large runs should be encoded at a time.
const int J[32]      = {0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 9, 10, 11, 12, 13, 14, 15};

const int BASIC_T1    = 3;
const int BASIC_T2    = 7;
const int BASIC_T3    = 21;

#include "losslesstraits.h"
#include "defaulttraits.h"

#include "scan.h"

signed char QuantizeGratientOrg(const Presets& preset, LONG NEAR, LONG Di)
{
  if (Di <= -preset.T3) return  -4;
  if (Di <= -preset.T2) return  -3;
  if (Di <= -preset.T1) return  -2;
  if (Di < -NEAR)  return  -1;
  if (Di <=  NEAR) return   0;
  if (Di < preset.T1)   return   1;
  if (Di < preset.T2)   return   2;
  if (Di < preset.T3)   return   3;

  return  4;
}



std::vector<signed char> CreateQLutLossless(LONG cbit)
{
  Presets preset = ComputeDefault((1 << cbit) - 1, 0);
  LONG range = preset.MAXVAL + 1;

  std::vector<signed char> lut;
  lut.resize(range * 2);

  for (LONG diff = -range; diff < range; diff++)
  {
    lut[range + diff] = QuantizeGratientOrg(preset, 0,diff);
  }
  return lut;
}

CTable rgtableShared[16] = { InitTable(0), InitTable(1), InitTable(2), InitTable(3),
               InitTable(4), InitTable(5), InitTable(6), InitTable(7),
               InitTable(8), InitTable(9), InitTable(10), InitTable(11),
               InitTable(12), InitTable(13), InitTable(14),InitTable(15) };


std::vector<signed char> rgquant8Ll = CreateQLutLossless(8);
std::vector<signed char> rgquant10Ll = CreateQLutLossless(10);
std::vector<signed char> rgquant12Ll = CreateQLutLossless(12);
std::vector<signed char> rgquant16Ll = CreateQLutLossless(16);


template<class STRATEGY>
STRATEGY* JlsCodecFactory<STRATEGY>::GetCodec(const JlsParamaters& _info, const JlsCustomParameters& presets)
{
  STRATEGY* pstrategy = NULL;
  if (presets.RESET != 0 && presets.RESET != BASIC_RESET)
  {
    DefaultTraitsT<BYTE,BYTE> traits((1 << _info.bitspersample) - 1, _info.allowedlossyerror);
    traits.MAXVAL = presets.MAXVAL;
    traits.RESET = presets.RESET;
    pstrategy = new JlsCodec<DefaultTraitsT<BYTE, BYTE>, STRATEGY>(traits, _info);
  }
  else
  {
    pstrategy = GetCodecImpl(_info);
  }

  if (pstrategy == NULL)
    return NULL;

  pstrategy->SetPresets(presets);
  return pstrategy;
}



template<class STRATEGY>
STRATEGY* JlsCodecFactory<STRATEGY>::GetCodecImpl(const JlsParamaters& _info)
{
  if (_info.ilv == ILV_SAMPLE && _info.components != 3)
    return NULL;

#ifndef DISABLE_SPECIALIZATIONS

  // optimized lossless versions common formats
  if (_info.allowedlossyerror == 0)
  {
    if (_info.ilv == ILV_SAMPLE)
    {
      if (_info.bitspersample == 8)
        return new JlsCodec<LosslessTraitsT<Triplet<BYTE>,8>, STRATEGY>(LosslessTraitsT<Triplet<BYTE>,8>(), _info);
    }
    else
    {
      switch (_info.bitspersample)
      {
        case  8: return new JlsCodec<LosslessTraitsT<BYTE,   8>, STRATEGY>(LosslessTraitsT<BYTE,   8>(), _info);
        case 12: return new JlsCodec<LosslessTraitsT<USHORT,12>, STRATEGY>(LosslessTraitsT<USHORT,  12>(), _info);
        case 16: return new JlsCodec<LosslessTraitsT<USHORT,16>, STRATEGY>(LosslessTraitsT<USHORT,  16>(), _info);
      }
    }
  }

#endif

  int maxval = (1 << _info.bitspersample) - 1;

  if (_info.bitspersample <= 8)
  {
    if (_info.ilv == ILV_SAMPLE)
    {
      DefaultTraitsT<BYTE,Triplet<BYTE> > traits(maxval, _info.allowedlossyerror);
      return new JlsCodec<DefaultTraitsT<BYTE,Triplet<BYTE> >, STRATEGY>(traits, _info);
    }
    DefaultTraitsT<BYTE, BYTE> traits((1 << _info.bitspersample) - 1, _info.allowedlossyerror);
    return new JlsCodec<DefaultTraitsT<BYTE, BYTE>, STRATEGY>(traits, _info);
  }
  else if (_info.bitspersample <= 16)
  {
    if (_info.ilv == ILV_SAMPLE)
    {
      DefaultTraitsT<USHORT,Triplet<USHORT> > traits(maxval, _info.allowedlossyerror);
      return new JlsCodec<DefaultTraitsT<USHORT,Triplet<USHORT> >, STRATEGY>(traits, _info);
    }

    DefaultTraitsT<USHORT, USHORT> traits(maxval, _info.allowedlossyerror);
    return new JlsCodec<DefaultTraitsT<USHORT, USHORT>, STRATEGY>(traits, _info);
  }
  return NULL;
}


template class JlsCodecFactory<DecoderStrategy>;
template class JlsCodecFactory<EncoderStrategy>;
