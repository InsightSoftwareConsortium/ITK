//
// (C) Jan de Vaan 2007-2009, all rights reserved. See the accompanying "License.txt" for licensed use.
//

#ifndef CHARLS_SCAN
#define CHARLS_SCAN

#include "lookuptable.h"

// This file contains the code for handling a "scan". Usually an image is encoded as a single scan.


#ifdef _MSC_VER
#pragma warning (disable: 4127)
#endif


extern CTable rgtableShared[16];
extern std::vector<signed char> rgquant8Ll;
extern std::vector<signed char> rgquant10Ll;
extern std::vector<signed char> rgquant12Ll;
extern std::vector<signed char> rgquant16Ll;
//
// Apply
//
inlinehint LONG ApplySign(LONG i, LONG sign)
{ return (sign ^ i) - sign; }


LONG CLAMP(LONG i, LONG j, LONG MAXVAL)
{
  if (i > MAXVAL || i < j)
    return j;

  return i;
}

Presets ComputeDefault(LONG MAXVAL, LONG NEAR)
{
  Presets preset;

  LONG FACTOR = (MIN(MAXVAL, 4095) + 128)/256;

  preset.T1 = CLAMP(FACTOR * (BASIC_T1 - 2) + 2 + 3*NEAR, NEAR + 1, MAXVAL);
  preset.T2 = CLAMP(FACTOR * (BASIC_T2 - 3) + 3 + 5*NEAR, preset.T1, MAXVAL);
  preset.T3 = CLAMP(FACTOR * (BASIC_T3 - 4) + 4 + 7*NEAR, preset.T2, MAXVAL);
  preset.MAXVAL = MAXVAL;
  preset.RESET = BASIC_RESET;
  return preset;
}


// Two alternatives for GetPredictedValue() (second is slightly faster due to reduced branching)

#if 0

inlinehint LONG GetPredictedValue(LONG Ra, LONG Rb, LONG Rc)
{
  if (Ra < Rb)
  {
    if (Rc < Ra)
      return Rb;

    if (Rc > Rb)
      return Ra;
  }
  else
  {
    if (Rc < Rb)
      return Ra;

    if (Rc > Ra)
      return Rb;
  }

  return Ra + Rb - Rc;
}

#else

inlinehint LONG GetPredictedValue(LONG Ra, LONG Rb, LONG Rc)
{
  // sign trick reduces the number of if statements (branches)
  LONG sgn = BitWiseSign(Rb - Ra);

  // is Ra between Rc and Rb?
  if ((sgn ^ (Rc - Ra)) < 0)
  {
    return Rb;
  }
  else if ((sgn ^ (Rb - Rc)) < 0)
  {
    return Ra;
  }

  // default case, valid if Rc element of [Ra,Rb]
  return Ra + Rb - Rc;
}

#endif

inlinehint LONG UnMapErrVal(LONG mappedError)
{
  //LONG sign = ~((mappedError & 1) - 1);
  LONG sign = LONG(mappedError << (LONG_BITCOUNT-1)) >> (LONG_BITCOUNT-1);
  return sign ^ (mappedError >> 1);
}



inlinehint LONG GetMappedErrVal(LONG Errval)
{
  LONG mappedError = (Errval >> (LONG_BITCOUNT-2)) ^ (2 * Errval);
  return mappedError;
}



inlinehint  LONG ComputeContextID(LONG Q1, LONG Q2, LONG Q3)
{ return (Q1*9 + Q2)*9 + Q3; }


//
//
//
template <class TRAITS, class STRATEGY>
class JlsCodec : public STRATEGY
{
public:
  typedef typename TRAITS::PIXEL PIXEL;
  typedef typename TRAITS::SAMPLE SAMPLE;

public:

    JlsCodec(const TRAITS& inTraits, const JlsParamaters& info) : STRATEGY(info),
    traits(inTraits),
      _size(0,0),
      T1(0),
      T2(0),
      T3(0),
      RUNindex(0),
      _pquant(0),
      _bCompare(0)

    {
      if (STRATEGY::_info.ilv == ILV_NONE)
      {
        STRATEGY::_info.components = 1;
      }
    }


    void SetPresets(const JlsCustomParameters& presets)
    {

      Presets presetDefault = ComputeDefault(traits.MAXVAL, traits.NEAR);

      InitParams(presets.T1 != 0 ? presets.T1 : presetDefault.T1,
        presets.T2 != 0 ? presets.T2 : presetDefault.T2,
        presets.T3 != 0 ? presets.T3 : presetDefault.T3,
        presets.RESET != 0 ? presets.RESET : presetDefault.RESET);
    }


    bool IsInterleaved()
    {
      if (STRATEGY::_info.ilv == ILV_NONE)
        return false;

      if (STRATEGY::_info.components == 1)
        return false;

      return true;
    }


    signed char QuantizeGratientOrg(LONG Di);
    inlinehint LONG QuantizeGratient(LONG Di)
    {
      ASSERT(QuantizeGratientOrg(Di) == *(_pquant + Di));
      return *(_pquant + Di);
    }

    void InitQuantizationLUT();

    LONG DecodeValue(LONG k, LONG limit, LONG qbpp);
    inlinehint void EncodeMappedValue(LONG k, LONG mappederval, LONG limit);

    void IncrementRunIndex()
    { RUNindex = MIN(31,RUNindex + 1); }
    void DecrementRunIndex()
    { RUNindex = MAX(0,RUNindex - 1); }

    LONG    DecodeRIError(CContextRunMode& ctx);
    Triplet<SAMPLE> DecodeRIPixel(Triplet<SAMPLE> Ra, Triplet<SAMPLE> Rb);
    SAMPLE   DecodeRIPixel(LONG Ra, LONG Rb);
    LONG    DecodeRunPixels(PIXEL Ra, PIXEL* ptype, LONG cpixelMac);
    LONG    DoRunMode(LONG ipixel, DecoderStrategy*);

    void  EncodeRIError(CContextRunMode& ctx, LONG Errval);
    SAMPLE  EncodeRIPixel(LONG x, LONG Ra, LONG Rb);
    Triplet<SAMPLE> EncodeRIPixel(Triplet<SAMPLE> x, Triplet<SAMPLE> Ra, Triplet<SAMPLE> Rb);
    void  EncodeRunPixels(LONG runLength, bool bEndofline);
    LONG    DoRunMode(LONG ipixel, EncoderStrategy*);

    inlinehint SAMPLE DoRegular(LONG Qs, LONG, LONG pred, DecoderStrategy*);
    inlinehint SAMPLE DoRegular(LONG Qs, LONG x, LONG pred, EncoderStrategy*);

    void DoLine(SAMPLE* pdummy);
    void DoLine(Triplet<SAMPLE>* pdummy);
    void DoScan(BYTE* pbyteCompressed, size_t cbyteCompressed);

public:
  ProcessLine* InitProcess(void* pvoidOut);
  void InitDefault();
  void InitParams(LONG t1, LONG t2, LONG t3, LONG nReset);

  size_t  EncodeScan(const void* pvoid, const Size& size, void* pvoidOut, size_t cbyte, void* pvoidCompare);
  size_t  DecodeScan(void* pvoidOut, const Size& size, const void* pvoidIn, size_t cbyte, bool bCompare);

protected:
  // codec parameters
  TRAITS traits;
  Size _size;
  LONG T1;
  LONG T2;
  LONG T3;

  // compression context
  JlsContext _contexts[365];
  CContextRunMode _contextRunmode[2];
  LONG RUNindex;
  PIXEL* ptypePrev; // previous line ptr
  PIXEL* ptypeCur; // current line ptr


  // quantization lookup table
  signed char* _pquant;
  std::vector<signed char> _rgquant;

  // debugging
  bool _bCompare;
};


// Encode/decode a single sample. Performancewise the #1 important functions

template<class TRAITS, class STRATEGY>
typename TRAITS::SAMPLE JlsCodec<TRAITS,STRATEGY>::DoRegular(LONG Qs, LONG, LONG pred, DecoderStrategy*)
{
  LONG sign    = BitWiseSign(Qs);
  JlsContext& ctx  = _contexts[ApplySign(Qs, sign)];
  LONG k      = ctx.GetGolomb();
  LONG Px      = traits.CorrectPrediction(pred + ApplySign(ctx.C, sign));

  LONG ErrVal;
  const Code& code    = rgtableShared[k].Get(STRATEGY::PeekByte());
  if (code.GetLength() != 0)
  {
    STRATEGY::Skip(code.GetLength());
    ErrVal = code.GetValue();
    ASSERT(abs(ErrVal) < 65535);
  }
  else
  {
    ErrVal = UnMapErrVal(DecodeValue(k, traits.LIMIT, traits.qbpp));
    if (abs(ErrVal) > 65535)
      throw JlsException(InvalidCompressedData);
  }
  ErrVal = ErrVal ^ ((traits.NEAR == 0) ? ctx.GetErrorCorrection(k) : 0);
  ctx.UpdateVariables(ErrVal, traits.NEAR, traits.RESET);
  ErrVal = ApplySign(ErrVal, sign);
  return traits.ComputeReconstructedSample(Px, ErrVal);
}


template<class TRAITS, class STRATEGY>
typename TRAITS::SAMPLE JlsCodec<TRAITS,STRATEGY>::DoRegular(LONG Qs, LONG x, LONG pred, EncoderStrategy*)
{
  LONG sign    = BitWiseSign(Qs);
  JlsContext& ctx  = _contexts[ApplySign(Qs, sign)];
  LONG k      = ctx.GetGolomb();
  LONG Px      = traits.CorrectPrediction(pred + ApplySign(ctx.C, sign));

  LONG ErrVal    = traits.ComputeErrVal(ApplySign(x - Px, sign));

  EncodeMappedValue(k, GetMappedErrVal(ctx.GetErrorCorrection(k | traits.NEAR) ^ ErrVal), traits.LIMIT);
  ctx.UpdateVariables(ErrVal, traits.NEAR, traits.RESET);
  ASSERT(traits.IsNear(traits.ComputeReconstructedSample(Px, ApplySign(ErrVal, sign)), x));
  return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Px, ApplySign(ErrVal, sign)));
}


// Functions to build tables used to decode short golomb codes.

inlinehint std::pair<LONG, LONG> CreateEncodedValue(LONG k, LONG mappederval)
{
  LONG highbits = mappederval >> k;
  return std::make_pair(highbits + k + 1, (LONG(1) << k) | (mappederval & ((LONG(1) << k) - 1)));
}


CTable InitTable(LONG k)
{
  CTable table;
  for (short nerr = 0; ; nerr++)
  {
    // Q is not used when k != 0
    LONG merrval = GetMappedErrVal(nerr);//, k, -1);
    std::pair<LONG, LONG> paircode = CreateEncodedValue(k, merrval);
    if (paircode.first > CTable::cbit)
      break;

    Code code = Code( nerr, short(paircode.first) );
    table.AddEntry(BYTE(paircode.second), code);
  }

  for (short nerr = -1; ; nerr--)
  {
    // Q is not used when k != 0
    LONG merrval = GetMappedErrVal(nerr);//, k, -1);
    std::pair<LONG, LONG> paircode = CreateEncodedValue(k, merrval);
    if (paircode.first > CTable::cbit)
      break;

    Code code = Code(nerr, short(paircode.first));
    table.AddEntry(BYTE(paircode.second), code);
  }

  return table;
}


// Encoding/decoding of golomb codes

template<class TRAITS, class STRATEGY>
LONG JlsCodec<TRAITS,STRATEGY>::DecodeValue(LONG k, LONG limit, LONG qbpp)
{
  LONG highbits = STRATEGY::ReadHighbits();

  if (highbits >= limit - (qbpp + 1))
    return STRATEGY::ReadValue(qbpp) + 1;

  if (k == 0)
    return highbits;

  return (highbits << k) + STRATEGY::ReadValue(k);
}



template<class TRAITS, class STRATEGY>
inlinehint void JlsCodec<TRAITS,STRATEGY>::EncodeMappedValue(LONG k, LONG mappederval, LONG limit)
{
  LONG highbits = mappederval >> k;

  if (highbits < limit - traits.qbpp - 1)
  {
    if (highbits + 1 > 31)
    {
      STRATEGY::AppendToBitStream(0, highbits / 2);
      highbits = highbits - highbits / 2;
    }
    STRATEGY::AppendToBitStream(1, highbits + 1);
    STRATEGY::AppendToBitStream((mappederval & ((1 << k) - 1)), k);
    return;
  }

  if (limit - traits.qbpp > 31)
  {
    STRATEGY::AppendToBitStream(0, 31);
    STRATEGY::AppendToBitStream(1, limit - traits.qbpp - 31);
  }
  else
  {
    STRATEGY::AppendToBitStream(1, limit - traits.qbpp);
  }
  STRATEGY::AppendToBitStream((mappederval - 1) & ((1 << traits.qbpp) - 1), traits.qbpp);
}


// Sets up a lookup table to "Quantize" sample difference.

template<class TRAITS, class STRATEGY>
void JlsCodec<TRAITS,STRATEGY>::InitQuantizationLUT()
{
  // for lossless mode with default paramaters, we have precomputed te luts for bitcounts 8,10,12 and 16
  if (traits.NEAR == 0 && traits.MAXVAL == (1 << traits.bpp) - 1)
  {
    Presets presets = ComputeDefault(traits.MAXVAL, traits.NEAR);
    if (presets.T1 == T1 && presets.T2 == T2 && presets.T3 == T3)
    {
      if (traits.bpp == 8)
      {
        _pquant = &rgquant8Ll[rgquant8Ll.size() / 2 ];
        return;
      }
      if (traits.bpp == 10)
      {
        _pquant = &rgquant10Ll[rgquant10Ll.size() / 2 ];
        return;
      }
      if (traits.bpp == 12)
      {
        _pquant = &rgquant12Ll[rgquant12Ll.size() / 2 ];
        return;
      }
      if (traits.bpp == 16)
      {
        _pquant = &rgquant16Ll[rgquant16Ll.size() / 2 ];
        return;
      }
    }
  }

  LONG RANGE = 1 << traits.bpp;

  _rgquant.resize(RANGE * 2);

  _pquant = &_rgquant[RANGE];
  for (LONG i = -RANGE; i < RANGE; ++i)
  {
    _pquant[i] = QuantizeGratientOrg(i);
  }
}


template<class TRAITS, class STRATEGY>
signed char JlsCodec<TRAITS,STRATEGY>::QuantizeGratientOrg(LONG Di)
{
  if (Di <= -T3) return  -4;
  if (Di <= -T2) return  -3;
  if (Di <= -T1) return  -2;
  if (Di < -traits.NEAR)  return  -1;
  if (Di <=  traits.NEAR) return   0;
  if (Di < T1)   return   1;
  if (Di < T2)   return   2;
  if (Di < T3)   return   3;

  return  4;
}



// RI = Run interruption: functions that handle the sample terminating a run.

template<class TRAITS, class STRATEGY>
LONG JlsCodec<TRAITS,STRATEGY>::DecodeRIError(CContextRunMode& ctx)
{
  LONG k = ctx.GetGolomb();
  LONG EMErrval = DecodeValue(k, traits.LIMIT - J[RUNindex]-1, traits.qbpp);
  LONG Errval = ctx.ComputeErrVal(EMErrval + ctx._nRItype, k);
  ctx.UpdateVariables(Errval, EMErrval);
  return Errval;
}



template<class TRAITS, class STRATEGY>
void JlsCodec<TRAITS,STRATEGY>::EncodeRIError(CContextRunMode& ctx, LONG Errval)
{
  LONG k      = ctx.GetGolomb();
  bool map    = ctx.ComputeMap(Errval, k);
  LONG EMErrval  = 2 * abs(Errval) - ctx._nRItype - map;

  ASSERT(Errval == ctx.ComputeErrVal(EMErrval + ctx._nRItype, k));
  EncodeMappedValue(k, EMErrval, traits.LIMIT-J[RUNindex]-1);
  ctx.UpdateVariables(Errval, EMErrval);
}


template<class TRAITS, class STRATEGY>
Triplet<typename TRAITS::SAMPLE> JlsCodec<TRAITS,STRATEGY>::DecodeRIPixel(Triplet<SAMPLE> Ra, Triplet<SAMPLE> Rb)
{
  LONG Errval1 = DecodeRIError(_contextRunmode[0]);
  LONG Errval2 = DecodeRIError(_contextRunmode[0]);
  LONG Errval3 = DecodeRIError(_contextRunmode[0]);

  return Triplet<SAMPLE>(traits.ComputeReconstructedSample(Rb.v1, Errval1 * Sign(Rb.v1  - Ra.v1)),
    traits.ComputeReconstructedSample(Rb.v2, Errval2 * Sign(Rb.v2  - Ra.v2)),
    traits.ComputeReconstructedSample(Rb.v3, Errval3 * Sign(Rb.v3  - Ra.v3)));
}



template<class TRAITS, class STRATEGY>
Triplet<typename TRAITS::SAMPLE> JlsCodec<TRAITS,STRATEGY>::EncodeRIPixel(Triplet<SAMPLE> x, Triplet<SAMPLE> Ra, Triplet<SAMPLE> Rb)
{
  LONG errval1  = traits.ComputeErrVal(Sign(Rb.v1 - Ra.v1) * (x.v1 - Rb.v1));
  EncodeRIError(_contextRunmode[0], errval1);

  LONG errval2  = traits.ComputeErrVal(Sign(Rb.v2 - Ra.v2) * (x.v2 - Rb.v2));
  EncodeRIError(_contextRunmode[0], errval2);

  LONG errval3  = traits.ComputeErrVal(Sign(Rb.v3 - Ra.v3) * (x.v3 - Rb.v3));
  EncodeRIError(_contextRunmode[0], errval3);


  return Triplet<SAMPLE>(traits.ComputeReconstructedSample(Rb.v1, errval1 * Sign(Rb.v1  - Ra.v1)),
    traits.ComputeReconstructedSample(Rb.v2, errval2 * Sign(Rb.v2  - Ra.v2)),
    traits.ComputeReconstructedSample(Rb.v3, errval3 * Sign(Rb.v3  - Ra.v3)));
}



template<class TRAITS, class STRATEGY>
typename TRAITS::SAMPLE JlsCodec<TRAITS,STRATEGY>::DecodeRIPixel(LONG Ra, LONG Rb)
{
  if (abs(Ra - Rb) <= traits.NEAR)
  {
    LONG ErrVal    = DecodeRIError(_contextRunmode[1]);
    return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Ra, ErrVal));
  }
  else
  {
    LONG ErrVal    = DecodeRIError(_contextRunmode[0]);
    return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Rb, ErrVal * Sign(Rb - Ra)));
  }
}


template<class TRAITS, class STRATEGY>
typename TRAITS::SAMPLE JlsCodec<TRAITS,STRATEGY>::EncodeRIPixel(LONG x, LONG Ra, LONG Rb)
{
  if (abs(Ra - Rb) <= traits.NEAR)
  {
    LONG ErrVal  = traits.ComputeErrVal(x - Ra);
    EncodeRIError(_contextRunmode[1], ErrVal);
    return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Ra, ErrVal));
  }
  else
  {
    LONG ErrVal  = traits.ComputeErrVal((x - Rb) * Sign(Rb - Ra));
    EncodeRIError(_contextRunmode[0], ErrVal);
    return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Rb, ErrVal * Sign(Rb - Ra)));
  }
}


// RunMode: Functions that handle run-length encoding

template<class TRAITS, class STRATEGY>
void JlsCodec<TRAITS,STRATEGY>::EncodeRunPixels(LONG runLength, bool bEndofline)
{
  while (runLength >= LONG(1 << J[RUNindex]))
  {
    STRATEGY::AppendOnesToBitStream(1);
    runLength = runLength - LONG(1 << J[RUNindex]);
    IncrementRunIndex();
  }

  if (bEndofline)
  {
    if (runLength != 0)
    {
      STRATEGY::AppendOnesToBitStream(1);
    }
  }
  else
  {
    STRATEGY::AppendToBitStream(runLength, J[RUNindex] + 1);  // leading 0 + actual remaining length
  }
}


template<class TRAITS, class STRATEGY>
LONG JlsCodec<TRAITS,STRATEGY>::DecodeRunPixels(PIXEL Ra, PIXEL* ptype, LONG cpixelMac)
{
  LONG ipixel = 0;
  while (STRATEGY::ReadBit())
  {
    int cpixel = MIN(1 << J[RUNindex], int(cpixelMac - ipixel));
    ipixel += cpixel;
    ASSERT(ipixel <= cpixelMac);

    if (cpixel == (1 << J[RUNindex]))
    {
      IncrementRunIndex();
    }

    if (ipixel == cpixelMac)
      break;
  }


  if (ipixel != cpixelMac)
  {
    // incomplete run
    ipixel += (J[RUNindex] > 0) ? STRATEGY::ReadValue(J[RUNindex]) : 0;
  }

  for (LONG i = 0; i < ipixel; ++i)
  {
    ptype[i] = Ra;
  }

  return ipixel;
}

template<class TRAITS, class STRATEGY>
LONG JlsCodec<TRAITS,STRATEGY>::DoRunMode(LONG ipixel, EncoderStrategy*)
{
  LONG ctypeRem = _size.cx - ipixel;
  PIXEL* ptypeCurX = ptypeCur + ipixel;
  PIXEL* ptypePrevX = ptypePrev + ipixel;

  PIXEL Ra = ptypeCurX[-1];

  LONG runLength = 0;

  while (traits.IsNear(ptypeCurX[runLength],Ra))
  {
    ptypeCurX[runLength] = Ra;
    runLength++;

    if (runLength == ctypeRem)
      break;
  }

  EncodeRunPixels(runLength, runLength == ctypeRem);

  if (runLength == ctypeRem)
    return runLength;

  ptypeCurX[runLength] = EncodeRIPixel(ptypeCurX[runLength], Ra, ptypePrevX[runLength]);
  DecrementRunIndex();
  return runLength + 1;
}



template<class TRAITS, class STRATEGY>
LONG JlsCodec<TRAITS,STRATEGY>::DoRunMode(LONG ipixelStart, DecoderStrategy*)
{
  PIXEL Ra = ptypeCur[ipixelStart-1];

  LONG cpixelRun = DecodeRunPixels(Ra, ptypeCur + ipixelStart, _size.cx - ipixelStart);

  LONG ipixelEnd = ipixelStart + cpixelRun;

  if (ipixelEnd == _size.cx)
    return ipixelEnd - ipixelStart;

  // run interruption
  PIXEL Rb = ptypePrev[ipixelEnd];
  ptypeCur[ipixelEnd] =  DecodeRIPixel(Ra, Rb);
  DecrementRunIndex();
  return ipixelEnd - ipixelStart + 1;
}


// DoLine: Encodes/Decodes a scanline of samples

template<class TRAITS, class STRATEGY>
void JlsCodec<TRAITS,STRATEGY>::DoLine(SAMPLE*)
{
  LONG ipixel = 0;
  LONG Rb = ptypePrev[ipixel-1];
  LONG Rd = ptypePrev[ipixel];

  while(ipixel < _size.cx)
  {
    LONG Ra = ptypeCur[ipixel -1];
    LONG Rc = Rb;
    Rb = Rd;
    Rd = ptypePrev[ipixel + 1];

    LONG Qs = ComputeContextID(QuantizeGratient(Rd - Rb), QuantizeGratient(Rb - Rc), QuantizeGratient(Rc - Ra));

    if (Qs != 0)
    {
      ptypeCur[ipixel] = DoRegular(Qs, ptypeCur[ipixel], GetPredictedValue(Ra, Rb, Rc), (STRATEGY*)(NULL));
      ipixel++;
    }
    else
    {
      ipixel += DoRunMode(ipixel, (STRATEGY*)(NULL));
      Rb = ptypePrev[ipixel-1];
      Rd = ptypePrev[ipixel];
    }
  }
}


// DoLine: Encodes/Decodes a scanline of triplets in ILV_SAMPLE mode

template<class TRAITS, class STRATEGY>
void JlsCodec<TRAITS,STRATEGY>::DoLine(Triplet<SAMPLE>*)
{
  LONG ipixel = 0;
  while(ipixel < _size.cx)
  {
    Triplet<SAMPLE> Ra = ptypeCur[ipixel -1];
    Triplet<SAMPLE> Rc = ptypePrev[ipixel-1];
    Triplet<SAMPLE> Rb = ptypePrev[ipixel];
    Triplet<SAMPLE> Rd = ptypePrev[ipixel + 1];

    LONG Qs1 = ComputeContextID(QuantizeGratient(Rd.v1 - Rb.v1), QuantizeGratient(Rb.v1 - Rc.v1), QuantizeGratient(Rc.v1 - Ra.v1));
    LONG Qs2 = ComputeContextID(QuantizeGratient(Rd.v2 - Rb.v2), QuantizeGratient(Rb.v2 - Rc.v2), QuantizeGratient(Rc.v2 - Ra.v2));
    LONG Qs3 = ComputeContextID(QuantizeGratient(Rd.v3 - Rb.v3), QuantizeGratient(Rb.v3 - Rc.v3), QuantizeGratient(Rc.v3 - Ra.v3));


    if (Qs1 == 0 && Qs2 == 0 && Qs3 == 0)
    {
      ipixel += DoRunMode(ipixel, (STRATEGY*)(NULL));
    }
    else
    {
      Triplet<SAMPLE> Rx;
      Rx.v1 = DoRegular(Qs1, ptypeCur[ipixel].v1, GetPredictedValue(Ra.v1, Rb.v1, Rc.v1), (STRATEGY*)(NULL));
      Rx.v2 = DoRegular(Qs2, ptypeCur[ipixel].v2, GetPredictedValue(Ra.v2, Rb.v2, Rc.v2), (STRATEGY*)(NULL));
      Rx.v3 = DoRegular(Qs3, ptypeCur[ipixel].v3, GetPredictedValue(Ra.v3, Rb.v3, Rc.v3), (STRATEGY*)(NULL));
      ptypeCur[ipixel] = Rx;
      ipixel++;
    }
  }
}


// DoScan: Encodes or decodes a scan.
// In ILV_SAMPLE mode, multiple components are handled in DoLine
// In ILV_LINE mode, a call do DoLine is made for every component
// In ILV_NONE mode, DoScan is called for each component

template<class TRAITS, class STRATEGY>
void JlsCodec<TRAITS,STRATEGY>::DoScan(BYTE* pbyteCompressed, size_t cbyteCompressed)
{
  STRATEGY::Init(pbyteCompressed, cbyteCompressed);

  LONG pixelstride = _size.cx + 4;

  int components = STRATEGY::_info.ilv == ILV_LINE ? STRATEGY::_info.components : 1;

  std::vector<PIXEL> vectmp;
  vectmp.resize((components*2) * pixelstride);

  std::vector<LONG> rgRUNindex;
  rgRUNindex.resize(components);

  for (LONG iline = 0; iline < _size.cy; ++iline)
  {
    ptypePrev      = &vectmp[1];
    ptypeCur      = &vectmp[1 + components* pixelstride];
    if ((iline & 1) == 1)
    {
      std::swap(ptypePrev, ptypeCur);
    }

    STRATEGY::OnLineBegin(_size.cx, ptypeCur, pixelstride);

    for (int component = 0; component < components; ++component)
    {
      RUNindex = rgRUNindex[component];

      // initialize edge pixels used for prediction
      ptypePrev[_size.cx]  = ptypePrev[_size.cx - 1];
      ptypeCur[-1]    = ptypePrev[0];
      DoLine((PIXEL*) NULL); // dummy arg for overload resolution

      rgRUNindex[component] = RUNindex;
      ptypePrev += pixelstride;
      ptypeCur += pixelstride;
    }

    STRATEGY::OnLineEnd(_size.cx, ptypeCur - (components * pixelstride), pixelstride);
  }
}


// Setup codec for encoding and calls DoScan

template<class TRAITS, class STRATEGY>
size_t JlsCodec<TRAITS,STRATEGY>::EncodeScan(const void* pvoid, const Size& size, void* pvoidOut, size_t cbyte, void* pvoidCompare)
{
  STRATEGY::_processLine = InitProcess(const_cast<void*>(pvoid));
  _size = size;

  BYTE* pbyteCompressed = static_cast<BYTE*>(pvoidOut);

  if (pvoidCompare != NULL)
  {
    DecoderStrategy* pdecoder = new JlsCodec<TRAITS,DecoderStrategy>(traits, STRATEGY::_info);
    BYTE* pbyteCompare = (BYTE*)pvoidCompare;
    pdecoder->Init(pbyteCompare, cbyte);
    STRATEGY::_qdecoder = pdecoder;
  }

  STRATEGY::_ptypeUncompressed = pvoid;

  DoScan(pbyteCompressed, cbyte);

  STRATEGY::FlushStreamEnd();

  return  STRATEGY::GetLength();

}

// Factory function for ProcessLine objects to copy/transform unencoded pixels to/from our scanline buffers.

template<class TRAITS, class STRATEGY>
ProcessLine* JlsCodec<TRAITS,STRATEGY>::InitProcess(void* pvoidOut)
{
  if (!IsInterleaved())
    return new PostProcesSingleComponent(pvoidOut, STRATEGY::_info, sizeof(typename TRAITS::PIXEL));

  if (STRATEGY::_info.colorTransform == 0)
    return new ProcessTransformed<TransformNone<typename TRAITS::SAMPLE> >(pvoidOut, STRATEGY::_info, TransformNone<SAMPLE>());

  if (STRATEGY::_info.bitspersample == sizeof(SAMPLE)*8)
  {
    switch(STRATEGY::_info.colorTransform)
    {
      case COLORXFORM_HP1 : return new ProcessTransformed<TransformHp1<SAMPLE> >(pvoidOut, STRATEGY::_info, TransformHp1<SAMPLE>()); break;
      case COLORXFORM_HP2 : return new ProcessTransformed<TransformHp2<SAMPLE> >(pvoidOut, STRATEGY::_info, TransformHp2<SAMPLE>()); break;
      case COLORXFORM_HP3 : return new ProcessTransformed<TransformHp3<SAMPLE> >(pvoidOut, STRATEGY::_info, TransformHp3<SAMPLE>()); break;
      default: throw JlsException(UnsupportedColorTransform);
    }
  } else if (STRATEGY::_info.bitspersample > 8)

  {
    int shift = 16 - STRATEGY::_info.bitspersample;
    switch(STRATEGY::_info.colorTransform)
    {
      case COLORXFORM_HP1 : return new ProcessTransformed<TransformShifted<TransformHp1<USHORT> > >(pvoidOut, STRATEGY::_info, TransformShifted<TransformHp1<USHORT> >(shift)); break;
      case COLORXFORM_HP2 : return new ProcessTransformed<TransformShifted<TransformHp2<USHORT> > >(pvoidOut, STRATEGY::_info, TransformShifted<TransformHp2<USHORT> >(shift)); break;
      case COLORXFORM_HP3 : return new ProcessTransformed<TransformShifted<TransformHp3<USHORT> > >(pvoidOut, STRATEGY::_info, TransformShifted<TransformHp3<USHORT> >(shift)); break;
      default: throw JlsException(UnsupportedColorTransform);
    }
  }
  throw JlsException(UnsupportedBitDepthForTransform);
}


// Setup codec for decoding and calls DoScan

template<class TRAITS, class STRATEGY>
size_t JlsCodec<TRAITS,STRATEGY>::DecodeScan(void* pvoidOut, const Size& size, const void* pvoidIn, size_t cbyte, bool bCompare)
{
  STRATEGY::_processLine = InitProcess(pvoidOut);

  PIXEL* ptypeOut      = static_cast<PIXEL*>(pvoidOut);
  BYTE* pbyteCompressed  = const_cast<BYTE*>(static_cast<const BYTE*>(pvoidIn));
  _bCompare = bCompare;

  BYTE rgbyte[20];

  LONG cbyteRead = 0;
  ::memcpy(rgbyte, pbyteCompressed, 4);
  cbyteRead += 4;

  LONG cbyteScanheader = rgbyte[3] - 2;

  ::memcpy(rgbyte, pbyteCompressed, cbyteScanheader);
  cbyteRead += cbyteScanheader;

  _size = size;

  STRATEGY::_ptypeUncompressed = ptypeOut;
  DoScan(pbyteCompressed + cbyteRead, cbyte - cbyteRead);

  return STRATEGY::GetCurBytePos() - pbyteCompressed;
}

// Initialize the codec data structures. Depends on JPEG-LS parameters like T1-T3.

template<class TRAITS, class STRATEGY>
void JlsCodec<TRAITS,STRATEGY>::InitParams(LONG t1, LONG t2, LONG t3, LONG nReset)
{
  T1 = t1;
  T2 = t2;
  T3 = t3;

  InitQuantizationLUT();

  LONG A = MAX(2, (traits.RANGE + 32)/64);
  for (unsigned int Q = 0; Q < sizeof(_contexts) / sizeof(_contexts[0]); ++Q)
  {
    _contexts[Q] = JlsContext(A);
  }

  _contextRunmode[0] = CContextRunMode(MAX(2, (traits.RANGE + 32)/64), 0, nReset);
  _contextRunmode[1] = CContextRunMode(MAX(2, (traits.RANGE + 32)/64), 1, nReset);
  RUNindex = 0;
}

#endif
