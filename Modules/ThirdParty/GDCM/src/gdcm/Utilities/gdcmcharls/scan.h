// 
// (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use. 
// 

#ifndef CHARLS_SCAN
#define CHARLS_SCAN

#include "lookuptable.h"
#include "contextrunmode.h"
#include "context.h"
#include "colortransform.h"
#include <sstream>

// This file contains the code for handling a "scan". Usually an image is encoded as a single scan.


#ifdef _MSC_VER
#pragma warning (disable: 4127) // conditional expression is constant (caused by some template methods that are not fully specialized) [VS2013]
#endif


extern CTable decodingTables[16];
extern std::vector<signed char> rgquant8Ll;
extern std::vector<signed char> rgquant10Ll;
extern std::vector<signed char> rgquant12Ll;
extern std::vector<signed char> rgquant16Ll;

inlinehint int32_t ApplySign(int32_t i, int32_t sign)
{
    return (sign ^ i) - sign;
}


// Two alternatives for GetPredictedValue() (second is slightly faster due to reduced branching)

#if 0

inlinehint int32_t GetPredictedValue(int32_t Ra, int32_t Rb, int32_t Rc)
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

inlinehint int32_t GetPredictedValue(int32_t Ra, int32_t Rb, int32_t Rc)
{
    // sign trick reduces the number of if statements (branches) 
    int32_t sgn = BitWiseSign(Rb - Ra);

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

inlinehint int32_t UnMapErrVal(int32_t mappedError)
{
    //int32_t sign = ~((mappedError & 1) - 1);
    int32_t sign = int32_t(mappedError << (INT32_BITCOUNT-1)) >> (INT32_BITCOUNT-1);
    return sign ^ (mappedError >> 1);
}

inlinehint int32_t GetMappedErrVal(int32_t Errval)
{
    int32_t mappedError = (Errval >> (INT32_BITCOUNT-2)) ^ (2 * Errval);
    return mappedError;
}

inlinehint  int32_t ComputeContextID(int32_t Q1, int32_t Q2, int32_t Q3)
{
    return (Q1*9 + Q2)*9 + Q3;
}


template<typename TRAITS, typename STRATEGY>
class JlsCodec : public STRATEGY
{
public:
    typedef typename TRAITS::PIXEL PIXEL;
    typedef typename TRAITS::SAMPLE SAMPLE;

    JlsCodec(const TRAITS& inTraits, const JlsParameters& params) :
        STRATEGY(params),
        traits(inTraits),
        _rect(),
        _width(params.width),
        T1(0),
        T2(0),
        T3(0),
        _RUNindex(0),
        _previousLine(),
        _currentLine(),
        _pquant(nullptr),
        _bCompare(false)
    {
        if (Info().interleaveMode == InterleaveMode::None)
        {
            Info().components = 1;
        }
    }

    void SetPresets(const JlsCustomParameters& presets) override
    {
        JlsCustomParameters presetDefault = ComputeDefault(traits.MAXVAL, traits.NEAR);

        InitParams(presets.T1 != 0 ? presets.T1 : presetDefault.T1,
                    presets.T2 != 0 ? presets.T2 : presetDefault.T2,
                    presets.T3 != 0 ? presets.T3 : presetDefault.T3,
                    presets.RESET != 0 ? presets.RESET : presetDefault.RESET);
    }


    bool IsInterleaved()
    {
        if (Info().interleaveMode == InterleaveMode::None)
            return false;

        if (Info().components == 1)
            return false;

        return true;
    }

    JlsParameters& Info()
    {
        return STRATEGY::_params;
    }

    signed char QuantizeGratientOrg(int32_t Di);

    inlinehint int32_t QuantizeGratient(int32_t Di)
    {
        ASSERT(QuantizeGratientOrg(Di) == *(_pquant + Di));
        return *(_pquant + Di);
    }

    void InitQuantizationLUT();
    
    int32_t DecodeValue(int32_t k, int32_t limit, int32_t qbpp);
    inlinehint void EncodeMappedValue(int32_t k, int32_t mappedError, int32_t limit);

    void IncrementRunIndex()
    {
        _RUNindex = MIN(31,_RUNindex + 1);
    }

    void DecrementRunIndex()
      { _RUNindex = MAX(0,_RUNindex - 1); }

    int32_t DecodeRIError(CContextRunMode& ctx);
    Triplet<SAMPLE> DecodeRIPixel(Triplet<SAMPLE> Ra, Triplet<SAMPLE> Rb);
    SAMPLE DecodeRIPixel(int32_t Ra, int32_t Rb);
    int32_t DecodeRunPixels(PIXEL Ra, PIXEL* ptype, int32_t cpixelMac);
    int32_t DoRunMode(int32_t index, DecoderStrategy*);

    void EncodeRIError(CContextRunMode& ctx, int32_t Errval);
    SAMPLE EncodeRIPixel(int32_t x, int32_t Ra, int32_t Rb);
    Triplet<SAMPLE> EncodeRIPixel(Triplet<SAMPLE> x, Triplet<SAMPLE> Ra, Triplet<SAMPLE> Rb);
    void EncodeRunPixels(int32_t runLength, bool bEndofline);
    int32_t DoRunMode(int32_t index, EncoderStrategy*);

    inlinehint SAMPLE DoRegular(int32_t Qs, int32_t, int32_t pred, DecoderStrategy*);
    inlinehint SAMPLE DoRegular(int32_t Qs, int32_t x, int32_t pred, EncoderStrategy*);

    void DoLine(SAMPLE* pdummy);
    void DoLine(Triplet<SAMPLE>* pdummy);
    void DoScan();

public:
    ProcessLine* CreateProcess(ByteStreamInfo rawStreamInfo) override;
    void InitDefault();
    void InitParams(int32_t t1, int32_t t2, int32_t t3, int32_t nReset);

#if defined(__clang__) && (__clang_major__ > 6)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winconsistent-missing-override"
#endif

    size_t EncodeScan(std::unique_ptr<ProcessLine> rawData, ByteStreamInfo& compressedData, void* pvoidCompare);
    void DecodeScan(std::unique_ptr<ProcessLine> rawData, const JlsRect& size, ByteStreamInfo& compressedData, bool bCompare);

#if defined(__clang__) && (__clang_major__ > 6)
#pragma clang diagnostic pop
#endif

protected:
    // codec parameters
    TRAITS traits;
    JlsRect _rect;
    int _width;
    int32_t T1;
    int32_t T2;
    int32_t T3; 

    // compression context
    JlsContext _contexts[365];
    CContextRunMode _contextRunmode[2];
    int32_t _RUNindex;
    PIXEL* _previousLine; // previous line ptr
    PIXEL* _currentLine; // current line ptr

    // quantization lookup table
    signed char* _pquant;
    std::vector<signed char> _rgquant;

    // debugging
    bool _bCompare;
};


// Encode/decode a single sample. Performancewise the #1 important functions
template<typename TRAITS, typename STRATEGY>
typename TRAITS::SAMPLE JlsCodec<TRAITS,STRATEGY>::DoRegular(int32_t Qs, int32_t, int32_t pred, DecoderStrategy*)
{
    int32_t sign = BitWiseSign(Qs);
    JlsContext& ctx = _contexts[ApplySign(Qs, sign)];
    int32_t k = ctx.GetGolomb();	
    int32_t Px = traits.CorrectPrediction(pred + ApplySign(ctx.C, sign));

    int32_t ErrVal;
    const Code& code = decodingTables[k].Get(STRATEGY::PeekByte());
    if (code.GetLength() != 0)
    {
        STRATEGY::Skip(code.GetLength());
        ErrVal = code.GetValue(); 
        ASSERT(std::abs(ErrVal) < 65535);
    }
    else
    {
        ErrVal = UnMapErrVal(DecodeValue(k, traits.LIMIT, traits.qbpp)); 
        if (std::abs(ErrVal) > 65535)
            throw std::system_error(static_cast<int>(charls::ApiResult::InvalidCompressedData), CharLSCategoryInstance());
    }
    if (k == 0)
    {
        ErrVal = ErrVal ^ ctx.GetErrorCorrection(traits.NEAR);
    }
    ctx.UpdateVariables(ErrVal, traits.NEAR, traits.RESET);
    ErrVal = ApplySign(ErrVal, sign);
    return traits.ComputeReconstructedSample(Px, ErrVal); 
}


template<typename TRAITS, typename STRATEGY>
typename TRAITS::SAMPLE JlsCodec<TRAITS,STRATEGY>::DoRegular(int32_t Qs, int32_t x, int32_t pred, EncoderStrategy*)
{
    int32_t sign = BitWiseSign(Qs);
    JlsContext& ctx = _contexts[ApplySign(Qs, sign)];
    int32_t k = ctx.GetGolomb();
    int32_t Px = traits.CorrectPrediction(pred + ApplySign(ctx.C, sign));
    int32_t ErrVal = traits.ComputeErrVal(ApplySign(x - Px, sign));

    EncodeMappedValue(k, GetMappedErrVal(ctx.GetErrorCorrection(k | traits.NEAR) ^ ErrVal), traits.LIMIT);
    ctx.UpdateVariables(ErrVal, traits.NEAR, traits.RESET);
    ASSERT(traits.IsNear(traits.ComputeReconstructedSample(Px, ApplySign(ErrVal, sign)), x));
    return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Px, ApplySign(ErrVal, sign)));
}


// Functions to build tables used to decode short golomb codes.

inlinehint std::pair<int32_t, int32_t> CreateEncodedValue(int32_t k, int32_t mappedError)
{
    int32_t highbits = mappedError >> k;
    return std::make_pair(highbits + k + 1, (int32_t(1) << k) | (mappedError & ((int32_t(1) << k) - 1)));
}


inline CTable InitTable(int32_t k)
{
    CTable table;
    for (short nerr = 0; ; nerr++)
    {
        // Q is not used when k != 0
        int32_t merrval = GetMappedErrVal(nerr);//, k, -1);
        std::pair<int32_t, int32_t> paircode = CreateEncodedValue(k, merrval);
        if (paircode.first > CTable::cbit)
            break;

        Code code = Code( nerr, short(paircode.first) );
        table.AddEntry(uint8_t(paircode.second), code);
    }

    for (short nerr = -1; ; nerr--)
    {
        // Q is not used when k != 0
        int32_t merrval = GetMappedErrVal(nerr);//, k, -1);
        std::pair<int32_t, int32_t> paircode = CreateEncodedValue(k, merrval);
        if (paircode.first > CTable::cbit)
            break;

        Code code = Code(nerr, short(paircode.first));
        table.AddEntry(uint8_t(paircode.second), code);
    }

    return table;
}


// Encoding/decoding of golomb codes

template<typename TRAITS, typename STRATEGY>
int32_t JlsCodec<TRAITS, STRATEGY>::DecodeValue(int32_t k, int32_t limit, int32_t qbpp)
{
    int32_t highbits = STRATEGY::ReadHighbits();

    if (highbits >= limit - (qbpp + 1))
        return STRATEGY::ReadValue(qbpp) + 1;

    if (k == 0)
        return highbits;

    return (highbits << k) + STRATEGY::ReadValue(k);
}


template<typename TRAITS, typename STRATEGY>
inlinehint void JlsCodec<TRAITS, STRATEGY>::EncodeMappedValue(int32_t k, int32_t mappedError, int32_t limit)
{
    int32_t highbits = mappedError >> k;

    if (highbits < limit - traits.qbpp - 1)
    {
        if (highbits + 1 > 31)
        {
            STRATEGY::AppendToBitStream(0, highbits / 2);
            highbits = highbits - highbits / 2;
        }
        STRATEGY::AppendToBitStream(1, highbits + 1);
        STRATEGY::AppendToBitStream((mappedError & ((1 << k) - 1)), k);
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
    STRATEGY::AppendToBitStream((mappedError - 1) & ((1 << traits.qbpp) - 1), traits.qbpp);
}


// Disable the Microsoft Static Analyzer warning: Potential comparison of a constant with another constant. (false warning, triggered by template construction)
#ifdef _PREFAST_
#pragma warning(push)
#pragma warning(disable:6326)
#endif

// Sets up a lookup table to "Quantize" sample difference.

template<typename TRAITS, typename STRATEGY>
void JlsCodec<TRAITS, STRATEGY>::InitQuantizationLUT()
{
    // for lossless mode with default parameters, we have precomputed the luts for bitcounts 8, 10, 12 and 16.
    if (traits.NEAR == 0 && traits.MAXVAL == (1 << traits.bpp) - 1)
    {
        JlsCustomParameters presets = ComputeDefault(traits.MAXVAL, traits.NEAR);
        if (presets.T1 == T1 && presets.T2 == T2 && presets.T3 == T3)
        {
            if (traits.bpp == 8)
            {
                _pquant = &rgquant8Ll[rgquant8Ll.size() / 2];
                return;
            }
            if (traits.bpp == 10)
            {
                _pquant = &rgquant10Ll[rgquant10Ll.size() / 2];
                return;
            }
            if (traits.bpp == 12)
            {
                _pquant = &rgquant12Ll[rgquant12Ll.size() / 2];
                return;
            }
            if (traits.bpp == 16)
            {
                _pquant = &rgquant16Ll[rgquant16Ll.size() / 2];
                return;
            }
        }
    }

    int32_t RANGE = 1 << traits.bpp;

    _rgquant.resize(RANGE * 2);

    _pquant = &_rgquant[RANGE];
    for (int32_t i = -RANGE; i < RANGE; ++i)
    {
        _pquant[i] = QuantizeGratientOrg(i);
    }
}

#ifdef _PREFAST_
#pragma warning(pop)
#endif

template<typename TRAITS, typename STRATEGY>
signed char JlsCodec<TRAITS,STRATEGY>::QuantizeGratientOrg(int32_t Di)
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

template<typename TRAITS, typename STRATEGY>
int32_t JlsCodec<TRAITS,STRATEGY>::DecodeRIError(CContextRunMode& ctx)
{
    int32_t k = ctx.GetGolomb();
    int32_t EMErrval = DecodeValue(k, traits.LIMIT - J[_RUNindex]-1, traits.qbpp);
    int32_t Errval = ctx.ComputeErrVal(EMErrval + ctx._nRItype, k);
    ctx.UpdateVariables(Errval, EMErrval);
    return Errval;
}


template<typename TRAITS, typename STRATEGY>
void JlsCodec<TRAITS,STRATEGY>::EncodeRIError(CContextRunMode& ctx, int32_t Errval)
{
    int32_t k = ctx.GetGolomb();
    bool map = ctx.ComputeMap(Errval, k);
    int32_t EMErrval = 2 * std::abs(Errval) - ctx._nRItype - int32_t(map);

    ASSERT(Errval == ctx.ComputeErrVal(EMErrval + ctx._nRItype, k));
    EncodeMappedValue(k, EMErrval, traits.LIMIT-J[_RUNindex]-1);
    ctx.UpdateVariables(Errval, EMErrval);
}


template<typename TRAITS, typename STRATEGY>
Triplet<typename TRAITS::SAMPLE> JlsCodec<TRAITS,STRATEGY>::DecodeRIPixel(Triplet<SAMPLE> Ra, Triplet<SAMPLE> Rb)
{ 
    int32_t Errval1 = DecodeRIError(_contextRunmode[0]);
    int32_t Errval2 = DecodeRIError(_contextRunmode[0]);
    int32_t Errval3 = DecodeRIError(_contextRunmode[0]);

    return Triplet<SAMPLE>(traits.ComputeReconstructedSample(Rb.v1, Errval1 * Sign(Rb.v1  - Ra.v1)),
        traits.ComputeReconstructedSample(Rb.v2, Errval2 * Sign(Rb.v2  - Ra.v2)),
        traits.ComputeReconstructedSample(Rb.v3, Errval3 * Sign(Rb.v3  - Ra.v3)));
}


template<typename TRAITS, typename STRATEGY>
Triplet<typename TRAITS::SAMPLE> JlsCodec<TRAITS,STRATEGY>::EncodeRIPixel(Triplet<SAMPLE> x, Triplet<SAMPLE> Ra, Triplet<SAMPLE> Rb)
{
    int32_t errval1 = traits.ComputeErrVal(Sign(Rb.v1 - Ra.v1) * (x.v1 - Rb.v1));
    EncodeRIError(_contextRunmode[0], errval1);

    int32_t errval2 = traits.ComputeErrVal(Sign(Rb.v2 - Ra.v2) * (x.v2 - Rb.v2));
    EncodeRIError(_contextRunmode[0], errval2);

    int32_t errval3 = traits.ComputeErrVal(Sign(Rb.v3 - Ra.v3) * (x.v3 - Rb.v3));
    EncodeRIError(_contextRunmode[0], errval3);

    return Triplet<SAMPLE>(traits.ComputeReconstructedSample(Rb.v1, errval1 * Sign(Rb.v1  - Ra.v1)),
        traits.ComputeReconstructedSample(Rb.v2, errval2 * Sign(Rb.v2  - Ra.v2)),
        traits.ComputeReconstructedSample(Rb.v3, errval3 * Sign(Rb.v3  - Ra.v3)));
}


template<typename TRAITS, typename STRATEGY>
typename TRAITS::SAMPLE JlsCodec<TRAITS,STRATEGY>::DecodeRIPixel(int32_t Ra, int32_t Rb)
{
    if (std::abs(Ra - Rb) <= traits.NEAR)
    {
        int32_t ErrVal = DecodeRIError(_contextRunmode[1]);
        return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Ra, ErrVal));
    }
    else
    {
        int32_t ErrVal = DecodeRIError(_contextRunmode[0]);
        return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Rb, ErrVal * Sign(Rb - Ra)));
    }
}


template<typename TRAITS, typename STRATEGY>
typename TRAITS::SAMPLE JlsCodec<TRAITS,STRATEGY>::EncodeRIPixel(int32_t x, int32_t Ra, int32_t Rb)
{
    if (std::abs(Ra - Rb) <= traits.NEAR)
    {
        int32_t ErrVal	= traits.ComputeErrVal(x - Ra);
        EncodeRIError(_contextRunmode[1], ErrVal);
        return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Ra, ErrVal));
    }
    else
    {
        int32_t ErrVal	= traits.ComputeErrVal((x - Rb) * Sign(Rb - Ra));
        EncodeRIError(_contextRunmode[0], ErrVal);
        return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Rb, ErrVal * Sign(Rb - Ra)));
    }
}


// RunMode: Functions that handle run-length encoding

template<typename TRAITS, typename STRATEGY>
void JlsCodec<TRAITS, STRATEGY>::EncodeRunPixels(int32_t runLength, bool endOfLine)
{
    while (runLength >= int32_t(1 << J[_RUNindex])) 
    {
        STRATEGY::AppendOnesToBitStream(1);
        runLength = runLength - int32_t(1 << J[_RUNindex]);
        IncrementRunIndex();
    }

    if (endOfLine) 
    {
        if (runLength != 0) 
        {
            STRATEGY::AppendOnesToBitStream(1);	
        }
    }
    else
    {
        STRATEGY::AppendToBitStream(runLength, J[_RUNindex] + 1); // leading 0 + actual remaining length
    }
}


template<typename TRAITS, typename STRATEGY>
int32_t JlsCodec<TRAITS, STRATEGY>::DecodeRunPixels(PIXEL Ra, PIXEL* startPos, int32_t cpixelMac)
{
    int32_t index = 0;
    while (STRATEGY::ReadBit())
    {
        int count = MIN(1 << J[_RUNindex], int(cpixelMac - index));
        index += count;
        ASSERT(index <= cpixelMac);

        if (count == (1 << J[_RUNindex]))
        {
            IncrementRunIndex();
        }

        if (index == cpixelMac)
            break;
    }

    if (index != cpixelMac)
    {
        // incomplete run.
        index += (J[_RUNindex] > 0) ? STRATEGY::ReadValue(J[_RUNindex]) : 0;
    }

    if (index > cpixelMac)
        throw std::system_error(static_cast<int>(charls::ApiResult::InvalidCompressedData), CharLSCategoryInstance());

    for (int32_t i = 0; i < index; ++i)
    {
        startPos[i] = Ra;
    }

    return index;
}

template<typename TRAITS, typename STRATEGY>
int32_t JlsCodec<TRAITS, STRATEGY>::DoRunMode(int32_t index, EncoderStrategy*)
{
    int32_t ctypeRem = _width - index;
    PIXEL* ptypeCurX = _currentLine + index;
    PIXEL* ptypePrevX = _previousLine + index;

    PIXEL Ra = ptypeCurX[-1];

    int32_t runLength = 0;

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


template<typename TRAITS, typename STRATEGY>
int32_t JlsCodec<TRAITS, STRATEGY>::DoRunMode(int32_t startIndex, DecoderStrategy*)
{
    PIXEL Ra = _currentLine[startIndex-1];

    int32_t runLength = DecodeRunPixels(Ra, _currentLine + startIndex, _width - startIndex);
    int32_t endIndex = startIndex + runLength;

    if (endIndex == _width)
        return endIndex - startIndex;

    // run interruption
    PIXEL Rb = _previousLine[endIndex];
    _currentLine[endIndex] = DecodeRIPixel(Ra, Rb);
    DecrementRunIndex();
    return endIndex - startIndex + 1;
}


// DoLine: Encodes/Decodes a scanline of samples

template<typename TRAITS, typename STRATEGY>
void JlsCodec<TRAITS, STRATEGY>::DoLine(SAMPLE*)
{
    int32_t index = 0;
    int32_t Rb = _previousLine[index-1];
    int32_t Rd = _previousLine[index];

    while(index < _width)
    {
        int32_t Ra = _currentLine[index -1];
        int32_t Rc = Rb;
        Rb = Rd;
        Rd = _previousLine[index + 1];

        int32_t Qs = ComputeContextID(QuantizeGratient(Rd - Rb), QuantizeGratient(Rb - Rc), QuantizeGratient(Rc - Ra));

        if (Qs != 0)
        {
            _currentLine[index] = DoRegular(Qs, _currentLine[index], GetPredictedValue(Ra, Rb, Rc), static_cast<STRATEGY*>(nullptr));
            index++;
        }
        else
        {
            index += DoRunMode(index, static_cast<STRATEGY*>(nullptr));
            Rb = _previousLine[index - 1];
            Rd = _previousLine[index];
        }
    }
}


// DoLine: Encodes/Decodes a scanline of triplets in ILV_SAMPLE mode

template<typename TRAITS, typename STRATEGY>
void JlsCodec<TRAITS, STRATEGY>::DoLine(Triplet<SAMPLE>*)
{
    int32_t index = 0;
    while(index < _width)
    {
        Triplet<SAMPLE> Ra = _currentLine[index - 1];
        Triplet<SAMPLE> Rc = _previousLine[index - 1];
        Triplet<SAMPLE> Rb = _previousLine[index];
        Triplet<SAMPLE> Rd = _previousLine[index + 1];

        int32_t Qs1 = ComputeContextID(QuantizeGratient(Rd.v1 - Rb.v1), QuantizeGratient(Rb.v1 - Rc.v1), QuantizeGratient(Rc.v1 - Ra.v1));
        int32_t Qs2 = ComputeContextID(QuantizeGratient(Rd.v2 - Rb.v2), QuantizeGratient(Rb.v2 - Rc.v2), QuantizeGratient(Rc.v2 - Ra.v2));
        int32_t Qs3 = ComputeContextID(QuantizeGratient(Rd.v3 - Rb.v3), QuantizeGratient(Rb.v3 - Rc.v3), QuantizeGratient(Rc.v3 - Ra.v3));

        if (Qs1 == 0 && Qs2 == 0 && Qs3 == 0)
        {
            index += DoRunMode(index, static_cast<STRATEGY*>(nullptr));
        }
        else
        {
            Triplet<SAMPLE> Rx;
            Rx.v1 = DoRegular(Qs1, _currentLine[index].v1, GetPredictedValue(Ra.v1, Rb.v1, Rc.v1), static_cast<STRATEGY*>(nullptr));
            Rx.v2 = DoRegular(Qs2, _currentLine[index].v2, GetPredictedValue(Ra.v2, Rb.v2, Rc.v2), static_cast<STRATEGY*>(nullptr));
            Rx.v3 = DoRegular(Qs3, _currentLine[index].v3, GetPredictedValue(Ra.v3, Rb.v3, Rc.v3), static_cast<STRATEGY*>(nullptr));
            _currentLine[index] = Rx;
            index++;
        }
    }
}


// DoScan: Encodes or decodes a scan. 
// In ILV_SAMPLE mode, multiple components are handled in DoLine
// In ILV_LINE mode, a call do DoLine is made for every component
// In ILV_NONE mode, DoScan is called for each component 

template<typename TRAITS, typename STRATEGY>
void JlsCodec<TRAITS, STRATEGY>::DoScan()
{
    int32_t pixelstride = _width + 4;
    int components = Info().interleaveMode == charls::InterleaveMode::Line ? Info().components : 1;

    std::vector<PIXEL> vectmp(2 * components * pixelstride);
    std::vector<int32_t> rgRUNindex(components);

    for (int32_t line = 0; line < Info().height; ++line)
    {
        _previousLine = &vectmp[1];
        _currentLine = &vectmp[1 + components * pixelstride];
        if ((line & 1) == 1)
        {
            std::swap(_previousLine, _currentLine);
        }

        STRATEGY::OnLineBegin(_width, _currentLine, pixelstride);

        for (int component = 0; component < components; ++component)
        {
            _RUNindex = rgRUNindex[component];

            // initialize edge pixels used for prediction
            _previousLine[_width] = _previousLine[_width - 1];
            _currentLine[-1] = _previousLine[0];
            DoLine(static_cast<PIXEL*>(nullptr)); // dummy arg for overload resolution

            rgRUNindex[component] = _RUNindex;
            _previousLine += pixelstride;
            _currentLine += pixelstride;
        }

        if (_rect.Y <= line && line < _rect.Y + _rect.Height)
        {
            STRATEGY::OnLineEnd(_rect.Width, _currentLine + _rect.X - (components * pixelstride), pixelstride);
        }
    }

    STRATEGY::EndScan();
}


// Factory function for ProcessLine objects to copy/transform unencoded pixels to/from our scanline buffers.

template<typename TRAITS, typename STRATEGY>
ProcessLine* JlsCodec<TRAITS, STRATEGY>::CreateProcess(ByteStreamInfo info)
{
    if (!IsInterleaved())
    {
        return info.rawData ?
            static_cast<ProcessLine*>(new PostProcesSingleComponent(info.rawData, Info(), sizeof(typename TRAITS::PIXEL))) :
            static_cast<ProcessLine*>(new PostProcesSingleStream(info.rawStream, Info(), sizeof(typename TRAITS::PIXEL)));
    }

    if (Info().colorTransformation == ColorTransformation::None)
        return new ProcessTransformed<TransformNone<typename TRAITS::SAMPLE> >(info, Info(), TransformNone<SAMPLE>()); 

    if (Info().bitsPerSample == sizeof(SAMPLE) * 8)
    {
        switch (Info().colorTransformation)
        {
            case ColorTransformation::HP1: return new ProcessTransformed<TransformHp1<SAMPLE>>(info, Info(), TransformHp1<SAMPLE>());
            case ColorTransformation::HP2: return new ProcessTransformed<TransformHp2<SAMPLE>>(info, Info(), TransformHp2<SAMPLE>());
            case ColorTransformation::HP3: return new ProcessTransformed<TransformHp3<SAMPLE>>(info, Info(), TransformHp3<SAMPLE>());
            default:
                std::ostringstream message;
                message << "Color transformation " << Info().colorTransformation << " is not supported.";
                throw CreateSystemError(ApiResult::UnsupportedColorTransform, message.str());
        }
    }

    if (Info().bitsPerSample > 8)
    {
        int shift = 16 - Info().bitsPerSample;
        switch (Info().colorTransformation)
        {
            case ColorTransformation::HP1: return new ProcessTransformed<TransformShifted<TransformHp1<uint16_t>>>(info, Info(), TransformShifted<TransformHp1<uint16_t>>(shift));
            case ColorTransformation::HP2: return new ProcessTransformed<TransformShifted<TransformHp2<uint16_t>>>(info, Info(), TransformShifted<TransformHp2<uint16_t>>(shift));
            case ColorTransformation::HP3: return new ProcessTransformed<TransformShifted<TransformHp3<uint16_t>>>(info, Info(), TransformShifted<TransformHp3<uint16_t>>(shift));
            default:
                std::ostringstream message;
                message << "Color transformation " << Info().colorTransformation << " is not supported.";
                throw CreateSystemError(ApiResult::UnsupportedColorTransform, message.str());
        }
    }

    throw std::system_error(static_cast<int>(ApiResult::UnsupportedBitDepthForTransform), CharLSCategoryInstance());
}


// Setup codec for encoding and calls DoScan

template<typename TRAITS, typename STRATEGY>
size_t JlsCodec<TRAITS, STRATEGY>::EncodeScan(std::unique_ptr<ProcessLine> processLine, ByteStreamInfo& compressedData, void* pvoidCompare)
{
    STRATEGY::_processLine = std::move(processLine);

    ByteStreamInfo info = { nullptr, static_cast<uint8_t*>(pvoidCompare), compressedData.count };
    if (pvoidCompare)
    {
        STRATEGY::_qdecoder = std::unique_ptr<DecoderStrategy>(new JlsCodec<TRAITS, DecoderStrategy>(traits, Info()));
        STRATEGY::_qdecoder->Init(info);
    }

    STRATEGY::Init(compressedData);

    DoScan();

    return STRATEGY::GetLength();
}

// Setup codec for decoding and calls DoScan


template<typename TRAITS, typename STRATEGY>
void JlsCodec<TRAITS, STRATEGY>::DecodeScan(std::unique_ptr<ProcessLine> processLine, const JlsRect& rect, ByteStreamInfo& compressedData, bool bCompare)
{
    STRATEGY::_processLine = std::move(processLine);

    uint8_t* compressedBytes = const_cast<uint8_t*>(static_cast<const uint8_t*>(compressedData.rawData));
    _bCompare = bCompare;
    _rect = rect;

    STRATEGY::Init(compressedData);
    DoScan();
    SkipBytes(compressedData, STRATEGY::GetCurBytePos() - compressedBytes);
}


// Initialize the codec data structures. Depends on JPEG-LS parameters like T1-T3.
template<typename TRAITS, typename STRATEGY>
void JlsCodec<TRAITS, STRATEGY>::InitParams(int32_t t1, int32_t t2, int32_t t3, int32_t nReset)
{
    T1 = t1;
    T2 = t2;
    T3 = t3;

    InitQuantizationLUT();

    int32_t A = MAX(2, (traits.RANGE + 32)/64);
    for (unsigned int Q = 0; Q < sizeof(_contexts) / sizeof(_contexts[0]); ++Q)
    {
        _contexts[Q] = JlsContext(A);
    }

    _contextRunmode[0] = CContextRunMode(MAX(2, (traits.RANGE + 32)/64), 0, nReset);
    _contextRunmode[1] = CContextRunMode(MAX(2, (traits.RANGE + 32)/64), 1, nReset);
    _RUNindex = 0;
}

#endif
