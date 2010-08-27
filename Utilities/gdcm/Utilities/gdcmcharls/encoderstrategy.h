//
// (C) Jan de Vaan 2007-2009, all rights reserved. See the accompanying "License.txt" for licensed use.
//

#ifndef CHARLS_ENCODERSTRATEGY
#define CHARLS_ENCODERSTRATEGY

#include "processline.h"
#include "decoderstrategy.h"

// Implements encoding to stream of bits. In encoding mode JpegLsCodec inherits from EncoderStrategy

class EncoderStrategy
{

public:
  explicit EncoderStrategy(const JlsParamaters& info) :
     _qdecoder(0),
     _info(info),
     _processLine(0),
      valcurrent(0),
     bitpos(0),
     _bFFWritten(false),
     _cbyteWritten(0)

  {
  }

  virtual ~EncoderStrategy()
  {
    delete _qdecoder;
      delete _processLine;
  }

  LONG PeekByte();

  void OnLineBegin(LONG cpixel, void* ptypeBuffer, LONG pixelStride)
  {
    _processLine->NewLineRequested(ptypeBuffer, cpixel, pixelStride);
  }

  void OnLineEnd(LONG /*cpixel*/, void* /*ptypeBuffer*/, LONG /*pixelStride*/) {}

    virtual void SetPresets(const JlsCustomParameters& presets) = 0;

  virtual size_t EncodeScan(const void* pvoid, const Size& size, void* pvoidOut, size_t cbyte, void* pvoidCompare) = 0;

protected:

  void Init(BYTE* pbyteCompressed, size_t cbyte)
  {
    bitpos = 32;
    valcurrent = 0;
    _pbyteCompressed = pbyteCompressed;
       _cbyteCompressed = cbyte;
  }


  void AppendToBitStream(LONG value, LONG length)
  {
    ASSERT(length < 32 && length >= 0);

    ASSERT((_qdecoder == NULL) || (length == 0 && value == 0) ||( _qdecoder->ReadLongValue(length) == value));

#ifdef _DEBUG
    if (length < 32)
    {
      int mask = (1 << (length)) - 1;
      ASSERT((value | mask) == mask);
    }
#endif

    bitpos -= length;
    if (bitpos >= 0)
    {
      valcurrent = valcurrent | (value << bitpos);
      return;
    }
    valcurrent |= value >> -bitpos;

    Flush();

    ASSERT(bitpos >=0);
    valcurrent |= value << bitpos;

  }

  void FlushStreamEnd()
  {
    Flush();
    // if a 0xff was written, Flush() will force one unset bit anyway
    if (_bFFWritten)
      AppendToBitStream(0, (bitpos - 1) % 8);
    else
      AppendToBitStream(0, bitpos % 8);
    ASSERT(bitpos % 8 == 0);
    Flush();
    ASSERT(bitpos == 0x20);
  }

  void Flush()
  {
    for (LONG i = 0; i < 4; ++i)
    {
      if (bitpos >= 32)
        break;

      if (_bFFWritten)
      {
        // insert highmost bit
        *_pbyteCompressed = BYTE(valcurrent >> 25);
        valcurrent = valcurrent << 7;
        bitpos += 7;
        _bFFWritten = false;
      }
      else
      {
        *_pbyteCompressed = BYTE(valcurrent >> 24);
        valcurrent = valcurrent << 8;
        bitpos += 8;
        _bFFWritten = *_pbyteCompressed == 0xFF;
      }

      _pbyteCompressed++;
      _cbyteCompressed--;
      _cbyteWritten++;

    }

  }

  size_t GetLength()
  {
    return _cbyteWritten - (bitpos -32)/8;
  }


  inlinehint void AppendOnesToBitStream(LONG length)
  {
    AppendToBitStream((1 << length) - 1, length);
  }


  DecoderStrategy* _qdecoder;

protected:
  JlsParamaters _info;
  const void* _ptypeUncompressed;
  ProcessLine* _processLine;
private:

  unsigned int valcurrent;
  LONG bitpos;
  size_t _cbyteCompressed;

  // encoding
  BYTE* _pbyteCompressed;
  bool _bFFWritten;
  size_t _cbyteWritten;

};

#endif
