//
// (C) Jan de Vaan 2007-2009, all rights reserved. See the accompanying "License.txt" for licensed use.
//

#ifndef CHARLS_DECODERSTATEGY
#define CHARLS_DECODERSTATEGY

#include "streams.h"
#include "processline.h"
#include "config.h"
#include "util.h"

// Implements encoding to stream of bits. In encoding mode JpegLsCodec inherits from EncoderStrategy



class DecoderStrategy
{
public:
  DecoderStrategy(const JlsParamaters& info) :
      _info(info),
            _processLine(0),
      _readCache(0),
      _validBits(0),
      _pbyteCompressed(0)
    {
    }

    virtual ~DecoderStrategy()
    {
      delete _processLine;
    }

    virtual void SetPresets(const JlsCustomParameters& presets) = 0;
    virtual size_t DecodeScan(void* pvoidOut, const Size& size, const void* pvoidIn, size_t cbyte, bool bCheck) = 0;

    void Init(BYTE* pbyteCompressed, size_t cbyte)
    {
      _validBits = 0;
      _readCache = 0;
      _pbyteCompressed = pbyteCompressed;
      _pbyteCompressedEnd = pbyteCompressed + cbyte;
      _pbyteNextFF = FindNextFF();
      MakeValid();
    }

    inlinehint void Skip(LONG length)
    {
      _validBits -= length;
      _readCache = _readCache << length;
    }


    void OnLineBegin(LONG /*cpixel*/, void* /*ptypeBuffer*/, LONG /*pixelStride*/)
    {}


    void OnLineEnd(LONG cpixel, const void* ptypeBuffer, LONG pixelStride)
    {
        _processLine->NewLineDecoded(ptypeBuffer, cpixel, pixelStride);
    }


    inlinehint bool OptimizedRead()
    {
      // Easy & fast: if there is no 0xFF byte in sight, we can read without bitstuffing
      if (_pbyteCompressed < _pbyteNextFF)
      {
        _readCache     |= FromBigEndian<sizeof(bufType)>::Read(_pbyteCompressed) >> _validBits;
        int bytesToRead = (bufferbits - _validBits) >> 3;
        _pbyteCompressed += bytesToRead;
        _validBits += bytesToRead * 8;
        ASSERT(_validBits >= bufferbits - 8);
        return true;
      }
      return false;
    }

    typedef size_t bufType;

    enum {
      bufferbits = sizeof( bufType ) * 8
    };

    void MakeValid()
    {
      ASSERT(_validBits <=bufferbits - 8);

      if (OptimizedRead())
        return;

      do
      {
        if (_pbyteCompressed >= _pbyteCompressedEnd)
        {
          if (_validBits <= 0)
            throw JlsException(InvalidCompressedData);

          return;
        }

        bufType valnew    = _pbyteCompressed[0];

        if (valnew == 0xFF)
        {
          // JPEG bitstream rule: no FF may be followed by 0x80 or higher
         if (_pbyteCompressed == _pbyteCompressedEnd - 1 || (_pbyteCompressed[1] & 0x80) != 0)
         {
           if (_validBits <= 0)
             throw JlsException(InvalidCompressedData);

           return;
           }
        }

        _readCache     |= valnew << (bufferbits - 8  - _validBits);
        _pbyteCompressed   += 1;
        _validBits     += 8;

        if (valnew == 0xFF)
        {
          _validBits--;
        }
      }
      while (_validBits < bufferbits - 8);

      _pbyteNextFF = FindNextFF();
      return;

    }


    BYTE* FindNextFF()
    {
      BYTE* pbyteNextFF =_pbyteCompressed;

      while (pbyteNextFF < _pbyteCompressedEnd)
        {
        if (*pbyteNextFF == 0xFF)
        {
          break;
        }
          pbyteNextFF++;
      }


      return pbyteNextFF - (sizeof(bufType)-1);
    }


    BYTE* GetCurBytePos() const
    {
      LONG  cbitValid = _validBits;
      BYTE* pbyteCompressed = _pbyteCompressed;

      for (;;)
      {
        LONG cbitLast = pbyteCompressed[-1] == 0xFF ? 7 : 8;

        if (cbitValid < cbitLast )
          return pbyteCompressed;

        cbitValid -= cbitLast;
        pbyteCompressed--;
      }
    }


    inlinehint LONG ReadValue(LONG length)
    {
      if (_validBits < length)
      {
        MakeValid();
      }

      ASSERT(length != 0 && length <= _validBits);
      ASSERT(length < 32);
      LONG result = LONG(_readCache >> (bufferbits - length));
      Skip(length);
      return result;
    }


    inlinehint LONG PeekByte()
    {
      if (_validBits < 8)
      {
        MakeValid();
      }

      return _readCache >> (bufferbits - 8);
    }



    inlinehint bool ReadBit()
    {
      if (_validBits <= 0)
      {
        MakeValid();
      }

      bool bSet = (_readCache & (bufType(1) << (bufferbits - 1))) != 0;
      Skip(1);
      return bSet;
    }



    inlinehint LONG Peek0Bits()
    {
      if (_validBits < 16)
      {
        MakeValid();
      }
      bufType valTest = _readCache;

      for (LONG cbit = 0; cbit < 16; cbit++)
      {
        if ((valTest & (bufType(1) << (bufferbits - 1))) != 0)
          return cbit;

        valTest <<= 1;
      }
      return -1;
    }



    inlinehint LONG ReadHighbits()
    {
      LONG cbit = Peek0Bits();
      if (cbit >= 0)
      {
        Skip(cbit + 1);
        return cbit;
      }
      Skip(15);

      for (LONG highbits = 15; ; highbits++)
      {
        if (ReadBit())
          return highbits;
      }
    }


    LONG ReadLongValue(LONG length)
    {
      if (length <= 24)
        return ReadValue(length);

      return (ReadValue(length - 24) << 24) + ReadValue(24);
    }

protected:
  JlsParamaters _info;
  void* _ptypeUncompressed;
  ProcessLine* _processLine;

private:
  // decoding
  bufType _readCache;
  LONG _validBits;
  BYTE* _pbyteCompressed;
  BYTE* _pbyteNextFF;
  BYTE* _pbyteCompressedEnd;
};


#endif
