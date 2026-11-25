// 
// (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use. 
// 

#ifndef CHARLS_DECODERSTATEGY
#define CHARLS_DECODERSTATEGY


#include "util.h"
#include "processline.h"
#include <memory>

// Purpose: Implements encoding to stream of bits. In encoding mode JpegLsCodec inherits from EncoderStrategy
class DecoderStrategy
{
public:
    DecoderStrategy(const JlsParameters& params) :
        _params(params),
        _byteStream(nullptr),
        _readCache(0),
        _validBits(0),
        _position(nullptr),
        _nextFFPosition(nullptr),
        _endPosition(nullptr)
    {
    }

    virtual ~DecoderStrategy()
    = default;

    virtual ProcessLine* CreateProcess(ByteStreamInfo rawStreamInfo) = 0;

    virtual void SetPresets(const JlsCustomParameters& presets) = 0;
    virtual void DecodeScan(std::unique_ptr<ProcessLine> outputData, const JlsRect& size, ByteStreamInfo& compressedData, bool bCheck) = 0;

    void Init(ByteStreamInfo& compressedStream)
    {
        _validBits = 0;
        _readCache = 0;

        if (compressedStream.rawStream)
        {
            _buffer.resize(40000);
            _position = static_cast<uint8_t*>(&_buffer[0]);
            _endPosition = _position;
            _byteStream = compressedStream.rawStream;
            AddBytesFromStream();
        }
        else
        {
            _byteStream = nullptr;
            _position = compressedStream.rawData;
            _endPosition = _position + compressedStream.count;
        }

        _nextFFPosition = FindNextFF();
        MakeValid();
    }

    void AddBytesFromStream()
    {
        if (!_byteStream || _byteStream->sgetc() == std::char_traits<char>::eof())
            return;

        std::size_t count = _endPosition - _position;

        if (count > 64)
            return;

        for (std::size_t i = 0; i < count; ++i)
        {
            _buffer[i] = _position[i];
        }
        std::size_t offset = &_buffer[0] - _position;

        _position += offset;
        _endPosition += offset;
        _nextFFPosition += offset;

        std::streamsize readbytes = _byteStream->sgetn(reinterpret_cast<char*>(_endPosition), _buffer.size() - count);
        _endPosition += readbytes;
    }

    inlinehint void Skip(int32_t length)
    {
        _validBits -= length;
        _readCache = _readCache << length; 
    }


    void OnLineBegin(int32_t /*cpixel*/, void* /*ptypeBuffer*/, int32_t /*pixelStride*/) 
    {
    }


    void OnLineEnd(int32_t pixelCount, const void* ptypeBuffer, int32_t pixelStride)
    {
        _processLine->NewLineDecoded(ptypeBuffer, pixelCount, pixelStride);
    }

    void EndScan()
    {
        if ((*_position) != 0xFF)
        {
            ReadBit();

            if ((*_position) != 0xFF)
                throw std::system_error(static_cast<int>(charls::ApiResult::TooMuchCompressedData), CharLSCategoryInstance());
        }

        if (_readCache != 0)
            throw std::system_error(static_cast<int>(charls::ApiResult::TooMuchCompressedData), CharLSCategoryInstance());
    }

    inlinehint bool OptimizedRead()
    {
        // Easy & fast: if there is no 0xFF byte in sight, we can read without bitstuffing
        if (_position < _nextFFPosition - (sizeof(bufType)-1))
        {
            _readCache |= FromBigEndian<sizeof(bufType)>::Read(_position) >> _validBits;
            int bytesToRead = (bufferbits - _validBits) >> 3;
            _position += bytesToRead;
            _validBits += bytesToRead * 8;
            ASSERT(_validBits >= bufferbits - 8);
            return true;
        }
        return false;
    }

    typedef std::size_t bufType;

    enum
    {
        bufferbits = sizeof( bufType ) * 8
    };

    void MakeValid()
    {
        ASSERT(_validBits <=bufferbits - 8);

        if (OptimizedRead())
            return;

        AddBytesFromStream();

        do
        {
            if (_position >= _endPosition)
            {
                if (_validBits <= 0)
                    throw std::system_error(static_cast<int>(charls::ApiResult::InvalidCompressedData), CharLSCategoryInstance());

                return;
            }

            bufType valnew = _position[0];

            if (valnew == 0xFF)
            {
                // JPEG bitstream rule: no FF may be followed by 0x80 or higher
                if (_position == _endPosition - 1 || (_position[1] & 0x80) != 0)
                {
                    if (_validBits <= 0)
                        throw std::system_error(static_cast<int>(charls::ApiResult::InvalidCompressedData), CharLSCategoryInstance());

                    return;
                }
            }

            _readCache |= valnew << (bufferbits - 8 - _validBits);
            _position += 1;
            _validBits += 8;

            if (valnew == 0xFF)
            {
                _validBits--;
            }
        }
        while (_validBits < bufferbits - 8);

        _nextFFPosition = FindNextFF();
        return;
    }

    uint8_t* FindNextFF() const
    {
        auto positionNextFF = _position;

        while (positionNextFF < _endPosition)
        {
            if (*positionNextFF == 0xFF) 
                break;

            positionNextFF++;
        }

        return positionNextFF;
    }

    uint8_t* GetCurBytePos() const
    {
        int32_t validBits = _validBits;
        uint8_t* compressedBytes = _position;

        for (;;)
        {
            int32_t cbitLast = compressedBytes[-1] == 0xFF ? 7 : 8;

            if (validBits < cbitLast )
                return compressedBytes;

            validBits -= cbitLast; 
            compressedBytes--;
        }
    }

    inlinehint int32_t ReadValue(int32_t length)
    {
        if (_validBits < length)
        {
            MakeValid();
            if (_validBits < length)
                throw std::system_error(static_cast<int>(charls::ApiResult::InvalidCompressedData), CharLSCategoryInstance());
        }

        ASSERT(length != 0 && length <= _validBits);
        ASSERT(length < 32);
        int32_t result = int32_t(_readCache >> (bufferbits - length));
        Skip(length);
        return result;
    }

    inlinehint int32_t PeekByte()
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

    inlinehint int32_t Peek0Bits()
    {
        if (_validBits < 16)
        {
            MakeValid();
        }
        bufType valTest = _readCache;

        for (int32_t count = 0; count < 16; count++)
        {
            if ((valTest & (bufType(1) << (bufferbits - 1))) != 0)
                return count;

            valTest <<= 1;
        }
        return -1;
    }

    inlinehint int32_t ReadHighbits()
    {
        int32_t count = Peek0Bits();
        if (count >= 0)
        {
            Skip(count + 1);
            return count;
        }
        Skip(15);

        for (int32_t highbits = 15; ; highbits++)
        {
            if (ReadBit())
                return highbits;
        }
    }

    int32_t ReadLongValue(int32_t length)
    {
        if (length <= 24)
            return ReadValue(length);

        return (ReadValue(length - 24) << 24) + ReadValue(24);
    }

protected:
    JlsParameters _params;
    std::unique_ptr<ProcessLine> _processLine;

private:
    std::vector<uint8_t> _buffer;
    std::basic_streambuf<char>* _byteStream;

    // decoding
    bufType _readCache;
    int32_t _validBits;
    uint8_t* _position;
    uint8_t* _nextFFPosition;
    uint8_t* _endPosition;
};


#endif
