// 
// (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use. 
// 

#ifndef CHARLS_ENCODERSTRATEGY
#define CHARLS_ENCODERSTRATEGY

#include "processline.h"
#include "decoderstrategy.h"


// Purpose: Implements encoding to stream of bits. In encoding mode JpegLsCodec inherits from EncoderStrategy
class EncoderStrategy
{

public:
    explicit EncoderStrategy(const JlsParameters& params) :
        _params(params),
        _bitBuffer(0),
        _freeBitCount(sizeof(_bitBuffer) * 8),
        _compressedLength(0),
        _position(nullptr),
        _isFFWritten(false),
        _bytesWritten(0),
        _compressedStream(nullptr)
    {
    }

    virtual ~EncoderStrategy() 
    = default;

    int32_t PeekByte();

    void OnLineBegin(int32_t cpixel, void* ptypeBuffer, int32_t pixelStride)
    {
        _processLine->NewLineRequested(ptypeBuffer, cpixel, pixelStride);
    }

    void OnLineEnd(int32_t /*cpixel*/, void* /*ptypeBuffer*/, int32_t /*pixelStride*/) { }

    virtual void SetPresets(const JlsCustomParameters& presets) = 0;

    virtual std::size_t EncodeScan(std::unique_ptr<ProcessLine> rawData, ByteStreamInfo& compressedData, void* pvoidCompare) = 0;

    virtual ProcessLine* CreateProcess(ByteStreamInfo rawStreamInfo) = 0;

protected:

    void Init(ByteStreamInfo& compressedStream)
    {
        _freeBitCount = sizeof(_bitBuffer) * 8;
        _bitBuffer = 0;

        if (compressedStream.rawStream)
        {
            _compressedStream = compressedStream.rawStream;
            _buffer.resize(4000);
            _position = static_cast<uint8_t*>(&_buffer[0]);
            _compressedLength = _buffer.size();
        }
        else
        {
            _position = compressedStream.rawData;
            _compressedLength = compressedStream.count;
        }
    }

    void AppendToBitStream(int32_t bits, int32_t bitCount)
    {
        ASSERT(bitCount < 32 && bitCount >= 0);
        ASSERT((!_qdecoder) || (bitCount == 0 && bits == 0) ||( _qdecoder->ReadLongValue(bitCount) == bits));
#ifndef NDEBUG
        int mask = (1u << (bitCount)) - 1;
        ASSERT((bits | mask) == mask); // Not used bits must be set to zero.
#endif

        _freeBitCount -= bitCount;
        if (_freeBitCount >= 0)
        {
            _bitBuffer |= bits << _freeBitCount;
        }
        else
        {
            // Add as much bits in the remaining space as possible and flush.
            _bitBuffer |= bits >> -_freeBitCount;
            Flush();

            // A second flush may be required if extra marker detect bits were needed and not all bits could be written.
            if (_freeBitCount < 0)
            {
                _bitBuffer |= bits >> -_freeBitCount;
                Flush();
            }

            ASSERT(_freeBitCount >= 0);
            _bitBuffer |= bits << _freeBitCount;
        }
    }

    void EndScan()
    {
        Flush();

        // if a 0xff was written, Flush() will force one unset bit anyway
        if (_isFFWritten)
            AppendToBitStream(0, (_freeBitCount - 1) % 8);
        else
            AppendToBitStream(0, _freeBitCount % 8);

        Flush();
        ASSERT(_freeBitCount == 0x20);

        if (_compressedStream)
        {
            OverFlow();
        }
    }

    void OverFlow()
    {
        if (!_compressedStream)
            throw std::system_error(static_cast<int>(charls::ApiResult::CompressedBufferTooSmall), CharLSCategoryInstance());

        std::size_t bytesCount = _position - static_cast<uint8_t*>(&_buffer[0]);
        std::size_t bytesWritten = static_cast<std::size_t>(_compressedStream->sputn(reinterpret_cast<char*>(&_buffer[0]), _position - static_cast<uint8_t*>(&_buffer[0])));

        if (bytesWritten != bytesCount)
            throw std::system_error(static_cast<int>(charls::ApiResult::CompressedBufferTooSmall), CharLSCategoryInstance());

        _position = static_cast<uint8_t*>(&_buffer[0]);
        _compressedLength = _buffer.size();
    }

    void Flush()
    {
        if (_compressedLength < 4)
        {
            OverFlow();
        }

        for (int i = 0; i < 4; ++i)
        {
            if (_freeBitCount >= 32)
                break;

            if (_isFFWritten)
            {
                // JPEG-LS requirement (T.87, A.1) to detect markers: after a xFF value a single 0 bit needs to be inserted.
                *_position = static_cast<uint8_t>(_bitBuffer >> 25);
                _bitBuffer = _bitBuffer << 7;
                _freeBitCount += 7;
            }
            else
            {
                *_position = static_cast<uint8_t>(_bitBuffer >> 24);
                _bitBuffer = _bitBuffer << 8;
                _freeBitCount += 8;
            }

            _isFFWritten = *_position == 0xFF;
            _position++;
            _compressedLength--;
            _bytesWritten++;
        }
    }

    std::size_t GetLength() const
    {
        return _bytesWritten - (_freeBitCount - 32) / 8;
    }

    inlinehint void AppendOnesToBitStream(int32_t length)
    {
        AppendToBitStream((1 << length) - 1, length);
    }

    std::unique_ptr<DecoderStrategy> _qdecoder;

protected:
    JlsParameters _params;
    std::unique_ptr<ProcessLine> _processLine;

private:
    unsigned int _bitBuffer;
    int32_t _freeBitCount;
    std::size_t _compressedLength;

    // encoding
    uint8_t* _position;
    bool _isFFWritten;
    std::size_t _bytesWritten;

    std::vector<uint8_t> _buffer;
    std::basic_streambuf<char>* _compressedStream;
};

#endif
