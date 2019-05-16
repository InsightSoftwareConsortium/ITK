//
// (C) CharLS Team 2014, all rights reserved. See the accompanying "License.txt" for licensed use. 
//

#ifndef CHARLS_JPEGSTREAMWRITER
#define CHARLS_JPEGSTREAMWRITER

#include "util.h"
#include "jpegsegment.h"
#include <vector>
#include <memory>

enum class JpegMarkerCode : uint8_t;


//
// Purpose: 'Writer'class that can generate JPEG-LS file streams.
//
class JpegStreamWriter
{
    friend class JpegMarkerSegment;
    friend class JpegImageDataSegment;

public:
    JpegStreamWriter();

    void AddSegment(std::unique_ptr<JpegSegment> segment)
    {
        _segments.push_back(std::move(segment));
    }

    void AddScan(const ByteStreamInfo& info, const JlsParameters& params);

    void AddColorTransform(charls::ColorTransformation transformation);

    std::size_t GetBytesWritten() const
    {
        return _byteOffset;
    }

    std::size_t GetLength() const
    {
        return _data.count - _byteOffset;
    }

    std::size_t Write(const ByteStreamInfo& info);

    void EnableCompare(bool bCompare)
    {
        _bCompare = bCompare;
    }

private:
    uint8_t* GetPos() const
    {
        return _data.rawData + _byteOffset;
    }

    ByteStreamInfo OutputStream() const
    {
        ByteStreamInfo data = _data;
        data.count -= _byteOffset;
        data.rawData += _byteOffset;
        return data;
    }

    void WriteByte(uint8_t val)
    {
        ASSERT(!_bCompare || _data.rawData[_byteOffset] == val);

        if (_data.rawStream)
        {
            _data.rawStream->sputc(val);
        }
        else
        {
            if (_byteOffset >= _data.count)
                throw std::system_error(static_cast<int>(charls::ApiResult::CompressedBufferTooSmall), CharLSCategoryInstance());

            _data.rawData[_byteOffset++] = val;
        }
    }

    void WriteBytes(const std::vector<uint8_t>& bytes)
    {
        for (std::size_t i = 0; i < bytes.size(); ++i)
        {
            WriteByte(bytes[i]);
        }
    }

    void WriteWord(uint16_t value)
    {
        WriteByte(static_cast<uint8_t>(value / 0x100));
        WriteByte(static_cast<uint8_t>(value % 0x100));
    }

    void WriteMarker(JpegMarkerCode marker)
    {
        WriteByte(0xFF);
        WriteByte(static_cast<uint8_t>(marker));
    }

    void Seek(std::size_t byteCount)
    {
        if (_data.rawStream)
            return;

        _byteOffset += byteCount;
    }

    bool _bCompare;
    ByteStreamInfo _data;
    std::size_t _byteOffset;
    int32_t _lastCompenentIndex;
    std::vector<std::unique_ptr<JpegSegment>> _segments;
};

#endif
