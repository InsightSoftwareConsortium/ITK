//
// (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use.
//


#include "charls.h"
#include "util.h"
#include "jpegstreamreader.h"
#include "jpegstreamwriter.h"
#include "jpegmarkersegment.h"
#include <cstring>

using namespace std;
using namespace charls;


static void VerifyInput(const ByteStreamInfo& uncompressedStream, const JlsParameters& parameters)
{
    if (!uncompressedStream.rawStream && !uncompressedStream.rawData)
        throw CreateSystemError(ApiResult::InvalidJlsParameters, "rawStream or rawData needs to reference to something");

    if (parameters.width < 1 || parameters.width > 65535)
        throw CreateSystemError(ApiResult::InvalidJlsParameters, "width needs to be in the range [1, 65535]");

    if (parameters.height < 1 || parameters.height > 65535)
        throw CreateSystemError(ApiResult::InvalidJlsParameters, "height needs to be in the range [1, 65535]");

    if (parameters.bitsPerSample < 2 || parameters.bitsPerSample > 16)
        throw CreateSystemError(ApiResult::InvalidJlsParameters, "bitspersample needs to be in the range [2, 16]");

    if (!(parameters.interleaveMode == InterleaveMode::None || parameters.interleaveMode == InterleaveMode::Sample || parameters.interleaveMode == InterleaveMode::Line))
        throw CreateSystemError(ApiResult::InvalidJlsParameters, "interleaveMode needs to be set to a value of {None, Sample, Line}");

    if (parameters.components < 1 || parameters.components > 255)
        throw CreateSystemError(ApiResult::InvalidJlsParameters, "components needs to be in the range [1, 255]");

    if (uncompressedStream.rawData)
    {
        if (uncompressedStream.count < size_t(parameters.height * parameters.width * parameters.components * (parameters.bitsPerSample > 8 ? 2 : 1)))
            throw CreateSystemError(ApiResult::InvalidJlsParameters, "uncompressed size does not match with the other parameters");
    }

    switch (parameters.components)
    {
    case 3:
        break;
    case 4:
        if (parameters.interleaveMode == InterleaveMode::Sample)
            throw CreateSystemError(ApiResult::InvalidJlsParameters, "interleaveMode cannot be set to Sample in combination with components = 4");
        break;
    default:
        if (parameters.interleaveMode != InterleaveMode::None)
            throw CreateSystemError(ApiResult::InvalidJlsParameters, "interleaveMode can only be set to None in combination with components = 1");
        break;
    }
}


static ApiResult SystemErrorToCharLSError(const system_error& e)
{
    return e.code().category() == CharLSCategoryInstance() ? static_cast<ApiResult>(e.code().value()) : ApiResult::UnspecifiedFailure;
}


static void ClearErrorMessage(char* errorMessage)
{
    if (errorMessage)
    {
        errorMessage[0] = 0;
    }
}


static void CopyWhatTextToErrorMessage(const system_error& e, char* errorMessage)
{
    if (!errorMessage)
        return;

    if (e.code().category() == CharLSCategoryInstance())
    {
        ASSERT(strlen(e.what()) < ErrorMessageSize);
        strcpy(errorMessage, e.what());
    }
    else
    {
        errorMessage[0] = 0;
    }
}


CHARLS_IMEXPORT(ApiResult) JpegLsEncodeStream(ByteStreamInfo compressedStreamInfo, size_t& pcbyteWritten,
    ByteStreamInfo rawStreamInfo, const struct JlsParameters& params, char* errorMessage)
{
    try
    {
        VerifyInput(rawStreamInfo, params);

        JlsParameters info = params;
        if (info.stride == 0)
        {
            info.stride = info.width * ((info.bitsPerSample + 7)/8);
            if (info.interleaveMode != InterleaveMode::None)
            {
                info.stride *= info.components;
            }
        }

        JpegStreamWriter writer;
        if (info.jfif.version)
        {
            writer.AddSegment(JpegMarkerSegment::CreateJpegFileInterchangeFormatSegment(info.jfif));
        }

        writer.AddSegment(JpegMarkerSegment::CreateStartOfFrameSegment(info.width, info.height, info.bitsPerSample, info.components));


        if (info.colorTransformation != ColorTransformation::None)
        {
            writer.AddColorTransform(info.colorTransformation);
        }

        if (info.interleaveMode == InterleaveMode::None)
        {
            int32_t cbyteComp = info.width * info.height * ((info.bitsPerSample + 7) / 8);
            for (int32_t component = 0; component < info.components; ++component)
            {
                writer.AddScan(rawStreamInfo, info);
                SkipBytes(rawStreamInfo, cbyteComp);
            }
        }
        else
        {
            writer.AddScan(rawStreamInfo, info);
        }

        writer.Write(compressedStreamInfo);
        pcbyteWritten = writer.GetBytesWritten();

        ClearErrorMessage(errorMessage);
        return ApiResult::OK;
    }
    catch (const system_error& e)
    {
        CopyWhatTextToErrorMessage(e, errorMessage);
        return SystemErrorToCharLSError(e);
    }
    catch (...)
    {
        ClearErrorMessage(errorMessage);
        return ApiResult::UnexpectedFailure;
    }
}


CHARLS_IMEXPORT(ApiResult) JpegLsDecodeStream(ByteStreamInfo rawStream, ByteStreamInfo compressedStream, const JlsParameters* info, char* errorMessage)
{
    try
    {
        JpegStreamReader reader(compressedStream);

        if (info)
        {
            reader.SetInfo(*info);
        }

        reader.Read(rawStream);

        ClearErrorMessage(errorMessage);
        return ApiResult::OK;
    }
    catch (const system_error& e)
    {
        CopyWhatTextToErrorMessage(e, errorMessage);
        return SystemErrorToCharLSError(e);
    }
    catch (...)
    {
        ClearErrorMessage(errorMessage);
        return ApiResult::UnexpectedFailure;
    }
}


CHARLS_IMEXPORT(ApiResult) JpegLsReadHeaderStream(ByteStreamInfo rawStreamInfo, JlsParameters* params, char* errorMessage)
{
    try
    {
        JpegStreamReader reader(rawStreamInfo);
        reader.ReadHeader();
        reader.ReadStartOfScan(true);
        *params = reader.GetMetadata();

        ClearErrorMessage(errorMessage);
        return ApiResult::OK;
    }
    catch (const std::system_error& e)
    {
        CopyWhatTextToErrorMessage(e, errorMessage);
        return SystemErrorToCharLSError(e);
    }
    catch (...)
    {
        ClearErrorMessage(errorMessage);
        return ApiResult::UnexpectedFailure;
    }
}

extern "C"
{
    CHARLS_IMEXPORT(ApiResult) JpegLsEncode(void* destination, size_t destinationLength, size_t* bytesWritten, const void* source, size_t sourceLength, const struct JlsParameters* params, char* errorMessage)
    {
        if (!destination || !bytesWritten || !source || !params)
            return ApiResult::InvalidJlsParameters;

        ByteStreamInfo rawStreamInfo = FromByteArray(source, sourceLength);
        ByteStreamInfo compressedStreamInfo = FromByteArray(destination, destinationLength);

        return JpegLsEncodeStream(compressedStreamInfo, *bytesWritten, rawStreamInfo, *params, errorMessage);
    }


    CHARLS_IMEXPORT(ApiResult) JpegLsReadHeader(const void* compressedData, size_t compressedLength, JlsParameters* params, char* errorMessage)
    {
        return JpegLsReadHeaderStream(FromByteArray(compressedData, compressedLength), params, errorMessage);
    }


    CHARLS_IMEXPORT(ApiResult) JpegLsDecode(void* destination, size_t destinationLength, const void* source, size_t sourceLength, const struct JlsParameters* params, char* errorMessage)
    {
        ByteStreamInfo compressedStream = FromByteArray(source, sourceLength);
        ByteStreamInfo rawStreamInfo = FromByteArray(destination, destinationLength);

        return JpegLsDecodeStream(rawStreamInfo, compressedStream, params, errorMessage);
    }


    CHARLS_IMEXPORT(ApiResult) JpegLsVerifyEncode(const void* uncompressedData, size_t uncompressedLength, const void* compressedData, size_t compressedLength, char* errorMessage)
    {
        try
        {
            JlsParameters info = JlsParameters();

            auto error = JpegLsReadHeader(compressedData, compressedLength, &info, errorMessage);
            if (error != ApiResult::OK)
                return error;

            ByteStreamInfo rawStreamInfo = FromByteArray(uncompressedData, uncompressedLength);

            VerifyInput(rawStreamInfo, info);

            JpegStreamWriter writer;
            if (info.jfif.version)
            {
                writer.AddSegment(JpegMarkerSegment::CreateJpegFileInterchangeFormatSegment(info.jfif));
            }

            writer.AddSegment(JpegMarkerSegment::CreateStartOfFrameSegment(info.width, info.height, info.bitsPerSample, info.components));

            if (info.interleaveMode == InterleaveMode::None)
            {
                int32_t fieldLength = info.width * info.height * ((info.bitsPerSample + 7) / 8);
                for (int32_t component = 0; component < info.components; ++component)
                {
                    writer.AddScan(rawStreamInfo, info);
                    SkipBytes(rawStreamInfo, fieldLength);
                }
            }
            else
            {
                writer.AddScan(rawStreamInfo, info);
            }

            vector<uint8_t> rgbyteCompressed(compressedLength + 16);

            memcpy(&rgbyteCompressed[0], compressedData, compressedLength);

            writer.EnableCompare(true);
            writer.Write(FromByteArray(&rgbyteCompressed[0], rgbyteCompressed.size()));
            ClearErrorMessage(errorMessage);
            return ApiResult::OK;
        }
        catch (const system_error& e)
        {
            CopyWhatTextToErrorMessage(e, errorMessage);
            return SystemErrorToCharLSError(e);
        }
        catch (...)
        {
            ClearErrorMessage(errorMessage);
            return ApiResult::UnexpectedFailure;
        }
    }


    CHARLS_IMEXPORT(ApiResult) JpegLsDecodeRect(void* uncompressedData, size_t uncompressedLength, const void* compressedData, size_t compressedLength,
        JlsRect roi, JlsParameters* info, char* errorMessage)
    {
        try
        {
            ByteStreamInfo compressedStream = FromByteArray(compressedData, compressedLength);
            JpegStreamReader reader(compressedStream);

            ByteStreamInfo rawStreamInfo = FromByteArray(uncompressedData, uncompressedLength);

            if (info)
            {
                reader.SetInfo(*info);
            }

            reader.SetRect(roi);
            reader.Read(rawStreamInfo);

            ClearErrorMessage(errorMessage);
            return ApiResult::OK;
        }
        catch (const system_error& e)
        {
            CopyWhatTextToErrorMessage(e, errorMessage);
            return SystemErrorToCharLSError(e);
        }
        catch (...)
        {
            ClearErrorMessage(errorMessage);
            return ApiResult::UnexpectedFailure;
        }
    }
}
