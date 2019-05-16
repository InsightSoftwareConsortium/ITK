//
// (C) CharLS Team 2014, all rights reserved. See the accompanying "License.txt" for licensed use. 
//

#pragma once

#include <cstdint>

// JPEG Marker codes have the pattern 0xFFaa in a JPEG byte stream.
// The valid 'aa' options are defined by several ITU / IEC standards:
// 0x00, 0x01, 0xFE, 0xC0-0xDF are defined in ITU T.81/IEC 10918-1
// 0xF0 - 0xF6 are defined in ITU T.84/IEC 10918-3 JPEG extensions
// 0xF7 - 0xF8 are defined in ITU T.87/IEC 14495-1 JPEG LS
// 0x4F - 0x6F, 0x90 - 0x93 are defined in JPEG 2000 IEC 15444-1
enum class JpegMarkerCode : uint8_t
{
    StartOfImage = 0xD8, // SOI: Marks the start of an image.
    EndOfImage = 0xD9,   // EOI: Marks the end of an image.
    StartOfScan = 0xDA,  // SOS: Marks the start of scan.

    // The following markers are defined in ITU T.81 | ISO IEC 10918-1.
    StartOfFrameBaselineJpeg = 0xC0,            // SOF_0:  Marks the start of a (Baseline jpeg) encoded frame.
    StartOfFrameExtendedSequential = 0xC1,      // SOF_1:  Marks the start of a (Extended sequential, huffman) encoded frame.
    StartOfFrameProgressive = 0xC2,             // SOF_2:  Marks the start of a (progressive, huffman) encoded frame.
    StartOfFrameLossless = 0xC3,                // SOF_3:  Marks the start of a (lossless, huffman) encoded frame.
    StartOfFrameDifferentialSequential = 0xC5,  // SOF_5:  Marks the start of a (differential sequential, huffman) encoded frame.
    StartOfFrameDifferentialProgressive = 0xC6, // SOF_6:  Marks the start of a (differential progressive, huffman) encoded frame.
    StartOfFrameDifferentialLossless = 0xC7,    // SOF_7:  Marks the start of a (differential lossless, huffman) encoded frame.
    StartOfFrameExtendedArithemtic = 0xC9,      // SOF_9:  Marks the start of a (extended sequential, arithmetic) encoded frame.
    StartOfFrameProgressiveArithemtic = 0xCA,   // SOF_10: Marks the start of a (progressive, arithmetic) encoded frame.
    StartOfFrameLosslessArithemtic = 0xCB,      // SOF_11: Marks the start of a (lossless, arithmetic) encoded frame.

    StartOfFrameJpegLS = 0xF7,                  // SOF_55: Marks the start of a (JPEG-LS) encoded frame.
    JpegLSExtendedParameters = 0xF8,            // LSE:    JPEG-LS extended parameters.

    ApplicationData0 = 0xE0,                    // APP0: Application data 0: used for JFIF header.
    ApplicationData7 = 0xE7,                    // APP7: Application data 7: colorspace.
    ApplicationData8 = 0xE8,                    // APP8: Application data 8: colorXForm.
    Comment = 0xFE                              // COM:  Comment block.
};
