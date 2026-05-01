/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmJPEGTurboCodec.h"

#ifndef GDCM_USE_JPEGTURBO
// Stub: nothing to compile when libjpeg-turbo is not enabled
namespace gdcm {
JPEGTurboCodec::JPEGTurboCodec() : Internals(nullptr) {}
JPEGTurboCodec::~JPEGTurboCodec() {}
bool JPEGTurboCodec::GetHeaderInfo(std::istream &, TransferSyntax &) { return false; }
bool JPEGTurboCodec::DecodeByStreams(std::istream &, std::ostream &) { return false; }
bool JPEGTurboCodec::InternalCode(const char *, unsigned long, std::ostream &) { return false; }
bool JPEGTurboCodec::EncodeBuffer(std::ostream &, const char *, size_t) { return false; }
bool JPEGTurboCodec::IsStateSuspension() const { return false; }
} // end namespace gdcm

#else // GDCM_USE_JPEGTURBO is defined

#include "gdcmTrace.h"
#include "gdcmTransferSyntax.h"

#include <csetjmp>
#include <climits>

// libjpeg-turbo headers (via ITK wrapper, symbols are mangled with itk_ prefix)
extern "C" {
#include "itk_jpeg.h"
#include <itkjpeg-turbo/jpegint.h>  // for cinfo.master->lossless
}

/*
 * --------------------------------------------------------------------------
 *  Stream-based source and destination managers for std::istream / std::ostream
 *  (ported from gdcmJPEGBITSCodec.hxx)
 * --------------------------------------------------------------------------
 */

namespace gdcm
{

/* ---- Source manager (decompression from std::istream) ---- */

struct turbo_source_mgr {
  struct jpeg_source_mgr pub;
  std::istream *infile;
  JOCTET *buffer;
  boolean start_of_file;
};
typedef turbo_source_mgr *turbo_src_ptr;

#define TURBO_INPUT_BUF_SIZE  4096

static void turbo_init_source(j_decompress_ptr cinfo)
{
  turbo_src_ptr src = (turbo_src_ptr)cinfo->src;
  src->start_of_file = TRUE;
}

static boolean turbo_fill_input_buffer(j_decompress_ptr cinfo)
{
  turbo_src_ptr src = (turbo_src_ptr)cinfo->src;

  std::streampos pos = src->infile->tellg();
  std::streampos end = src->infile->seekg(0, std::ios::end).tellg();
  src->infile->seekg(pos, std::ios::beg);

  if (end == pos)
    return FALSE; // suspension

  size_t toread = TURBO_INPUT_BUF_SIZE;
  if ((end - pos) < (std::streamoff)TURBO_INPUT_BUF_SIZE)
    toread = (size_t)(end - pos);

  src->infile->read((char *)src->buffer, toread);
  std::streamsize gcount = src->infile->gcount();

  if (gcount <= 0) {
    if (src->start_of_file)
      ERREXIT(cinfo, JERR_INPUT_EMPTY);
    WARNMS(cinfo, JWRN_JPEG_EOF);
    src->buffer[0] = (JOCTET)0xFF;
    src->buffer[1] = (JOCTET)JPEG_EOI;
    gcount = 2;
  }

  src->pub.next_input_byte = src->buffer;
  src->pub.bytes_in_buffer = (size_t)gcount;
  src->start_of_file = FALSE;
  return TRUE;
}

static void turbo_skip_input_data(j_decompress_ptr cinfo, long num_bytes)
{
  turbo_src_ptr src = (turbo_src_ptr)cinfo->src;
  if (num_bytes > 0) {
    while (num_bytes > (long)src->pub.bytes_in_buffer) {
      num_bytes -= (long)src->pub.bytes_in_buffer;
      (void)turbo_fill_input_buffer(cinfo);
    }
    src->pub.next_input_byte += (size_t)num_bytes;
    src->pub.bytes_in_buffer -= (size_t)num_bytes;
  }
}

static void turbo_term_source(j_decompress_ptr)
{
}

static void turbo_stdio_src(j_decompress_ptr cinfo, std::istream &infile, bool reset)
{
  turbo_src_ptr src;
  if (cinfo->src == nullptr) {
    cinfo->src = (struct jpeg_source_mgr *)
      (*cinfo->mem->alloc_small)((j_common_ptr)cinfo, JPOOL_PERMANENT,
                                  sizeof(turbo_source_mgr));
    src = (turbo_src_ptr)cinfo->src;
    src->buffer = (JOCTET *)
      (*cinfo->mem->alloc_small)((j_common_ptr)cinfo, JPOOL_PERMANENT,
                                  TURBO_INPUT_BUF_SIZE * sizeof(JOCTET));
  }
  src = (turbo_src_ptr)cinfo->src;
  src->pub.init_source = turbo_init_source;
  src->pub.fill_input_buffer = turbo_fill_input_buffer;
  src->pub.skip_input_data = turbo_skip_input_data;
  src->pub.resync_to_restart = jpeg_resync_to_restart;
  src->pub.term_source = turbo_term_source;
  src->infile = &infile;
  if (reset) {
    src->pub.bytes_in_buffer = 0;
    src->pub.next_input_byte = nullptr;
  }
}

/* ---- Destination manager (compression to std::ostream) ---- */

struct turbo_destination_mgr {
  struct jpeg_destination_mgr pub;
  std::ostream *outfile;
  JOCTET *buffer;
};
typedef turbo_destination_mgr *turbo_dest_ptr;

#define TURBO_OUTPUT_BUF_SIZE  4096

static void turbo_init_destination(j_compress_ptr cinfo)
{
  turbo_dest_ptr dest = (turbo_dest_ptr)cinfo->dest;
  dest->buffer = (JOCTET *)
    (*cinfo->mem->alloc_small)((j_common_ptr)cinfo, JPOOL_IMAGE,
                                TURBO_OUTPUT_BUF_SIZE * sizeof(JOCTET));
  dest->pub.next_output_byte = dest->buffer;
  dest->pub.free_in_buffer = TURBO_OUTPUT_BUF_SIZE;
}

static boolean turbo_empty_output_buffer(j_compress_ptr cinfo)
{
  turbo_dest_ptr dest = (turbo_dest_ptr)cinfo->dest;
  if (!dest->outfile->write((char *)dest->buffer, TURBO_OUTPUT_BUF_SIZE))
    ERREXIT(cinfo, JERR_FILE_WRITE);
  dest->pub.next_output_byte = dest->buffer;
  dest->pub.free_in_buffer = TURBO_OUTPUT_BUF_SIZE;
  return TRUE;
}

static void turbo_term_destination(j_compress_ptr cinfo)
{
  turbo_dest_ptr dest = (turbo_dest_ptr)cinfo->dest;
  size_t datacount = TURBO_OUTPUT_BUF_SIZE - dest->pub.free_in_buffer;
  if (datacount > 0) {
    if (!dest->outfile->write((char *)dest->buffer, datacount))
      ERREXIT(cinfo, JERR_FILE_WRITE);
  }
  dest->outfile->flush();
  if (dest->outfile->fail())
    ERREXIT(cinfo, JERR_FILE_WRITE);
}

static void turbo_stdio_dest(j_compress_ptr cinfo, std::ostream *outfile)
{
  turbo_dest_ptr dest;
  if (cinfo->dest == nullptr) {
    cinfo->dest = (struct jpeg_destination_mgr *)
      (*cinfo->mem->alloc_small)((j_common_ptr)cinfo, JPOOL_PERMANENT,
                                  sizeof(turbo_destination_mgr));
  }
  dest = (turbo_dest_ptr)cinfo->dest;
  dest->pub.init_destination = turbo_init_destination;
  dest->pub.empty_output_buffer = turbo_empty_output_buffer;
  dest->pub.term_destination = turbo_term_destination;
  dest->outfile = outfile;
}

/*
 * --------------------------------------------------------------------------
 *  Error handler with setjmp/longjmp
 * --------------------------------------------------------------------------
 */

struct turbo_error_mgr {
  struct jpeg_error_mgr pub;
  jmp_buf setjmp_buffer;
};
typedef turbo_error_mgr *turbo_error_ptr;

extern "C" {
static void turbo_error_exit(j_common_ptr cinfo)
{
  turbo_error_ptr myerr = (turbo_error_ptr)cinfo->err;
  (*cinfo->err->output_message)(cinfo);
  longjmp(myerr->setjmp_buffer, 1);
}
}

/*
 * --------------------------------------------------------------------------
 *  Internal state (pimpl)
 * --------------------------------------------------------------------------
 */

class JPEGTurboInternals
{
public:
  JPEGTurboInternals() : cinfo(), cinfo_comp(), jerr(), StateSuspension(0), SampBuffer(nullptr) {}
  jpeg_decompress_struct cinfo;
  jpeg_compress_struct cinfo_comp;
  turbo_error_mgr jerr;
  int StateSuspension;
  void *SampBuffer;
};

JPEGTurboCodec::JPEGTurboCodec()
{
  Internals = new JPEGTurboInternals;
}

JPEGTurboCodec::~JPEGTurboCodec()
{
  delete Internals;
}

/*
 * --------------------------------------------------------------------------
 *  Helper: determine if the decompressor is in lossless mode.
 *  Uses the internal master->lossless flag set after jpeg_read_header().
 * --------------------------------------------------------------------------
 */
static bool turbo_is_lossless(j_decompress_ptr cinfo)
{
  // After jpeg_read_header, master is allocated and lossless flag is set
  if (cinfo->master)
    return cinfo->master->lossless != 0;
  // Fallback heuristic: lossless requires Se==0, Ss in [1..7]
  return false;
}

/*
 * --------------------------------------------------------------------------
 *  GetHeaderInfo — read JPEG header, detect transfer syntax and pixel format
 * --------------------------------------------------------------------------
 */
bool JPEGTurboCodec::GetHeaderInfo(std::istream &is, TransferSyntax &ts)
{
  jpeg_decompress_struct &cinfo = Internals->cinfo;
  turbo_error_mgr &jerr = Internals->jerr;

  if (Internals->StateSuspension == 0) {
    cinfo.err = jpeg_std_error(&jerr.pub);
    jerr.pub.error_exit = turbo_error_exit;
    if (setjmp(jerr.setjmp_buffer)) {
      if (jerr.pub.msg_code == JERR_BAD_PRECISION) {
        this->BitSample = jerr.pub.msg_parm.i[0];
      }
      jpeg_destroy_decompress(&cinfo);
      return false;
    }
  }

  if (Internals->StateSuspension == 0) {
    jpeg_create_decompress(&cinfo);
    turbo_stdio_src(&cinfo, is, true);
  } else {
    turbo_stdio_src(&cinfo, is, false);
  }

  if (Internals->StateSuspension < 2) {
    if (jpeg_read_header(&cinfo, TRUE) == JPEG_SUSPENDED) {
      Internals->StateSuspension = 2;
    }
    if (jerr.pub.num_warnings) {
      // libjpeg-turbo handles multi-precision natively, but report bit depth mismatch
      this->BitSample = cinfo.data_precision;
      jpeg_destroy_decompress(&cinfo);
      return false;
    }

    this->Dimensions[1] = cinfo.image_height;
    this->Dimensions[0] = cinfo.image_width;

    int prep = this->PF.GetPixelRepresentation();
    int precision = cinfo.data_precision;
    if (precision == 1) {
      this->PF = PixelFormat(PixelFormat::SINGLEBIT);
    } else if (precision <= 8) {
      this->PF = PixelFormat(PixelFormat::UINT8);
    } else if (precision <= 12) {
      this->PF = PixelFormat(PixelFormat::UINT12);
    } else if (precision <= 16) {
      this->PF = PixelFormat(PixelFormat::UINT16);
    } else {
      gdcm_assert(0);
    }
    this->PF.SetPixelRepresentation((uint16_t)prep);
    this->PF.SetBitsStored((uint16_t)precision);
    gdcm_assert((precision - 1) >= 0);
    this->PF.SetHighBit((uint16_t)(precision - 1));
    this->BitSample = precision;

    this->PlanarConfiguration = 0;

    // Color space
    if (cinfo.jpeg_color_space == JCS_UNKNOWN) {
      if (cinfo.num_components == 1) {
        PI = PhotometricInterpretation::MONOCHROME2;
        this->PF.SetSamplesPerPixel(1);
      } else if (cinfo.num_components == 3) {
        PI = PhotometricInterpretation::RGB;
        this->PF.SetSamplesPerPixel(3);
      } else {
        gdcm_assert(0);
      }
    } else if (cinfo.jpeg_color_space == JCS_GRAYSCALE) {
      gdcm_assert(cinfo.num_components == 1);
      PI = PhotometricInterpretation::MONOCHROME2;
      this->PF.SetSamplesPerPixel(1);
    } else if (cinfo.jpeg_color_space == JCS_RGB) {
      gdcm_assert(cinfo.num_components == 3);
      PI = PhotometricInterpretation::RGB;
      this->PF.SetSamplesPerPixel(3);
    } else if (cinfo.jpeg_color_space == JCS_YCbCr) {
      gdcm_assert(cinfo.num_components == 3);
      PI = PhotometricInterpretation::YBR_FULL_422;
      if (turbo_is_lossless(&cinfo))
        PI = PhotometricInterpretation::RGB;
      this->PF.SetSamplesPerPixel(3);
      this->PlanarConfiguration = 1;
    } else if (cinfo.jpeg_color_space == JCS_CMYK) {
      gdcm_assert(cinfo.num_components == 4);
      PI = PhotometricInterpretation::CMYK;
      this->PF.SetSamplesPerPixel(4);
    } else if (cinfo.jpeg_color_space == JCS_YCCK) {
      gdcm_assert(cinfo.num_components == 4);
      gdcmWarningMacro("JCS_YCCK is not handled. Setting to CMYK for now.");
      PI = PhotometricInterpretation::CMYK;
      this->PF.SetSamplesPerPixel(4);
    } else {
      gdcm_assert(0);
    }
  }

  // Determine transfer syntax
  bool lossless = turbo_is_lossless(&cinfo);
  if (lossless) {
    int predictor = cinfo.Ss;
    switch (predictor) {
    case 1:
      ts = TransferSyntax::JPEGLosslessProcess14_1;
      break;
    default:
      ts = TransferSyntax::JPEGLosslessProcess14;
      break;
    }
  } else if (cinfo.progressive_mode) {
    ts = TransferSyntax::JPEGFullProgressionProcess10_12;
  } else {
    // Sequential
    if (this->BitSample <= 8)
      ts = TransferSyntax::JPEGBaselineProcess1;
    else if (this->BitSample <= 12)
      ts = TransferSyntax::JPEGExtendedProcess2_4;
    else {
      gdcm_assert(0);
      return false;
    }
  }

  LossyFlag = !lossless;

  if (cinfo.density_unit != 0 || cinfo.X_density != 1 || cinfo.Y_density != 1) {
    gdcmWarningMacro("Pixel Density from JFIF Marker is not supported (for now)");
  }

  Internals->StateSuspension = 0;
  jpeg_destroy_decompress(&cinfo);
  return true;
}

/*
 * --------------------------------------------------------------------------
 *  DecodeByStreams — full decompression
 * --------------------------------------------------------------------------
 */
bool JPEGTurboCodec::DecodeByStreams(std::istream &is, std::ostream &os)
{
  jpeg_decompress_struct &cinfo = Internals->cinfo;
  turbo_error_mgr &jerr = Internals->jerr;
  size_t row_stride;

  if (Internals->StateSuspension == 0) {
    cinfo.err = jpeg_std_error(&jerr.pub);
    jerr.pub.error_exit = turbo_error_exit;
    if (setjmp(jerr.setjmp_buffer)) {
      if (jerr.pub.msg_code == JERR_BAD_PRECISION) {
        this->BitSample = jerr.pub.msg_parm.i[0];
      }
      jpeg_destroy_decompress(&cinfo);
      return false;
    }
  }

  if (Internals->StateSuspension == 0) {
    jpeg_create_decompress(&cinfo);
    turbo_stdio_src(&cinfo, is, true);
  } else {
    turbo_stdio_src(&cinfo, is, false);
  }

  if (Internals->StateSuspension < 2) {
    if (jpeg_read_header(&cinfo, TRUE) == JPEG_SUSPENDED) {
      Internals->StateSuspension = 2;
    }
    if (jerr.pub.num_warnings) {
      // Precision mismatch — report actual data precision
      this->BitSample = cinfo.data_precision;
      jpeg_destroy_decompress(&cinfo);
      return false;
    }

    // Sanity check dimensions
    const unsigned int *dims = this->GetDimensions();
    if (cinfo.image_width != dims[0] || cinfo.image_height != dims[1]) {
      gdcmWarningMacro("dimension mismatch. JPEG is " << cinfo.image_width << ","
        << cinfo.image_height << " while DICOM " << dims[0] << "," << dims[1]);
      return false;
    }

    bool lossless = turbo_is_lossless(&cinfo);

    // Color space handling
    switch (cinfo.jpeg_color_space) {
    case JCS_GRAYSCALE:
      if (GetPhotometricInterpretation() != PhotometricInterpretation::MONOCHROME1 &&
          GetPhotometricInterpretation() != PhotometricInterpretation::MONOCHROME2) {
        gdcmWarningMacro("Wrong PhotometricInterpretation. DICOM says: "
          << GetPhotometricInterpretation() << " but JPEG says: " << (int)cinfo.jpeg_color_space);
        this->PI = PhotometricInterpretation::MONOCHROME2;
      }
      break;
    case JCS_RGB:
      if (lossless) {
        cinfo.jpeg_color_space = JCS_UNKNOWN;
        cinfo.out_color_space = JCS_UNKNOWN;
      }
      if (GetPhotometricInterpretation() == PhotometricInterpretation::YBR_RCT ||
          GetPhotometricInterpretation() == PhotometricInterpretation::YBR_ICT)
        this->PI = PhotometricInterpretation::RGB;
      break;
    case JCS_YCbCr:
      if (GetPhotometricInterpretation() != PhotometricInterpretation::YBR_FULL &&
          GetPhotometricInterpretation() != PhotometricInterpretation::YBR_PARTIAL_422 &&
          GetPhotometricInterpretation() != PhotometricInterpretation::YBR_FULL_422) {
        gdcmWarningMacro("Wrong PhotometricInterpretation. DICOM says: "
          << GetPhotometricInterpretation() << " but JPEG says: " << (int)cinfo.jpeg_color_space);
        cinfo.jpeg_color_space = JCS_UNKNOWN;
        cinfo.out_color_space = JCS_UNKNOWN;
      }
      if (GetPhotometricInterpretation() == PhotometricInterpretation::YBR_FULL ||
          GetPhotometricInterpretation() == PhotometricInterpretation::YBR_PARTIAL_422 ||
          GetPhotometricInterpretation() == PhotometricInterpretation::YBR_FULL_422) {
        cinfo.jpeg_color_space = JCS_UNKNOWN;
        cinfo.out_color_space = JCS_UNKNOWN;
      }
      break;
    case JCS_CMYK:
      if (lossless) {
        cinfo.jpeg_color_space = JCS_UNKNOWN;
        cinfo.out_color_space = JCS_UNKNOWN;
      }
      break;
    case JCS_UNKNOWN:
      if (lossless) {
        cinfo.jpeg_color_space = JCS_UNKNOWN;
        cinfo.out_color_space = JCS_UNKNOWN;
      }
      break;
    default:
      gdcm_assert(0);
      return false;
    }
  }

  // Start decompressor
  if (Internals->StateSuspension < 3) {
    if (jpeg_start_decompress(&cinfo) == FALSE) {
      Internals->StateSuspension = 3;
    }
  }

  // Determine sample size and row stride based on data_precision
  int precision = cinfo.data_precision;
  size_t sample_size;
  if (precision <= 8)
    sample_size = sizeof(JSAMPLE);
  else if (precision <= 12)
    sample_size = sizeof(J12SAMPLE);
  else
    sample_size = sizeof(J16SAMPLE);

  row_stride = cinfo.output_width * cinfo.output_components * sample_size;

  // Read scanlines, dispatching by precision
  if (precision <= 8) {
    JSAMPARRAY buffer;
    if (Internals->StateSuspension < 3) {
      buffer = (*cinfo.mem->alloc_sarray)((j_common_ptr)&cinfo, JPOOL_IMAGE,
                                           (JDIMENSION)row_stride, 1);
      Internals->SampBuffer = buffer;
    } else {
      buffer = (JSAMPARRAY)Internals->SampBuffer;
    }
    while (cinfo.output_scanline < cinfo.output_height) {
      if (jpeg_read_scanlines(&cinfo, buffer, 1) == 0) {
        Internals->StateSuspension = 3;
        return true;
      }
      os.write((char *)buffer[0], row_stride);
    }
  } else if (precision <= 12) {
    // 12-bit path: use jpeg12_read_scanlines with J12SAMPARRAY
    J12SAMPROW row12 = (J12SAMPROW)(*cinfo.mem->alloc_small)(
      (j_common_ptr)&cinfo, JPOOL_IMAGE, row_stride);
    J12SAMPARRAY buffer12 = &row12;
    while (cinfo.output_scanline < cinfo.output_height) {
      if (jpeg12_read_scanlines(&cinfo, buffer12, 1) == 0) {
        Internals->StateSuspension = 3;
        return true;
      }
      os.write((char *)buffer12[0], row_stride);
    }
  } else {
    // 16-bit path: use jpeg16_read_scanlines with J16SAMPARRAY
    J16SAMPROW row16 = (J16SAMPROW)(*cinfo.mem->alloc_small)(
      (j_common_ptr)&cinfo, JPOOL_IMAGE, row_stride);
    J16SAMPARRAY buffer16 = &row16;
    while (cinfo.output_scanline < cinfo.output_height) {
      if (jpeg16_read_scanlines(&cinfo, buffer16, 1) == 0) {
        Internals->StateSuspension = 3;
        return true;
      }
      os.write((char *)buffer16[0], row_stride);
    }
  }

  if (jpeg_finish_decompress(&cinfo) == FALSE) {
    Internals->StateSuspension = 4;
    return true;
  }

  if (turbo_is_lossless(&cinfo))
    LossyFlag = false;
  else
    LossyFlag = true;

  jpeg_destroy_decompress(&cinfo);

  if (jerr.pub.num_warnings > 1) {
    gdcmErrorMacro("Too many warnings during decompression: " << jerr.pub.num_warnings);
    return false;
  }
  Internals->StateSuspension = 0;
  return true;
}

/*
 * --------------------------------------------------------------------------
 *  InternalCode — compress a single frame
 * --------------------------------------------------------------------------
 */
bool JPEGTurboCodec::InternalCode(const char *input, unsigned long len, std::ostream &os)
{
  JSAMPLE *image_buffer = (JSAMPLE *)(void *)const_cast<char *>(input);
  const unsigned int *dims = this->GetDimensions();
  int image_width = dims[0];
  int image_height = dims[1];

  size_t expected_frame_size = (size_t)image_width * image_height *
                               this->GetPixelFormat().GetPixelSize();
  if (len != expected_frame_size) {
    gdcmErrorMacro("Frame size don't match");
    return false;
  }

  struct jpeg_compress_struct cinfo;
  struct turbo_error_mgr jerr;
  std::ostream *outfile = &os;
  size_t row_stride;

  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = turbo_error_exit;
  if (setjmp(jerr.setjmp_buffer)) {
    jpeg_destroy_compress(&cinfo);
    return false;
  }

  jpeg_create_compress(&cinfo);
  turbo_stdio_dest(&cinfo, outfile);

  cinfo.image_width = image_width;
  cinfo.image_height = image_height;

  switch (this->GetPhotometricInterpretation()) {
  case PhotometricInterpretation::MONOCHROME1:
  case PhotometricInterpretation::MONOCHROME2:
  case PhotometricInterpretation::PALETTE_COLOR:
    cinfo.input_components = 1;
    cinfo.in_color_space = JCS_GRAYSCALE;
    break;
  case PhotometricInterpretation::RGB:
  case PhotometricInterpretation::YBR_RCT:
  case PhotometricInterpretation::YBR_ICT:
    cinfo.input_components = 3;
    cinfo.in_color_space = JCS_RGB;
    break;
  case PhotometricInterpretation::YBR_FULL:
  case PhotometricInterpretation::YBR_FULL_422:
  case PhotometricInterpretation::YBR_PARTIAL_420:
  case PhotometricInterpretation::YBR_PARTIAL_422:
    cinfo.input_components = 3;
    cinfo.in_color_space = JCS_YCbCr;
    break;
  default:
    return false;
  }

  // Set data_precision based on our pixel format's bits stored
  cinfo.data_precision = this->GetPixelFormat().GetBitsStored();

  jpeg_set_defaults(&cinfo);

  if (!LossyFlag) {
    jpeg_enable_lossless(&cinfo, 1, 0);
  }

  if (!LossyFlag)
    gdcm_assert(Quality == 100);
  jpeg_set_quality(&cinfo, Quality, TRUE);

  cinfo.write_JFIF_header = 0;

  jpeg_start_compress(&cinfo, TRUE);

  int precision = cinfo.data_precision;
  row_stride = image_width * cinfo.input_components;

  if (precision <= 8) {
    JSAMPROW row_pointer[1];
    if (this->GetPlanarConfiguration() == 0) {
      while (cinfo.next_scanline < cinfo.image_height) {
        row_pointer[0] = &image_buffer[cinfo.next_scanline * row_stride];
        jpeg_write_scanlines(&cinfo, row_pointer, 1);
      }
    } else {
      JSAMPLE *tempbuffer = (JSAMPLE *)malloc(row_stride * sizeof(JSAMPLE));
      row_pointer[0] = tempbuffer;
      int offset = image_height * image_width;
      while (cinfo.next_scanline < cinfo.image_height) {
        gdcm_assert(row_stride % 3 == 0);
        JSAMPLE *ptempbuffer = tempbuffer;
        JSAMPLE *red = image_buffer + cinfo.next_scanline * row_stride / 3;
        JSAMPLE *green = red + offset;
        JSAMPLE *blue = green + offset;
        for (size_t i = 0; i < row_stride / 3; ++i) {
          *ptempbuffer++ = *red++;
          *ptempbuffer++ = *green++;
          *ptempbuffer++ = *blue++;
        }
        jpeg_write_scanlines(&cinfo, row_pointer, 1);
      }
      free(tempbuffer);
    }
  } else if (precision <= 12) {
    J12SAMPROW row12;
    J12SAMPLE *image12 = (J12SAMPLE *)(void *)const_cast<char *>(input);
    while (cinfo.next_scanline < cinfo.image_height) {
      row12 = &image12[cinfo.next_scanline * row_stride];
      jpeg12_write_scanlines(&cinfo, &row12, 1);
    }
  } else {
    J16SAMPROW row16;
    J16SAMPLE *image16 = (J16SAMPLE *)(void *)const_cast<char *>(input);
    while (cinfo.next_scanline < cinfo.image_height) {
      row16 = &image16[cinfo.next_scanline * row_stride];
      jpeg16_write_scanlines(&cinfo, &row16, 1);
    }
  }

  jpeg_finish_compress(&cinfo);
  jpeg_destroy_compress(&cinfo);

  return true;
}

/*
 * --------------------------------------------------------------------------
 *  EncodeBuffer — row-by-row encoding (used by AppendRowEncode)
 * --------------------------------------------------------------------------
 */
bool JPEGTurboCodec::EncodeBuffer(std::ostream &os, const char *data, size_t datalen)
{
  JSAMPLE *image_buffer = (JSAMPLE *)(void *)const_cast<char *>(data);
  const unsigned int *dims = this->GetDimensions();
  int image_width = dims[0];
  int image_height = dims[1];

  jpeg_compress_struct &cinfo = Internals->cinfo_comp;
  turbo_error_mgr &jerr = Internals->jerr;
  std::ostream *outfile = &os;
  size_t row_stride;

  if (Internals->StateSuspension == 0) {
    cinfo.err = jpeg_std_error(&jerr.pub);
    jerr.pub.error_exit = turbo_error_exit;
    if (setjmp(jerr.setjmp_buffer)) {
      jpeg_destroy_compress(&cinfo);
      return false;
    }
    jpeg_create_compress(&cinfo);
  }

  if (Internals->StateSuspension == 0) {
    turbo_stdio_dest(&cinfo, outfile);
  }

  if (Internals->StateSuspension == 0) {
    cinfo.image_width = image_width;
    cinfo.image_height = image_height;

    switch (this->GetPhotometricInterpretation()) {
    case PhotometricInterpretation::MONOCHROME1:
    case PhotometricInterpretation::MONOCHROME2:
    case PhotometricInterpretation::PALETTE_COLOR:
      cinfo.input_components = 1;
      cinfo.in_color_space = JCS_GRAYSCALE;
      break;
    case PhotometricInterpretation::RGB:
    case PhotometricInterpretation::YBR_RCT:
    case PhotometricInterpretation::YBR_ICT:
      cinfo.input_components = 3;
      cinfo.in_color_space = JCS_RGB;
      break;
    case PhotometricInterpretation::YBR_FULL:
    case PhotometricInterpretation::YBR_FULL_422:
    case PhotometricInterpretation::YBR_PARTIAL_420:
    case PhotometricInterpretation::YBR_PARTIAL_422:
      cinfo.input_components = 3;
      cinfo.in_color_space = JCS_YCbCr;
      break;
    default:
      return false;
    }

    cinfo.data_precision = this->GetPixelFormat().GetBitsStored();
    jpeg_set_defaults(&cinfo);

    if (!LossyFlag) {
      jpeg_enable_lossless(&cinfo, 1, 0);
    }
    if (!LossyFlag)
      gdcm_assert(Quality == 100);
    jpeg_set_quality(&cinfo, Quality, TRUE);
    cinfo.write_JFIF_header = 0;
  }

  if (Internals->StateSuspension == 0) {
    jpeg_start_compress(&cinfo, TRUE);
    Internals->StateSuspension = 1;
  }

  row_stride = image_width * cinfo.input_components;

  if (Internals->StateSuspension == 1) {
    int precision = cinfo.data_precision;
    gdcm_assert(this->GetPlanarConfiguration() == 0);

    if (precision <= 8) {
      gdcm_assert(row_stride * sizeof(JSAMPLE) == datalen);
      JSAMPROW row_pointer[1];
      row_pointer[0] = &image_buffer[0];
      JDIMENSION nscanline = jpeg_write_scanlines(&cinfo, row_pointer, 1);
      gdcm_assert(nscanline == 1); (void)nscanline;
    } else if (precision <= 12) {
      J12SAMPROW row12 = (J12SAMPROW)(void *)const_cast<char *>(data);
      jpeg12_write_scanlines(&cinfo, &row12, 1);
    } else {
      J16SAMPROW row16 = (J16SAMPROW)(void *)const_cast<char *>(data);
      jpeg16_write_scanlines(&cinfo, &row16, 1);
    }

    if (cinfo.next_scanline == cinfo.image_height) {
      Internals->StateSuspension = 2;
    }
  }

  if (Internals->StateSuspension == 2) {
    jpeg_finish_compress(&cinfo);
  }
  if (Internals->StateSuspension == 2) {
    jpeg_destroy_compress(&cinfo);
    Internals->StateSuspension = 0;
  }

  return true;
}

bool JPEGTurboCodec::IsStateSuspension() const
{
  return Internals->StateSuspension != 0;
}

} // end namespace gdcm

#endif // GDCM_USE_JPEGTURBO
