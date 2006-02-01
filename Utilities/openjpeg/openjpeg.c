/*
 * Copyright (c) 2005, Hervé Drolon, FreeImage Team
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#ifdef WIN32
#include <windows.h>
#endif /* WIN32 */

#include "opj_includes.h"

/* ---------------------------------------------------------------------- */
#ifdef WIN32
#ifndef OPJ_STATIC
BOOL APIENTRY
DllMain(HANDLE hModule, DWORD ul_reason_for_call, LPVOID lpReserved) {
  switch (ul_reason_for_call) {
    case DLL_PROCESS_ATTACH :
      break;
    case DLL_PROCESS_DETACH :
      break;
    case DLL_THREAD_ATTACH :
    case DLL_THREAD_DETACH :
      break;
    }

    return TRUE;
}
#endif /* OPJ_STATIC */
#endif /* WIN32 */

/* ---------------------------------------------------------------------- */


const char* OPJ_CALLCONV opj_version() {
    return OPENJPEG_VERSION;
}

opj_dinfo_t* OPJ_CALLCONV opj_create_decompress(OPJ_CODEC_FORMAT format) {
  opj_dinfo_t *dinfo = (opj_dinfo_t*)opj_malloc(sizeof(opj_dinfo_t));
  if(!dinfo) return NULL;
  dinfo->is_decompressor = true;
  switch(format) {
    case CODEC_J2K:
    case CODEC_JPT:
      /* get a J2K decoder handle */
      dinfo->j2k_handle = (void*)j2k_create_decompress((opj_common_ptr)dinfo);
      if(!dinfo->j2k_handle) {
        opj_free(dinfo);
        return NULL;
      }
      break;
    case CODEC_JP2:
      /* get a JP2 decoder handle */
      dinfo->jp2_handle = (void*)jp2_create_decompress((opj_common_ptr)dinfo);
      if(!dinfo->jp2_handle) {
        opj_free(dinfo);
        return NULL;
      }
      break;
    case CODEC_UNKNOWN:
    default:
      opj_free(dinfo);
      return NULL;
  }

  dinfo->codec_format = format;

  return dinfo;
}

void OPJ_CALLCONV opj_destroy_decompress(opj_dinfo_t *dinfo) {
  if(dinfo) {
    /* destroy the codec */
    switch(dinfo->codec_format) {
      case CODEC_J2K:
      case CODEC_JPT:
        j2k_destroy_decompress((opj_j2k_t*)dinfo->j2k_handle);
        break;
      case CODEC_JP2:
        jp2_destroy_decompress((opj_jp2_t*)dinfo->jp2_handle);
        break;
      case CODEC_UNKNOWN:
      default:
        break;
    }
    /* destroy the decompressor */
    opj_free(dinfo);
  }
}

void OPJ_CALLCONV opj_set_default_decoder_parameters(opj_dparameters_t *parameters) {
  if(parameters) {
    memset(parameters, 0, sizeof(opj_dparameters_t));
    /* default decoding parameters */
    parameters->cp_layer = 0;
    parameters->cp_reduce = 0;

    parameters->decod_format = -1;
    parameters->cod_format = -1;
  }
}

void OPJ_CALLCONV opj_setup_decoder(opj_dinfo_t *dinfo, opj_dparameters_t *parameters) {
  if(dinfo && parameters) {
    switch(dinfo->codec_format) {
      case CODEC_J2K:
      case CODEC_JPT:
        j2k_setup_decoder((opj_j2k_t*)dinfo->j2k_handle, parameters);
        break;
      case CODEC_JP2:
        jp2_setup_decoder((opj_jp2_t*)dinfo->jp2_handle, parameters);
        break;
      case CODEC_UNKNOWN:
      default:
        break;
    }
  }
}

opj_image_t* OPJ_CALLCONV opj_decode(opj_dinfo_t *dinfo, opj_cio_t *cio) {
  if(dinfo && cio) {
    switch(dinfo->codec_format) {
      case CODEC_J2K:
        return j2k_decode((opj_j2k_t*)dinfo->j2k_handle, cio);
      case CODEC_JPT:
        return j2k_decode_jpt_stream((opj_j2k_t*)dinfo->j2k_handle, cio);
      case CODEC_JP2:
        return jp2_decode((opj_jp2_t*)dinfo->jp2_handle, cio);
      case CODEC_UNKNOWN:
      default:
        break;
    }
  }

  return NULL;
}

opj_cinfo_t* OPJ_CALLCONV opj_create_compress(OPJ_CODEC_FORMAT format) {
  opj_cinfo_t *cinfo = (opj_cinfo_t*)opj_malloc(sizeof(opj_cinfo_t));
  if(!cinfo) return NULL;
  cinfo->is_decompressor = false;
  switch(format) {
    case CODEC_J2K:
      /* get a J2K coder handle */
      cinfo->j2k_handle = (void*)j2k_create_compress((opj_common_ptr)cinfo);
      if(!cinfo->j2k_handle) {
        opj_free(cinfo);
        return NULL;
      }
      break;
    case CODEC_JP2:
      /* get a JP2 coder handle */
      cinfo->jp2_handle = (void*)jp2_create_compress((opj_common_ptr)cinfo);
      if(!cinfo->jp2_handle) {
        opj_free(cinfo);
        return NULL;
      }
      break;
    case CODEC_JPT:
    case CODEC_UNKNOWN:
    default:
      opj_free(cinfo);
      return NULL;
  }

  cinfo->codec_format = format;

  return cinfo;
}

void OPJ_CALLCONV opj_destroy_compress(opj_cinfo_t *cinfo) {
  if(cinfo) {
    /* destroy the codec */
    switch(cinfo->codec_format) {
      case CODEC_J2K:
        j2k_destroy_decompress((opj_j2k_t*)cinfo->j2k_handle);
        break;
      case CODEC_JP2:
        jp2_destroy_decompress((opj_jp2_t*)cinfo->jp2_handle);
        break;
      case CODEC_JPT:
      case CODEC_UNKNOWN:
      default:
        break;
    }
    /* destroy the decompressor */
    opj_free(cinfo);
  }
}

void OPJ_CALLCONV opj_set_default_encoder_parameters(opj_cparameters_t *parameters) {
  if(parameters) {
    memset(parameters, 0, sizeof(opj_cparameters_t));
    /* default coding parameters */
    parameters->numresolution = 6;
    parameters->cblockw_init = 64;
    parameters->cblockh_init = 64;
    parameters->prog_order = LRCP;
    parameters->roi_compno = -1;    /* no ROI */
    parameters->subsampling_dx = 1;
    parameters->subsampling_dy = 1;

    parameters->decod_format = -1;
    parameters->cod_format = -1;
  }
}

void OPJ_CALLCONV opj_setup_encoder(opj_cinfo_t *cinfo, opj_cparameters_t *parameters, opj_image_t *image) {
  if(cinfo && parameters && image) {
    switch(cinfo->codec_format) {
      case CODEC_J2K:
        j2k_setup_encoder((opj_j2k_t*)cinfo->j2k_handle, parameters, image);
        break;
      case CODEC_JP2:
        jp2_setup_encoder((opj_jp2_t*)cinfo->jp2_handle, parameters, image);
        break;
      case CODEC_JPT:
      case CODEC_UNKNOWN:
      default:
        break;
    }
  }
}

bool OPJ_CALLCONV opj_encode(opj_cinfo_t *cinfo, opj_cio_t *cio, opj_image_t *image, char *index) {
  if(cinfo && cio && image) {
    switch(cinfo->codec_format) {
      case CODEC_J2K:
        return j2k_encode((opj_j2k_t*)cinfo->j2k_handle, cio, image, index);
      case CODEC_JP2:
        return jp2_encode((opj_jp2_t*)cinfo->jp2_handle, cio, image, index);
      case CODEC_JPT:
      case CODEC_UNKNOWN:
      default:
        break;
    }
  }

  return false;
}


