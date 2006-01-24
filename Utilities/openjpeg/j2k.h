/*
 * Copyright (c) 2001-2003, David Janssens
 * Copyright (c) 2002-2003, Yannick Verschueren
 * Copyright (c) 2003-2005, Francois Devaux and Antonin Descampe
 * Copyright (c) 2005, Hervé Drolon, FreeImage Team
 * Copyright (c) 2002-2005, Communications and remote sensing Laboratory, Universite catholique de Louvain, Belgium
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
#ifndef __J2K_H
#define __J2K_H
/**
@file j2k.h
@brief The JPEG-2000 Codestream Reader/Writer (J2K)

The functions in J2K.C have for goal to read/write the several parts of the codestream: markers and data.
*/

/** @defgroup J2K J2K - JPEG-2000 codestream reader/writer */
/*@{*/

#define J2K_CP_CSTY_PRT 0x01
#define J2K_CP_CSTY_SOP 0x02
#define J2K_CP_CSTY_EPH 0x04
#define J2K_CCP_CSTY_PRT 0x01
#define J2K_CCP_CBLKSTY_LAZY 0x01
#define J2K_CCP_CBLKSTY_RESET 0x02
#define J2K_CCP_CBLKSTY_TERMALL 0x04
#define J2K_CCP_CBLKSTY_VSC 0x08
#define J2K_CCP_CBLKSTY_PTERM 0x10
#define J2K_CCP_CBLKSTY_SEGSYM 0x20
#define J2K_CCP_QNTSTY_NOQNT 0
#define J2K_CCP_QNTSTY_SIQNT 1
#define J2K_CCP_QNTSTY_SEQNT 2

/* ----------------------------------------------------------------------- */

#define J2K_MS_SOC 0xff4f  /**< SOC marker value */
#define J2K_MS_SOT 0xff90  /**< SOT marker value */
#define J2K_MS_SOD 0xff93  /**< SOD marker value */
#define J2K_MS_EOC 0xffd9  /**< EOC marker value */
#define J2K_MS_SIZ 0xff51  /**< SIZ marker value */
#define J2K_MS_COD 0xff52  /**< COD marker value */
#define J2K_MS_COC 0xff53  /**< COC marker value */
#define J2K_MS_RGN 0xff5e  /**< RGN marker value */
#define J2K_MS_QCD 0xff5c  /**< QCD marker value */
#define J2K_MS_QCC 0xff5d  /**< QCC marker value */
#define J2K_MS_POC 0xff5f  /**< POC marker value */
#define J2K_MS_TLM 0xff55  /**< TLM marker value */
#define J2K_MS_PLM 0xff57  /**< PLM marker value */
#define J2K_MS_PLT 0xff58  /**< PLT marker value */
#define J2K_MS_PPM 0xff60  /**< PPM marker value */
#define J2K_MS_PPT 0xff61  /**< PPT marker value */
#define J2K_MS_SOP 0xff91  /**< SOP marker value */
#define J2K_MS_EPH 0xff92  /**< EPH marker value */
#define J2K_MS_CRG 0xff63  /**< CRG marker value */
#define J2K_MS_COM 0xff64  /**< COM marker value */

/* ----------------------------------------------------------------------- */

/**
Values that specify the status of the decoding process when decoding the main header. 
These values may be combined with a | operator. 
*/
typedef enum J2K_STATUS {
  J2K_STATE_MHSOC  = 0x0001, /**< a SOC marker is expected */
  J2K_STATE_MHSIZ  = 0x0002, /**< a SIZ marker is expected */
  J2K_STATE_MH     = 0x0004, /**< the decoding process is in the main header */
  J2K_STATE_TPHSOT = 0x0008, /**< the decoding process is in a tile part header and expects a SOT marker */
  J2K_STATE_TPH    = 0x0010, /**< the decoding process is in a tile part header */
  J2K_STATE_MT     = 0x0020, /**< the EOC marker has just been read */
  J2K_STATE_NEOC   = 0x0040  /**< the decoding process must not expect a EOC marker because the codestream is truncated */
} J2K_STATUS;

/* ----------------------------------------------------------------------- */

/**
Quantization stepsize
*/
typedef struct opj_stepsize {
  /** exponent */
  int expn;
  /** mantissa */
  int mant;
} opj_stepsize_t;

/**
Tile-component coding parameters
*/
typedef struct opj_tccp {
  /** coding style */
  int csty;
  /** number of resolutions */
  int numresolutions;
  /** code-blocks width */
  int cblkw;
  /** code-blocks height */
  int cblkh;
  /** code-block coding style */
  int cblksty;
  /** discrete wavelet transform identifier */
  int qmfbid;
  /** quantisation style */
  int qntsty;
  /** stepsizes used for quantization */
  opj_stepsize_t stepsizes[J2K_MAXBANDS];
  /** number of guard bits */
  int numgbits;
  /** Region Of Interest shift */
  int roishift;
  /** precinct width */
  int prcw[J2K_MAXRLVLS];
  /** precinct height */
  int prch[J2K_MAXRLVLS];  
} opj_tccp_t;

/**
Tile coding parameters : 
this structure is used to store coding/decoding parameters common to all
tiles (information like COD, COC in main header)
*/
typedef struct opj_tcp {
  /** 1 : first part-tile of a tile */
  int first;
  /** coding style */
  int csty;
  /** progression order */
  OPJ_PROG_ORDER prg;
  /** number of layers */
  int numlayers;
  /** multi-component transform identifier */
  int mct;
  /** rates of layers */
  int rates[100];
  /** number of progression order changes */
  int numpocs;
  /** indicates if a POC marker has been used O:NO, 1:YES */
  int POC;
  /** progression order changes */
  opj_poc_t pocs[32];
  /** packet header store there for futur use in t2_decode_packet */
  unsigned char *ppt_data;
  /** pointer remaining on the first byte of the first header if ppt is used */
  unsigned char *ppt_data_first;
  /** If ppt == 1 --> there was a PPT marker for the present tile */
  int ppt;
  /** used in case of multiple marker PPT (number of info already stored) */
  int ppt_store;
  /** ppmbug1 */
  int ppt_len;
  /** add fixed_quality */
  float distoratio[100];
  /** tile-component coding parameters */
  opj_tccp_t *tccps;
} opj_tcp_t;

/**
Coding parameters
*/
typedef struct opj_cp {
  /** allocation by rate/distortion */
  int disto_alloc;
  /** allocation by fixed layer */
  int fixed_alloc;
  /** add fixed_quality */
  int fixed_quality;
  /** if != 0, then original dimension divided by 2^(reduce); if == 0 or not used, image is decoded to the full resolution */
  int reduce;
  /** if != 0, then only the first "layer" layers are decoded; if == 0 or not used, all the quality layers are decoded */
  int layer;
  /** 0 = no index || 1 = index */
  int index_on;
  /** XTOsiz */
  int tx0;
  /** YTOsiz */
  int ty0;
  /** XTsiz */
  int tdx;
  /** YTsiz */
  int tdy;
  /** comment for coding */
  char *comment;
  /** number of tiles in width */
  int tw;
  /** number of tiles in heigth */
  int th;
  /** ID number of the tiles present in the codestream */
  int *tileno;
  /** size of the vector tileno */
  int tileno_size;
  /** packet header store there for futur use in t2_decode_packet */
  unsigned char *ppm_data;
  /** pointer remaining on the first byte of the first header if ppm is used */
  unsigned char *ppm_data_first;
  /** if ppm == 1 --> there was a PPM marker for the present tile */
  int ppm;
  /** use in case of multiple marker PPM (number of info already store) */
  int ppm_store;
  /** use in case of multiple marker PPM (case on non-finished previous info) */
  int ppm_previous;
  /** ppmbug1 */
  int ppm_len;
  /** tile coding parameters */
  opj_tcp_t *tcps;
  /** fixed layer */
  int *matrice;
} opj_cp_t;

/**
Information concerning a packet inside tile
*/
typedef struct opj_packet_info {
  /** start position */
  int start_pos;
  /** end position */
  int end_pos;
  /** ADD for Marcela */
  double disto;
} opj_packet_info_t;

/**
Index structure : information regarding tiles inside image
*/
typedef struct opj_tile_info {
  /** value of thresh for each layer by tile cfr. Marcela   */
  double *thresh;
  /** number of tile */
  int num_tile;
  /** start position */
  int start_pos;
  /** end position of the header */
  int end_header;
  /** end position */
  int end_pos;
  /** precinct number for each resolution level (width) */
  int pw[33];
  /** precinct number for each resolution level (height) */
  int ph[33];
  /** precinct size (in power of 2), in X for each resolution level */
  int pdx[33];
  /** precinct size (in power of 2), in Y for each resolution level */
  int pdy[33];
  /** information concerning packets inside tile */
  opj_packet_info_t *packet;
  /** add fixed_quality */
  int nbpix;
  /** add fixed_quality */
  double distotile;
} opj_tile_info_t;

/**
Index structure
*/
typedef struct opj_image_info {
  /** 0 = no index || 1 = index */
  int index_on;
  /** maximum distortion reduction on the whole image (add for Marcela) */
  double D_max;
  /** packet number */
  int num;
  /** writing the packet in the index with t2_encode_packets */
  int index_write;
  /** image width */
  int image_w;
  /** image height */
  int image_h;
  /** progression order */
  OPJ_PROG_ORDER prog;
  /** tile size in x */
  int tile_x;
  /** tile size in y */
  int tile_y;
  /** */
  int tile_Ox;
  /** */
  int tile_Oy;
  /** number of tiles in X */
  int tw;
  /** number of tiles in Y */
  int th;
  /** component numbers */
  int comp;
  /** number of layer */
  int layer;
  /** number of decomposition */
  int decomposition;
  /** main header position */
  int main_head_end;
  /** codestream's size */
  int codestream_size;
  /** information regarding tiles inside image */
  opj_tile_info_t *tile;
} opj_image_info_t;

/**
JPEG-2000 codestream reader/writer
*/
typedef struct opj_j2k {
  /** codec context */
  opj_common_ptr cinfo;

  /** locate in which part of the codestream the decoder is (main header, tile header, end) */
  int state;
  /** number of the tile curently concern by coding/decoding */
  int curtileno;
  /** 
  locate the position of the end of the tile in the codestream, 
  used to detect a truncated codestream (in j2k_read_sod)
  */
  unsigned char *eot;
  /**
  locate the start position of the SOT marker of the current coded tile:  
  after encoding the tile, a jump (in j2k_write_sod) is done to the SOT marker to store the value of its length. 
  */
  int sot_start;
  int sod_start;
  /**
  as the J2K-file is written in several parts during encoding, 
  it enables to make the right correction in position return by cio_tell
  */
  int pos_correction;
  /** array used to store the data of each tile */
  unsigned char **tile_data;
  /** array used to store the length of each tile */
  int *tile_len;
  /** 
  decompression only : 
  store decoding parameters common to all tiles (information like COD, COC in main header)
  */
  opj_tcp_t *default_tcp;
  /** pointer to the encoded / decoded image */
  opj_image_t *image;
  /** pointer to the coding parameters */
  opj_cp_t *cp;
  /** helper used to write the index file */
  opj_image_info_t *image_info;
  /** pointer to the byte i/o stream */
  opj_cio_t *cio;
} opj_j2k_t;

/** @name Exported functions */
/*@{*/
/* ----------------------------------------------------------------------- */
/**
Creates a J2K decompression structure
@param cinfo Codec context info
@return Returns a handle to a J2K decompressor if successful, returns NULL otherwise
*/
opj_j2k_t* j2k_create_decompress(opj_common_ptr cinfo);
/**
Destroy a J2K decompressor handle
@param j2k J2K decompressor handle to destroy
*/
void j2k_destroy_decompress(opj_j2k_t *j2k);
/**
Setup the decoder decoding parameters using user parameters.
Decoding parameters are returned in j2k->cp. 
@param j2k J2K decompressor handle
@param parameters decompression parameters
*/
void j2k_setup_decoder(opj_j2k_t *j2k, opj_dparameters_t *parameters);
/**
Decode an image from a JPEG-2000 codestream
@param j2k J2K decompressor handle
@param cio Input buffer stream
@return Returns a decoded image if successful, returns NULL otherwise
*/
opj_image_t* j2k_decode(opj_j2k_t *j2k, opj_cio_t *cio);
/**
Decode an image form a JPT-stream (JPEG 2000, JPIP)
@param j2k J2K decompressor handle
@param cio Input buffer stream
@return Returns a decoded image if successful, returns NULL otherwise
*/
opj_image_t* j2k_decode_jpt_stream(opj_j2k_t *j2k, opj_cio_t *cio);
/**
Creates a J2K compression structure
@param cinfo Codec context info
@return Returns a handle to a J2K compressor if successful, returns NULL otherwise
*/
opj_j2k_t* j2k_create_compress(opj_common_ptr cinfo);
/**
Destroy a J2K compressor handle
@param j2k J2K compressor handle to destroy
*/
void j2k_destroy_compress(opj_j2k_t *j2k);
/**
Setup the encoder parameters using the current image and using user parameters. 
Coding parameters are returned in j2k->cp. 
@param j2k J2K compressor handle
@param parameters compression parameters
@param image input filled image
*/
void j2k_setup_encoder(opj_j2k_t *j2k, opj_cparameters_t *parameters, opj_image_t *image);
/**
Encode an image into a JPEG-2000 codestream
@param j2k J2K compressor handle
@param cio Output buffer stream
@param image Image to encode
@param index Name of the index file if required, NULL otherwise
@return Returns true if successful, returns false otherwise
*/
bool j2k_encode(opj_j2k_t *j2k, opj_cio_t *cio, opj_image_t *image, char *index);
/* ----------------------------------------------------------------------- */
/*@}*/

/*@}*/

#endif /* __J2K_H */
