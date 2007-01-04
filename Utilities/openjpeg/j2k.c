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

#include "opj_includes.h"

/** @defgroup J2K J2K - JPEG-2000 codestream reader/writer */
/*@{*/

/** @name Local static functions */
/*@{*/

/**
Write the SOC marker (Start Of Codestream)
@param j2k J2K handle
*/
static void j2k_write_soc(opj_j2k_t *j2k);
/**
Read the SOC marker (Start of Codestream)
@param j2k J2K handle
*/
static void j2k_read_soc(opj_j2k_t *j2k);
/**
Write the SIZ marker (image and tile size)
@param j2k J2K handle
*/
static void j2k_write_siz(opj_j2k_t *j2k);
/**
Read the SIZ marker (image and tile size)
@param j2k J2K handle
*/
static void j2k_read_siz(opj_j2k_t *j2k);
/**
Write the COM marker (comment)
@param j2k J2K handle
*/
static void j2k_write_com(opj_j2k_t *j2k);
/**
Read the COM marker (comment)
@param j2k J2K handle
*/
static void j2k_read_com(opj_j2k_t *j2k);
/**
Write the value concerning the specified component in the marker COD and COC
@param j2k J2K handle
@param compno Number of the component concerned by the information written
*/
static void j2k_write_cox(opj_j2k_t *j2k, int compno);
/**
Read the value concerning the specified component in the marker COD and COC
@param j2k J2K handle
@param compno Number of the component concerned by the information read
*/
static void j2k_read_cox(opj_j2k_t *j2k, int compno);
/**
Write the COD marker (coding style default)
@param j2k J2K handle
*/
static void j2k_write_cod(opj_j2k_t *j2k);
/**
Read the COD marker (coding style default)
@param j2k J2K handle
*/
static void j2k_read_cod(opj_j2k_t *j2k);
/**
Write the COC marker (coding style component)
@param j2k J2K handle
@param compno Number of the component concerned by the information written
*/
static void j2k_write_coc(opj_j2k_t *j2k, int compno);
/**
Read the COC marker (coding style component)
@param j2k J2K handle
*/
static void j2k_read_coc(opj_j2k_t *j2k);
/**
Write the value concerning the specified component in the marker QCD and QCC
@param j2k J2K handle
@param compno Number of the component concerned by the information written
*/
static void j2k_write_qcx(opj_j2k_t *j2k, int compno);
/**
Read the value concerning the specified component in the marker QCD and QCC
@param j2k J2K handle
@param compno Number of the component concern by the information read
@param len Length of the information in the QCX part of the marker QCD/QCC
*/
static void j2k_read_qcx(opj_j2k_t *j2k, int compno, int len);
/**
Write the QCD marker (quantization default)
@param j2k J2K handle
*/
static void j2k_write_qcd(opj_j2k_t *j2k);
/**
Read the QCD marker (quantization default)
@param j2k J2K handle
*/
static void j2k_read_qcd(opj_j2k_t *j2k);
/**
Write the QCC marker (quantization component)
@param j2k J2K handle
@param compno Number of the component concerned by the information written
*/
static void j2k_write_qcc(opj_j2k_t *j2k, int compno);
/**
Read the QCC marker (quantization component)
@param j2k J2K handle
*/
static void j2k_read_qcc(opj_j2k_t *j2k);
/**
Write the POC marker (progression order change)
@param j2k J2K handle
*/
static void j2k_write_poc(opj_j2k_t *j2k);
/**
Read the POC marker (progression order change)
@param j2k J2K handle
*/
static void j2k_read_poc(opj_j2k_t *j2k);
/**
Read the CRG marker (component registration)
@param j2k J2K handle
*/
static void j2k_read_crg(opj_j2k_t *j2k);
/**
Read the TLM marker (tile-part lengths)
@param j2k J2K handle
*/
static void j2k_read_tlm(opj_j2k_t *j2k);
/**
Read the PLM marker (packet length, main header)
@param j2k J2K handle
*/
static void j2k_read_plm(opj_j2k_t *j2k);
/**
Read the PLT marker (packet length, tile-part header)
@param j2k J2K handle
*/
static void j2k_read_plt(opj_j2k_t *j2k);
/**
Read the PPM marker (packet packet headers, main header)
@param j2k J2K handle
*/
static void j2k_read_ppm(opj_j2k_t *j2k);
/**
Read the PPT marker (packet packet headers, tile-part header)
@param j2k J2K handle
*/
static void j2k_read_ppt(opj_j2k_t *j2k);
/**
Write the SOT marker (start of tile-part)
@param j2k J2K handle
*/
static void j2k_write_sot(opj_j2k_t *j2k);
/**
Read the SOT marker (start of tile-part)
@param j2k J2K handle
*/
static void j2k_read_sot(opj_j2k_t *j2k);
/**
Write the SOD marker (start of data)
@param j2k J2K handle
@param tile_coder Pointer to a TCD handle
*/
static void j2k_write_sod(opj_j2k_t *j2k, void *tile_coder);
/**
Read the SOD marker (start of data)
@param j2k J2K handle
*/
static void j2k_read_sod(opj_j2k_t *j2k);
/**
Write the RGN marker (region-of-interest)
@param j2k J2K handle
@param compno Number of the component concerned by the information written
@param tileno Number of the tile concerned by the information written
*/
static void j2k_write_rgn(opj_j2k_t *j2k, int compno, int tileno);
/**
Read the RGN marker (region-of-interest)
@param j2k J2K handle
*/
static void j2k_read_rgn(opj_j2k_t *j2k);
/**
Write the EOC marker (end of codestream)
@param j2k J2K handle
*/
static void j2k_write_eoc(opj_j2k_t *j2k);
/**
Read the EOC marker (end of codestream)
@param j2k J2K handle
*/
static void j2k_read_eoc(opj_j2k_t *j2k);
/**
Read an unknown marker
@param j2k J2K handle
*/
static void j2k_read_unk(opj_j2k_t *j2k);

/*@}*/

/*@}*/

/* ----------------------------------------------------------------------- */

void j2k_dump_image(FILE *fd, opj_image_t * img) {
  int compno;
  fprintf(fd, "image {\n");
  fprintf(fd, "  x0=%d, y0=%d, x1=%d, y1=%d\n", img->x0, img->y0, img->x1, img->y1);
  fprintf(fd, "  numcomps=%d\n", img->numcomps);
  for (compno = 0; compno < img->numcomps; compno++) {
    opj_image_comp_t *comp = &img->comps[compno];
    fprintf(fd, "  comp %d {\n", compno);
    fprintf(fd, "    dx=%d, dy=%d\n", comp->dx, comp->dy);
    fprintf(fd, "    prec=%d\n", comp->prec);
    fprintf(fd, "    sgnd=%d\n", comp->sgnd);
    fprintf(fd, "  }\n");
  }
  fprintf(fd, "}\n");
}

void j2k_dump_cp(FILE *fd, opj_image_t * img, opj_cp_t * cp) {
  int tileno, compno, layno, bandno, resno, numbands;
  fprintf(fd, "coding parameters {\n");
  fprintf(fd, "  tx0=%d, ty0=%d\n", cp->tx0, cp->ty0);
  fprintf(fd, "  tdx=%d, tdy=%d\n", cp->tdx, cp->tdy);
  fprintf(fd, "  tw=%d, th=%d\n", cp->tw, cp->th);
  for (tileno = 0; tileno < cp->tw * cp->th; tileno++) {
    opj_tcp_t *tcp = &cp->tcps[tileno];
    fprintf(fd, "  tile %d {\n", tileno);
    fprintf(fd, "    csty=%x\n", tcp->csty);
    fprintf(fd, "    prg=%d\n", tcp->prg);
    fprintf(fd, "    numlayers=%d\n", tcp->numlayers);
    fprintf(fd, "    mct=%d\n", tcp->mct);
    fprintf(fd, "    rates=");
    for (layno = 0; layno < tcp->numlayers; layno++) {
      fprintf(fd, "%d ", tcp->rates[layno]);
    }
    fprintf(fd, "\n");
    for (compno = 0; compno < img->numcomps; compno++) {
      opj_tccp_t *tccp = &tcp->tccps[compno];
      fprintf(fd, "    comp %d {\n", compno);
      fprintf(fd, "      csty=%x\n", tccp->csty);
      fprintf(fd, "      numresolutions=%d\n", tccp->numresolutions);
      fprintf(fd, "      cblkw=%d\n", tccp->cblkw);
      fprintf(fd, "      cblkh=%d\n", tccp->cblkh);
      fprintf(fd, "      cblksty=%x\n", tccp->cblksty);
      fprintf(fd, "      qmfbid=%d\n", tccp->qmfbid);
      fprintf(fd, "      qntsty=%d\n", tccp->qntsty);
      fprintf(fd, "      numgbits=%d\n", tccp->numgbits);
      fprintf(fd, "      roishift=%d\n", tccp->roishift);
      fprintf(fd, "      stepsizes=");
      numbands = tccp->qntsty == J2K_CCP_QNTSTY_SIQNT ? 1 : tccp->numresolutions * 3 - 2;
      for (bandno = 0; bandno < numbands; bandno++) {
        fprintf(fd, "(%d,%d) ", tccp->stepsizes[bandno].mant,
          tccp->stepsizes[bandno].expn);
      }
      fprintf(fd, "\n");
      
      if (tccp->csty & J2K_CCP_CSTY_PRT) {
        fprintf(fd, "      prcw=");
        for (resno = 0; resno < tccp->numresolutions; resno++) {
          fprintf(fd, "%d ", tccp->prcw[resno]);
        }
        fprintf(fd, "\n");
        fprintf(fd, "      prch=");
        for (resno = 0; resno < tccp->numresolutions; resno++) {
          fprintf(fd, "%d ", tccp->prch[resno]);
        }
        fprintf(fd, "\n");
      }
      fprintf(fd, "    }\n");
    }
    fprintf(fd, "  }\n");
  }
  fprintf(fd, "}\n");
}

/* ----------------------------------------------------------------------- */

static void j2k_write_soc(opj_j2k_t *j2k) {
  opj_cio_t *cio = j2k->cio;
  cio_write(cio, J2K_MS_SOC, 2);
}

static void j2k_read_soc(opj_j2k_t *j2k) {
  j2k->state = J2K_STATE_MHSIZ;
}

static void j2k_write_siz(opj_j2k_t *j2k) {
  int i;
  int lenp, len;

  opj_cio_t *cio = j2k->cio;
  opj_image_t *image = j2k->image;
  opj_cp_t *cp = j2k->cp;
  
  cio_write(cio, J2K_MS_SIZ, 2);  /* SIZ */
  lenp = cio_tell(cio);
  cio_skip(cio, 2);
  cio_write(cio, 0, 2);      /* Rsiz (capabilities) */
  cio_write(cio, image->x1, 4);  /* Xsiz */
  cio_write(cio, image->y1, 4);  /* Ysiz */
  cio_write(cio, image->x0, 4);  /* X0siz */
  cio_write(cio, image->y0, 4);  /* Y0siz */
  cio_write(cio, cp->tdx, 4);    /* XTsiz */
  cio_write(cio, cp->tdy, 4);    /* YTsiz */
  cio_write(cio, cp->tx0, 4);    /* XT0siz */
  cio_write(cio, cp->ty0, 4);    /* YT0siz */
  cio_write(cio, image->numcomps, 2);  /* Csiz */
  for (i = 0; i < image->numcomps; i++) {
    cio_write(cio, image->comps[i].prec - 1 + (image->comps[i].sgnd << 7), 1);  /* Ssiz_i */
    cio_write(cio, image->comps[i].dx, 1);  /* XRsiz_i */
    cio_write(cio, image->comps[i].dy, 1);  /* YRsiz_i */
  }
  len = cio_tell(cio) - lenp;
  cio_seek(cio, lenp);
  cio_write(cio, len, 2);    /* Lsiz */
  cio_seek(cio, lenp + len);
}

static void j2k_read_siz(opj_j2k_t *j2k) {
  int len, i;
  
  opj_cio_t *cio = j2k->cio;
  opj_image_t *image = j2k->image;
  opj_cp_t *cp = j2k->cp;
  
  len = cio_read(cio, 2);      /* Lsiz */
  cio_read(cio, 2);        /* Rsiz (capabilities) */
  image->x1 = cio_read(cio, 4);  /* Xsiz */
  image->y1 = cio_read(cio, 4);  /* Ysiz */
  image->x0 = cio_read(cio, 4);  /* X0siz */
  image->y0 = cio_read(cio, 4);  /* Y0siz */
  cp->tdx = cio_read(cio, 4);    /* XTsiz */
  cp->tdy = cio_read(cio, 4);    /* YTsiz */
  cp->tx0 = cio_read(cio, 4);    /* XT0siz */
  cp->ty0 = cio_read(cio, 4);    /* YT0siz */
  
  image->numcomps = cio_read(cio, 2);  /* Csiz */
  image->comps = (opj_image_comp_t *) opj_malloc(image->numcomps * sizeof(opj_image_comp_t));
  for (i = 0; i < image->numcomps; i++) {
    int tmp, w, h;
    tmp = cio_read(cio, 1);    /* Ssiz_i */
    image->comps[i].prec = (tmp & 0x7f) + 1;
    image->comps[i].sgnd = tmp >> 7;
    image->comps[i].dx = cio_read(cio, 1);  /* XRsiz_i */
    image->comps[i].dy = cio_read(cio, 1);  /* YRsiz_i */
    
    /* TODO: unused ? */
    w = int_ceildiv(image->x1 - image->x0, image->comps[i].dx);
    h = int_ceildiv(image->y1 - image->y0, image->comps[i].dy);

    image->comps[i].resno_decoded = 0;  /* number of resolution decoded */
    image->comps[i].factor = 0;      /* reducing factor per component */
  }
  
  cp->tw = int_ceildiv(image->x1 - cp->tx0, cp->tdx);
  cp->th = int_ceildiv(image->y1 - cp->ty0, cp->tdy);
  cp->tcps = (opj_tcp_t *) opj_malloc(cp->tw * cp->th * sizeof(opj_tcp_t));
  cp->tileno = (int *) opj_malloc(cp->tw * cp->th * sizeof(int));
  cp->tileno_size = 0;
  
  for (i = 0; i < cp->tw * cp->th; i++) {
    cp->tcps[i].POC = 0;
    cp->tcps[i].numpocs = 0;
    cp->tcps[i].first = 1;
  }
  
  /* Initialization for PPM marker */
  cp->ppm = 0;
  cp->ppm_data = NULL;
  cp->ppm_data_first = NULL;
  cp->ppm_previous = 0;
  cp->ppm_store = 0;
  
  j2k->default_tcp->tccps = (opj_tccp_t *) opj_malloc(sizeof(opj_tccp_t) * image->numcomps);
  for (i = 0; i < cp->tw * cp->th; i++) {
    cp->tcps[i].tccps = (opj_tccp_t *) opj_malloc(sizeof(opj_tccp_t) * image->numcomps);
  }
  j2k->tile_data = (unsigned char **) opj_malloc(cp->tw * cp->th * sizeof(unsigned char *));
  j2k->tile_len = (int *) opj_malloc(cp->tw * cp->th * sizeof(int));
  j2k->state = J2K_STATE_MH;
}

static void j2k_write_com(opj_j2k_t *j2k) {
  unsigned int i;
  int lenp, len;

  if(j2k->cp->comment) {
    opj_cio_t *cio = j2k->cio;
    char *comment = j2k->cp->comment;

    cio_write(cio, J2K_MS_COM, 2);
    lenp = cio_tell(cio);
    cio_skip(cio, 2);
    cio_write(cio, 0, 2);
    for (i = 0; i < strlen(comment); i++) {
      cio_write(cio, comment[i], 1);
    }
    len = cio_tell(cio) - lenp;
    cio_seek(cio, lenp);
    cio_write(cio, len, 2);
    cio_seek(cio, lenp + len);
  }
}

static void j2k_read_com(opj_j2k_t *j2k) {
  int len;
  
  opj_cio_t *cio = j2k->cio;

  len = cio_read(cio, 2);
  cio_skip(cio, len - 2);  
}

static void j2k_write_cox(opj_j2k_t *j2k, int compno) {
  int i;

  opj_cp_t *cp = j2k->cp;
  opj_tcp_t *tcp = &cp->tcps[j2k->curtileno];
  opj_tccp_t *tccp = &tcp->tccps[compno];
  opj_cio_t *cio = j2k->cio;
  
  cio_write(cio, tccp->numresolutions - 1, 1);  /* SPcox (D) */
  cio_write(cio, tccp->cblkw - 2, 1);        /* SPcox (E) */
  cio_write(cio, tccp->cblkh - 2, 1);        /* SPcox (F) */
  cio_write(cio, tccp->cblksty, 1);        /* SPcox (G) */
  cio_write(cio, tccp->qmfbid, 1);        /* SPcox (H) */
  
  if (tccp->csty & J2K_CCP_CSTY_PRT) {
    for (i = 0; i < tccp->numresolutions; i++) {
      cio_write(cio, tccp->prcw[i] + (tccp->prch[i] << 4), 1);  /* SPcox (I_i) */
    }
  }
}

static void j2k_read_cox(opj_j2k_t *j2k, int compno) {
  int i;

  opj_cp_t *cp = j2k->cp;
  opj_tcp_t *tcp = j2k->state == J2K_STATE_TPH ? &cp->tcps[j2k->curtileno] : j2k->default_tcp;
  opj_tccp_t *tccp = &tcp->tccps[compno];
  opj_cio_t *cio = j2k->cio;

  tccp->numresolutions = cio_read(cio, 1) + 1;  /* SPcox (D) */

  /* check the reduce value */
  cp->reduce = int_min((tccp->numresolutions)-1, cp->reduce);
  tccp->cblkw = cio_read(cio, 1) + 2;  /* SPcox (E) */
  tccp->cblkh = cio_read(cio, 1) + 2;  /* SPcox (F) */
  tccp->cblksty = cio_read(cio, 1);  /* SPcox (G) */
  tccp->qmfbid = cio_read(cio, 1);  /* SPcox (H) */
  if (tccp->csty & J2K_CP_CSTY_PRT) {
    for (i = 0; i < tccp->numresolutions; i++) {
      int tmp = cio_read(cio, 1);  /* SPcox (I_i) */
      tccp->prcw[i] = tmp & 0xf;
      tccp->prch[i] = tmp >> 4;
    }
  }
}

static void j2k_write_cod(opj_j2k_t *j2k) {
  opj_cp_t *cp = NULL;
  opj_tcp_t *tcp = NULL;
  int lenp, len;

  opj_cio_t *cio = j2k->cio;
  
  cio_write(cio, J2K_MS_COD, 2);  /* COD */
  
  lenp = cio_tell(cio);
  cio_skip(cio, 2);
  
  cp = j2k->cp;
  tcp = &cp->tcps[j2k->curtileno];

  cio_write(cio, tcp->csty, 1);    /* Scod */
  cio_write(cio, tcp->prg, 1);    /* SGcod (A) */
  cio_write(cio, tcp->numlayers, 2);  /* SGcod (B) */
  cio_write(cio, tcp->mct, 1);    /* SGcod (C) */
  
  j2k_write_cox(j2k, 0);
  len = cio_tell(cio) - lenp;
  cio_seek(cio, lenp);
  cio_write(cio, len, 2);    /* Lcod */
  cio_seek(cio, lenp + len);
}

static void j2k_read_cod(opj_j2k_t *j2k) {
  int len, i, pos;
  
  opj_cio_t *cio = j2k->cio;
  opj_cp_t *cp = j2k->cp;
  opj_tcp_t *tcp = j2k->state == J2K_STATE_TPH ? &cp->tcps[j2k->curtileno] : j2k->default_tcp;
  opj_image_t *image = j2k->image;
  
  len = cio_read(cio, 2);        /* Lcod */
  tcp->csty = cio_read(cio, 1);    /* Scod */
  tcp->prg = (OPJ_PROG_ORDER)cio_read(cio, 1);    /* SGcod (A) */
  tcp->numlayers = cio_read(cio, 2);  /* SGcod (B) */
  tcp->mct = cio_read(cio, 1);    /* SGcod (C) */
  
  pos = cio_tell(cio);
  for (i = 0; i < image->numcomps; i++) {
    tcp->tccps[i].csty = tcp->csty & J2K_CP_CSTY_PRT;
    cio_seek(cio, pos);
    j2k_read_cox(j2k, i);
  }
}

static void j2k_write_coc(opj_j2k_t *j2k, int compno) {
  int lenp, len;

  opj_cp_t *cp = j2k->cp;
  opj_tcp_t *tcp = &cp->tcps[j2k->curtileno];
  opj_image_t *image = j2k->image;
  opj_cio_t *cio = j2k->cio;
  
  cio_write(cio, J2K_MS_COC, 2);  /* COC */
  lenp = cio_tell(cio);
  cio_skip(cio, 2);
  cio_write(cio, compno, image->numcomps <= 256 ? 1 : 2);  /* Ccoc */
  cio_write(cio, tcp->tccps[compno].csty, 1);  /* Scoc */
  j2k_write_cox(j2k, compno);
  len = cio_tell(cio) - lenp;
  cio_seek(cio, lenp);
  cio_write(cio, len, 2);      /* Lcoc */
  cio_seek(cio, lenp + len);
}

static void j2k_read_coc(opj_j2k_t *j2k) {
  int len, compno;

  opj_cp_t *cp = j2k->cp;
  opj_tcp_t *tcp = j2k->state == J2K_STATE_TPH ? &cp->tcps[j2k->curtileno] : j2k->default_tcp;
  opj_image_t *image = j2k->image;
  opj_cio_t *cio = j2k->cio;
  
  len = cio_read(cio, 2);    /* Lcoc */
  compno = cio_read(cio, image->numcomps <= 256 ? 1 : 2);  /* Ccoc */
  tcp->tccps[compno].csty = cio_read(cio, 1);  /* Scoc */
  j2k_read_cox(j2k, compno);
}

static void j2k_write_qcx(opj_j2k_t *j2k, int compno) {
  int bandno, numbands;
  int expn, mant;
  
  opj_cp_t *cp = j2k->cp;
  opj_tcp_t *tcp = &cp->tcps[j2k->curtileno];
  opj_tccp_t *tccp = &tcp->tccps[compno];
  opj_cio_t *cio = j2k->cio;
  
  cio_write(cio, tccp->qntsty + (tccp->numgbits << 5), 1);  /* Sqcx */
  numbands = tccp->qntsty == J2K_CCP_QNTSTY_SIQNT ? 1 : tccp->numresolutions * 3 - 2;
  
  for (bandno = 0; bandno < numbands; bandno++) {
    expn = tccp->stepsizes[bandno].expn;
    mant = tccp->stepsizes[bandno].mant;
    
    if (tccp->qntsty == J2K_CCP_QNTSTY_NOQNT) {
      cio_write(cio, expn << 3, 1);  /* SPqcx_i */
    } else {
      cio_write(cio, (expn << 11) + mant, 2);  /* SPqcx_i */
    }
  }
}

static void j2k_read_qcx(opj_j2k_t *j2k, int compno, int len) {
  int tmp;
  int bandno, numbands;

  opj_cp_t *cp = j2k->cp;
  opj_tcp_t *tcp = j2k->state == J2K_STATE_TPH ? &cp->tcps[j2k->curtileno] : j2k->default_tcp;
  opj_tccp_t *tccp = &tcp->tccps[compno];
  opj_cio_t *cio = j2k->cio;

  tmp = cio_read(cio, 1);    /* Sqcx */
  tccp->qntsty = tmp & 0x1f;
  tccp->numgbits = tmp >> 5;
  numbands = (tccp->qntsty == J2K_CCP_QNTSTY_SIQNT) ? 
    1 : ((tccp->qntsty == J2K_CCP_QNTSTY_NOQNT) ? len - 1 : (len - 1) / 2);
  for (bandno = 0; bandno < numbands; bandno++) {
    int expn, mant;
    if (tccp->qntsty == J2K_CCP_QNTSTY_NOQNT) {
      expn = cio_read(cio, 1) >> 3;  /* SPqcx_i */
      mant = 0;
    } else {
      tmp = cio_read(cio, 2);  /* SPqcx_i */
      expn = tmp >> 11;
      mant = tmp & 0x7ff;
    }
    tccp->stepsizes[bandno].expn = expn;
    tccp->stepsizes[bandno].mant = mant;
  }
  
  /* Add Antonin : if scalar_derived -> compute other stepsizes */
  if (tccp->qntsty == J2K_CCP_QNTSTY_SIQNT) {
    for (bandno = 1; bandno < J2K_MAXBANDS; bandno++) {
      tccp->stepsizes[bandno].expn = 
        ((tccp->stepsizes[0].expn) - ((bandno - 1) / 3) > 0) ? 
          (tccp->stepsizes[0].expn) - ((bandno - 1) / 3) : 0;
      tccp->stepsizes[bandno].mant = tccp->stepsizes[0].mant;
    }
  }
  /* ddA */
}

static void j2k_write_qcd(opj_j2k_t *j2k) {
  int lenp, len;

  opj_cio_t *cio = j2k->cio;
  
  cio_write(cio, J2K_MS_QCD, 2);  /* QCD */
  lenp = cio_tell(cio);
  cio_skip(cio, 2);
  j2k_write_qcx(j2k, 0);
  len = cio_tell(cio) - lenp;
  cio_seek(cio, lenp);
  cio_write(cio, len, 2);      /* Lqcd */
  cio_seek(cio, lenp + len);
}

static void j2k_read_qcd(opj_j2k_t *j2k) {
  int len, i, pos;

  opj_cio_t *cio = j2k->cio;
  opj_image_t *image = j2k->image;
  
  len = cio_read(cio, 2);    /* Lqcd */
  pos = cio_tell(cio);
  for (i = 0; i < image->numcomps; i++) {
    cio_seek(cio, pos);
    j2k_read_qcx(j2k, i, len - 2);
  }
}

static void j2k_write_qcc(opj_j2k_t *j2k, int compno) {
  int lenp, len;

  opj_cio_t *cio = j2k->cio;
  
  cio_write(cio, J2K_MS_QCC, 2);  /* QCC */
  lenp = cio_tell(cio);
  cio_skip(cio, 2);
  cio_write(cio, compno, j2k->image->numcomps <= 256 ? 1 : 2);  /* Cqcc */
  j2k_write_qcx(j2k, compno);
  len = cio_tell(cio) - lenp;
  cio_seek(cio, lenp);
  cio_write(cio, len, 2);      /* Lqcc */
  cio_seek(cio, lenp + len);
}

static void j2k_read_qcc(opj_j2k_t *j2k) {
  int len, compno;
  int numcomp = j2k->image->numcomps;
  opj_cio_t *cio = j2k->cio;
  
  len = cio_read(cio, 2);  /* Lqcc */
  compno = cio_read(cio, numcomp <= 256 ? 1 : 2);  /* Cqcc */
  j2k_read_qcx(j2k, compno, len - 2 - (numcomp <= 256 ? 1 : 2));
}

static void j2k_write_poc(opj_j2k_t *j2k) {
  int len, numpchgs, i;

  int numcomps = j2k->image->numcomps;
  
  opj_cp_t *cp = j2k->cp;
  opj_tcp_t *tcp = &cp->tcps[j2k->curtileno];
  opj_tccp_t *tccp = &tcp->tccps[0];
  opj_cio_t *cio = j2k->cio;

  numpchgs = tcp->numpocs;
  cio_write(cio, J2K_MS_POC, 2);  /* POC  */
  len = 2 + (5 + 2 * (numcomps <= 256 ? 1 : 2)) * numpchgs;
  cio_write(cio, len, 2);    /* Lpoc */
  for (i = 0; i < numpchgs; i++) {
    opj_poc_t *poc = &tcp->pocs[i];
    cio_write(cio, poc->resno0, 1);  /* RSpoc_i */
    cio_write(cio, poc->compno0, (numcomps <= 256 ? 1 : 2));  /* CSpoc_i */
    cio_write(cio, poc->layno1, 2);  /* LYEpoc_i */
    poc->layno1 = int_min(poc->layno1, tcp->numlayers);
    cio_write(cio, poc->resno1, 1);  /* REpoc_i */
    poc->resno1 = int_min(poc->resno1, tccp->numresolutions);
    cio_write(cio, poc->compno1, (numcomps <= 256 ? 1 : 2));  /* CEpoc_i */
    poc->compno1 = int_min(poc->compno1, numcomps);
    cio_write(cio, poc->prg, 1);  /* Ppoc_i */
  }
}

static void j2k_read_poc(opj_j2k_t *j2k) {
  int len, numpchgs, i, old_poc;

  int numcomps = j2k->image->numcomps;
  
  opj_cp_t *cp = j2k->cp;
  opj_tcp_t *tcp = j2k->state == J2K_STATE_TPH ? &cp->tcps[j2k->curtileno] : j2k->default_tcp;
  opj_tccp_t *tccp = &tcp->tccps[0];
  opj_cio_t *cio = j2k->cio;
  
  old_poc = tcp->POC ? tcp->numpocs + 1 : 0;
  tcp->POC = 1;
  len = cio_read(cio, 2);    /* Lpoc */
  numpchgs = (len - 2) / (5 + 2 * (numcomps <= 256 ? 1 : 2));
  
  for (i = old_poc; i < numpchgs + old_poc; i++) {
    opj_poc_t *poc;
    poc = &tcp->pocs[i];
    poc->resno0 = cio_read(cio, 1);  /* RSpoc_i */
    poc->compno0 = cio_read(cio, numcomps <= 256 ? 1 : 2);  /* CSpoc_i */
    poc->layno1 = int_min(cio_read(cio, 2), (unsigned int) tcp->numlayers);  /* LYEpoc_i */
    poc->resno1 = int_min(cio_read(cio, 1), (unsigned int) tccp->numresolutions);  /* REpoc_i */
    poc->compno1 = int_min(
      cio_read(cio, numcomps <= 256 ? 1 : 2), (unsigned int) numcomps);  /* CEpoc_i */
    poc->prg = (OPJ_PROG_ORDER)cio_read(cio, 1);  /* Ppoc_i */
  }
  
  tcp->numpocs = numpchgs + old_poc - 1;
}

static void j2k_read_crg(opj_j2k_t *j2k) {
  int len, i, Xcrg_i, Ycrg_i;
  
  opj_cio_t *cio = j2k->cio;
  int numcomps = j2k->image->numcomps;
  
  len = cio_read(cio, 2);      /* Lcrg */
  for (i = 0; i < numcomps; i++) {
    Xcrg_i = cio_read(cio, 2);  /* Xcrg_i */
    Ycrg_i = cio_read(cio, 2);  /* Ycrg_i */
  }
}

static void j2k_read_tlm(opj_j2k_t *j2k) {
  int len, Ztlm, Stlm, ST, SP, tile_tlm, i;
  long int Ttlm_i, Ptlm_i;

  opj_cio_t *cio = j2k->cio;
  
  len = cio_read(cio, 2);    /* Ltlm */
  Ztlm = cio_read(cio, 1);  /* Ztlm */
  Stlm = cio_read(cio, 1);  /* Stlm */
  ST = ((Stlm >> 4) & 0x01) + ((Stlm >> 4) & 0x02);
  SP = (Stlm >> 6) & 0x01;
  tile_tlm = (len - 4) / ((SP + 1) * 2 + ST);
  for (i = 0; i < tile_tlm; i++) {
    Ttlm_i = cio_read(cio, ST);  /* Ttlm_i */
    Ptlm_i = cio_read(cio, SP ? 4 : 2);  /* Ptlm_i */
  }
}

static void j2k_read_plm(opj_j2k_t *j2k) {
  int len, i, Zplm, Nplm, add, packet_len = 0;
  
  opj_cio_t *cio = j2k->cio;

  len = cio_read(cio, 2);    /* Lplm */
  Zplm = cio_read(cio, 1);  /* Zplm */
  len -= 3;
  while (len > 0) {
    Nplm = cio_read(cio, 4);    /* Nplm */
    len -= 4;
    for (i = Nplm; i > 0; i--) {
      add = cio_read(cio, 1);
      len--;
      packet_len = (packet_len << 7) + add;  /* Iplm_ij */
      if ((add & 0x80) == 0) {
        /* New packet */
        packet_len = 0;
      }
      if (len <= 0)
        break;
    }
  }
}

static void j2k_read_plt(opj_j2k_t *j2k) {
  int len, i, Zplt, packet_len = 0, add;
  
  opj_cio_t *cio = j2k->cio;
  
  len = cio_read(cio, 2);    /* Lplt */
  Zplt = cio_read(cio, 1);  /* Zplt */
  for (i = len - 3; i > 0; i--) {
    add = cio_read(cio, 1);
    packet_len = (packet_len << 7) + add;  /* Iplt_i */
    if ((add & 0x80) == 0) {
      /* New packet */
      packet_len = 0;
    }
  }
}

static void j2k_read_ppm(opj_j2k_t *j2k) {
  int len, Z_ppm, i, j;
  int N_ppm;

  opj_cp_t *cp = j2k->cp;
  opj_cio_t *cio = j2k->cio;
  
  len = cio_read(cio, 2);
  cp->ppm = 1;
  
  Z_ppm = cio_read(cio, 1);  /* Z_ppm */
  len -= 3;
  while (len > 0) {
    if (cp->ppm_previous == 0) {
      N_ppm = cio_read(cio, 4);  /* N_ppm */
      len -= 4;
    } else {
      N_ppm = cp->ppm_previous;
    }
    j = cp->ppm_store;
    if (Z_ppm == 0) {  /* First PPM marker */
      cp->ppm_data = (unsigned char *) opj_malloc(N_ppm * sizeof(unsigned char));
      cp->ppm_data_first = cp->ppm_data;
      cp->ppm_len = N_ppm;
    } else {      /* NON-first PPM marker */
      cp->ppm_data = (unsigned char *) opj_realloc(cp->ppm_data, (N_ppm +  cp->ppm_store) * sizeof(unsigned char));
      cp->ppm_data_first = cp->ppm_data;
      cp->ppm_len = N_ppm + cp->ppm_store;
    }
    for (i = N_ppm; i > 0; i--) {  /* Read packet header */
      cp->ppm_data[j] = cio_read(cio, 1);
      j++;
      len--;
      if (len == 0)
        break;      /* Case of non-finished packet header in present marker but finished in next one */
    }
    cp->ppm_previous = i - 1;
    cp->ppm_store = j;
  }
}

static void j2k_read_ppt(opj_j2k_t *j2k) {
  int len, Z_ppt, i, j = 0;

  opj_cp_t *cp = j2k->cp;
  opj_tcp_t *tcp = cp->tcps + j2k->curtileno;
  opj_cio_t *cio = j2k->cio;

  len = cio_read(cio, 2);
  Z_ppt = cio_read(cio, 1);
  tcp->ppt = 1;
  if (Z_ppt == 0) {    /* First PPT marker */
    tcp->ppt_data = (unsigned char *) opj_malloc((len - 3) * sizeof(unsigned char));
    tcp->ppt_data_first = tcp->ppt_data;
    tcp->ppt_store = 0;
    tcp->ppt_len = len - 3;
  } else {      /* NON-first PPT marker */
    tcp->ppt_data =  (unsigned char *) opj_realloc(tcp->ppt_data, (len - 3 + tcp->ppt_store) * sizeof(unsigned char));
    tcp->ppt_data_first = tcp->ppt_data;
    tcp->ppt_len = len - 3 + tcp->ppt_store;
  }
  j = tcp->ppt_store;
  for (i = len - 3; i > 0; i--) {
    tcp->ppt_data[j] = cio_read(cio, 1);
    j++;
  }
  tcp->ppt_store = j;
}

static void j2k_write_sot(opj_j2k_t *j2k) {
  int lenp, len;

  opj_cio_t *cio = j2k->cio;

  j2k->sot_start = cio_tell(cio);
  cio_write(cio, J2K_MS_SOT, 2);    /* SOT */
  lenp = cio_tell(cio);
  cio_skip(cio, 2);          /* Lsot (further) */
  cio_write(cio, j2k->curtileno, 2);  /* Isot */
  cio_skip(cio, 4);          /* Psot (further in j2k_write_sod) */
  cio_write(cio, 0, 1);        /* TPsot */
  cio_write(cio, 1, 1);        /* TNsot */
  len = cio_tell(cio) - lenp;
  cio_seek(cio, lenp);
  cio_write(cio, len, 2);        /* Lsot */
  cio_seek(cio, lenp + len);
}

static void j2k_read_sot(opj_j2k_t *j2k) {
  int len, tileno, totlen, partno, numparts, i;
  opj_tcp_t *tcp = NULL;
  char status = 0;

  opj_cp_t *cp = j2k->cp;
  opj_cio_t *cio = j2k->cio;
  
  len = cio_read(cio, 2);
  tileno = cio_read(cio, 2);
  
  if (cp->tileno_size == 0) {
    cp->tileno[cp->tileno_size] = tileno;
    cp->tileno_size++;
  } else {
    i = 0;
    while (i < cp->tileno_size && status == 0) {
      status = cp->tileno[i] == tileno ? 1 : 0;
      i++;
    }
    if (status == 0) {
      cp->tileno[cp->tileno_size] = tileno;
      cp->tileno_size++;
    }
  }
  
  totlen = cio_read(cio, 4);
  if (!totlen)
    totlen = cio_numbytesleft(cio) + 8;
  
  partno = cio_read(cio, 1);
  numparts = cio_read(cio, 1);
  
  j2k->curtileno = tileno;
  j2k->eot = cio_getbp(cio) - 12 + totlen;
  j2k->state = J2K_STATE_TPH;
  tcp = &cp->tcps[j2k->curtileno];
  
  if (tcp->first == 1) {
    
    /* Initialization PPT */
    opj_tccp_t *tmp = tcp->tccps;
    memcpy(tcp, j2k->default_tcp, sizeof(opj_tcp_t));
    tcp->ppt = 0;
    tcp->ppt_data = NULL;
    tcp->ppt_data_first = NULL;
    tcp->tccps = tmp;

    for (i = 0; i < j2k->image->numcomps; i++) {
      tcp->tccps[i] = j2k->default_tcp->tccps[i];
    }
    cp->tcps[j2k->curtileno].first = 0;
  }
}

static void j2k_write_sod(opj_j2k_t *j2k, void *tile_coder) {
  int l, layno;
  int totlen;
  opj_tcp_t *tcp = NULL;
  opj_image_info_t *image_info = NULL;
  
  opj_tcd_t *tcd = (opj_tcd_t*)tile_coder;  /* cast is needed because of conflicts in header inclusions */
  opj_cp_t *cp = j2k->cp;
  opj_cio_t *cio = j2k->cio;
  
  cio_write(cio, J2K_MS_SOD, 2);
  if (j2k->curtileno == 0) {
    j2k->sod_start = cio_tell(cio) + j2k->pos_correction;
  }
  
  /* INDEX >> */
  image_info = j2k->image_info;
  if (image_info && image_info->index_on) {
    image_info->tile[j2k->curtileno].end_header = cio_tell(cio) + j2k->pos_correction - 1;
  }
  /* << INDEX */
  
  tcp = &cp->tcps[j2k->curtileno];
  for (layno = 0; layno < tcp->numlayers; layno++) {
    tcp->rates[layno] -= tcp->rates[layno] ? (j2k->sod_start / (cp->th * cp->tw)) : 0;
  }
  if(image_info) {
    image_info->num = 0;
  }
  
  l = tcd_encode_tile(tcd, j2k->curtileno, cio_getbp(cio), cio_numbytesleft(cio) - 2, image_info);
  
  /* Writing Psot in SOT marker */
  totlen = cio_tell(cio) + l - j2k->sot_start;
  cio_seek(cio, j2k->sot_start + 6);
  cio_write(cio, totlen, 4);
  cio_seek(cio, j2k->sot_start + totlen);
}

static void j2k_read_sod(opj_j2k_t *j2k) {
  int len, truncate = 0, i;
  unsigned char *data = NULL, *data_ptr = NULL;

  opj_cio_t *cio = j2k->cio;
  int curtileno = j2k->curtileno;
  
  len = int_min( (int)(j2k->eot - cio_getbp(cio)), cio_numbytesleft(cio) + 1);
  
  if (len == cio_numbytesleft(cio) + 1) {
    truncate = 1;    /* Case of a truncate codestream */
  }
  
  data = (unsigned char *) opj_malloc((j2k->tile_len[curtileno] + len) * sizeof(unsigned char));

  for (i = 0; i < j2k->tile_len[curtileno]; i++) {
    data[i] = j2k->tile_data[curtileno][i];
  }

  data_ptr = data + j2k->tile_len[curtileno];
  for (i = 0; i < len; i++) {
    data_ptr[i] = cio_read(cio, 1);
  }
  
  j2k->tile_len[curtileno] += len;
  opj_free(j2k->tile_data[curtileno]);
  j2k->tile_data[curtileno] = data;
  
  if (!truncate) {
    j2k->state = J2K_STATE_TPHSOT;
  } else {
    j2k->state = J2K_STATE_NEOC;  /* RAJOUTE !! */
  }
}

static void j2k_write_rgn(opj_j2k_t *j2k, int compno, int tileno) {
  
  opj_cp_t *cp = j2k->cp;
  opj_tcp_t *tcp = &cp->tcps[tileno];
  opj_cio_t *cio = j2k->cio;
  int numcomps = j2k->image->numcomps;
  
  cio_write(cio, J2K_MS_RGN, 2);            /* RGN  */
  cio_write(cio, numcomps <= 256 ? 5 : 6, 2);      /* Lrgn */
  cio_write(cio, compno, numcomps <= 256 ? 1 : 2);  /* Crgn */
  cio_write(cio, 0, 1);                /* Srgn */
  cio_write(cio, tcp->tccps[compno].roishift, 1);    /* SPrgn */
}

static void j2k_read_rgn(opj_j2k_t *j2k) {
  int len, compno, roisty;

  opj_cp_t *cp = j2k->cp;
  opj_tcp_t *tcp = j2k->state == J2K_STATE_TPH ? &cp->tcps[j2k->curtileno] : j2k->default_tcp;
  opj_cio_t *cio = j2k->cio;
  int numcomps = j2k->image->numcomps;

  len = cio_read(cio, 2);                    /* Lrgn */
  compno = cio_read(cio, numcomps <= 256 ? 1 : 2);      /* Crgn */
  roisty = cio_read(cio, 1);                  /* Srgn */
  tcp->tccps[compno].roishift = cio_read(cio, 1);        /* SPrgn */
}

static void j2k_write_eoc(opj_j2k_t *j2k) {
  opj_cio_t *cio = j2k->cio;
  /* opj_event_msg(j2k->cinfo, "%.8x: EOC\n", cio_tell(cio) + j2k->pos_correction); */
  cio_write(cio, J2K_MS_EOC, 2);
}

static void j2k_read_eoc(opj_j2k_t *j2k) {
  int i, tileno;

#ifndef NO_PACKETS_DECODING  
  opj_tcd_t *tcd = tcd_create(j2k->cinfo);
  tcd_malloc_decode(tcd, j2k->image, j2k->cp);
  for (i = 0; i < j2k->cp->tileno_size; i++) {
    tileno = j2k->cp->tileno[i];
    tcd_decode_tile(tcd, j2k->tile_data[tileno], j2k->tile_len[tileno], tileno);
    opj_free(j2k->tile_data[tileno]);
    j2k->tile_data[tileno] = NULL;
  }
  tcd_free_decode(tcd);
  tcd_destroy(tcd);
#else 
  for (i = 0; i < j2k->cp->tileno_size; i++) {
    tileno = j2k->cp->tileno[i];
    opj_free(j2k->tile_data[tileno]);
    j2k->tile_data[tileno] = NULL;
  }
#endif
  
  j2k->state = J2K_STATE_MT;
}

static void j2k_read_unk(opj_j2k_t *j2k) {
  opj_event_msg(j2k->cinfo, EVT_WARNING, "Unknown marker\n");
}

typedef struct opj_dec_mstabent {
  /** marker value */
  int id;
  /** value of the state when the marker can appear */
  int states;
  /** action linked to the marker */
  void (*handler) (opj_j2k_t *j2k);
} opj_dec_mstabent_t;

opj_dec_mstabent_t j2k_dec_mstab[] = {
  {J2K_MS_SOC, J2K_STATE_MHSOC, j2k_read_soc},
  {J2K_MS_SOT, J2K_STATE_MH | J2K_STATE_TPHSOT, j2k_read_sot},
  {J2K_MS_SOD, J2K_STATE_TPH, j2k_read_sod},
  {J2K_MS_EOC, J2K_STATE_TPHSOT, j2k_read_eoc},
  {J2K_MS_SIZ, J2K_STATE_MHSIZ, j2k_read_siz},
  {J2K_MS_COD, J2K_STATE_MH | J2K_STATE_TPH, j2k_read_cod},
  {J2K_MS_COC, J2K_STATE_MH | J2K_STATE_TPH, j2k_read_coc},
  {J2K_MS_RGN, J2K_STATE_MH | J2K_STATE_TPH, j2k_read_rgn},
  {J2K_MS_QCD, J2K_STATE_MH | J2K_STATE_TPH, j2k_read_qcd},
  {J2K_MS_QCC, J2K_STATE_MH | J2K_STATE_TPH, j2k_read_qcc},
  {J2K_MS_POC, J2K_STATE_MH | J2K_STATE_TPH, j2k_read_poc},
  {J2K_MS_TLM, J2K_STATE_MH, j2k_read_tlm},
  {J2K_MS_PLM, J2K_STATE_MH, j2k_read_plm},
  {J2K_MS_PLT, J2K_STATE_TPH, j2k_read_plt},
  {J2K_MS_PPM, J2K_STATE_MH, j2k_read_ppm},
  {J2K_MS_PPT, J2K_STATE_TPH, j2k_read_ppt},
  {J2K_MS_SOP, 0, 0},
  {J2K_MS_CRG, J2K_STATE_MH, j2k_read_crg},
  {J2K_MS_COM, J2K_STATE_MH | J2K_STATE_TPH, j2k_read_com},
  {0, J2K_STATE_MH | J2K_STATE_TPH, j2k_read_unk}
};

/**
Read the lookup table containing all the marker, status and action
@param id Marker value
*/
static opj_dec_mstabent_t *j2k_dec_mstab_lookup(int id) {
  opj_dec_mstabent_t *e;
  for (e = j2k_dec_mstab; e->id != 0; e++) {
    if (e->id == id) {
      break;
    }
  }
  return e;
}

/* ----------------------------------------------------------------------- */
/* J2K / JPT decoder interface                                             */
/* ----------------------------------------------------------------------- */

opj_j2k_t* j2k_create_decompress(opj_common_ptr cinfo) {
  opj_j2k_t *j2k = (opj_j2k_t*)opj_malloc(sizeof(opj_j2k_t));
  if(j2k) {
    j2k->cinfo = cinfo;
    j2k->default_tcp = (opj_tcp_t*)opj_malloc(sizeof(opj_tcp_t));
    if(!j2k->default_tcp) {
      opj_free(j2k);
      return NULL;
    }
  }
  return j2k;
}

void j2k_destroy_decompress(opj_j2k_t *j2k) {
  int i = 0;

  if(j2k->tile_len != NULL) {
    opj_free(j2k->tile_len);
  }
  if(j2k->tile_data != NULL) {
    opj_free(j2k->tile_data);
  }
  if(j2k->default_tcp != NULL) {
    opj_tcp_t *default_tcp = j2k->default_tcp;
    if(default_tcp->ppt_data_first != NULL) {
      opj_free(default_tcp->ppt_data_first);
    }
    if(j2k->default_tcp->tccps != NULL) {
      opj_free(j2k->default_tcp->tccps);
    }
    opj_free(j2k->default_tcp);
  }
  if(j2k->cp != NULL) {
    opj_cp_t *cp = j2k->cp;
    if(cp->tcps != NULL) {
      for(i = 0; i < cp->tw * cp->th; i++) {
        if(cp->tcps[i].ppt_data_first != NULL) {
          opj_free(cp->tcps[i].ppt_data_first);
        }
        if(cp->tcps[i].tccps != NULL) {
          opj_free(cp->tcps[i].tccps);
        }
      }
      opj_free(cp->tcps);
    }
    if(cp->ppm_data_first != NULL) {
      opj_free(cp->ppm_data_first);
    }
    if(cp->tileno != NULL) {
      opj_free(cp->tileno);  
    }
    if(cp->comment != NULL) {
      opj_free(cp->comment);
    }

    opj_free(cp);
  }

  opj_free(j2k);
}

void j2k_setup_decoder(opj_j2k_t *j2k, opj_dparameters_t *parameters) {
  if(j2k && parameters) {
    /* create and initialize the coding parameters structure */
    opj_cp_t *cp = (opj_cp_t*)opj_malloc(sizeof(opj_cp_t));
    cp->reduce = parameters->cp_reduce;  
    cp->layer = parameters->cp_layer;

    /* keep a link to cp so that we can destroy it later in j2k_destroy_decompress */
    j2k->cp = cp;
  }
}

opj_image_t* j2k_decode(opj_j2k_t *j2k, opj_cio_t *cio) {
  opj_image_t *image = NULL;

  opj_common_ptr cinfo = j2k->cinfo;

  j2k->cio = cio;

  /* create an empty image */
  image = opj_image_create0();
  j2k->image = image;

  j2k->state = J2K_STATE_MHSOC;

  for (;;) {
    opj_dec_mstabent_t *e;
    int id = cio_read(cio, 2);
    if (id >> 8 != 0xff) {
      opj_image_destroy(image);
      opj_event_msg(cinfo, EVT_ERROR, "%.8x: expected a marker instead of %x\n", cio_tell(cio) - 2, id);
      return 0;
    }
    e = j2k_dec_mstab_lookup(id);
    if (!(j2k->state & e->states)) {
      opj_image_destroy(image);
      opj_event_msg(cinfo, EVT_ERROR, "%.8x: unexpected marker %x\n", cio_tell(cio) - 2, id);
      return 0;
    }
    if (e->handler) {
      (*e->handler)(j2k);
    }
    if (j2k->state == J2K_STATE_MT) {
      break;
    }
    if (j2k->state == J2K_STATE_NEOC) {
      break;
    }
  }
  if (j2k->state == J2K_STATE_NEOC) {
    j2k_read_eoc(j2k);
  }

  if (j2k->state != J2K_STATE_MT) {
    opj_event_msg(cinfo, EVT_WARNING, "Incomplete bitstream\n");
  }

  return image;
}

/*
* Read a JPT-stream and decode file
*
*/
opj_image_t* j2k_decode_jpt_stream(opj_j2k_t *j2k, opj_cio_t *cio) {
  opj_image_t *image = NULL;
  opj_jpt_msg_header_t header;
  int position;

  opj_common_ptr cinfo = j2k->cinfo;
  
  j2k->cio = cio;

  /* create an empty image */
  image = opj_image_create0();

  j2k->state = J2K_STATE_MHSOC;
  
  /* Initialize the header */
  jpt_init_msg_header(&header);
  /* Read the first header of the message */
  jpt_read_msg_header(cinfo, cio, &header);
  
  position = cio_tell(cio);
  if (header.Class_Id != 6) {  /* 6 : Main header data-bin message */
    opj_image_destroy(image);
    opj_event_msg(cinfo, EVT_ERROR, "[JPT-stream] : Expecting Main header first [class_Id %d] !\n", header.Class_Id);
    return 0;
  }
  
  for (;;) {
    opj_dec_mstabent_t *e = NULL;
    int id;
    
    if (!cio_numbytesleft(cio)) {
      j2k_read_eoc(j2k);
      return image;
    }
    /* data-bin read -> need to read a new header */
    if ((unsigned int) (cio_tell(cio) - position) == header.Msg_length) {
      jpt_read_msg_header(cinfo, cio, &header);
      position = cio_tell(cio);
      if (header.Class_Id != 4) {  /* 4 : Tile data-bin message */
        opj_image_destroy(image);
        opj_event_msg(cinfo, EVT_ERROR, "[JPT-stream] : Expecting Tile info !\n");
        return 0;
      }
    }
    
    id = cio_read(cio, 2);
    if (id >> 8 != 0xff) {
      opj_image_destroy(image);
      opj_event_msg(cinfo, EVT_ERROR, "%.8x: expected a marker instead of %x\n", cio_tell(cio) - 2, id);
      return 0;
    }
    e = j2k_dec_mstab_lookup(id);
    if (!(j2k->state & e->states)) {
      opj_image_destroy(image);
      opj_event_msg(cinfo, EVT_ERROR, "%.8x: unexpected marker %x\n", cio_tell(cio) - 2, id);
      return 0;
    }
    if (e->handler) {
      (*e->handler)(j2k);
    }
    if (j2k->state == J2K_STATE_MT) {
      break;
    }
    if (j2k->state == J2K_STATE_NEOC) {
      break;
    }
  }
  if (j2k->state == J2K_STATE_NEOC) {
    j2k_read_eoc(j2k);
  }
  
  if (j2k->state != J2K_STATE_MT) {
    opj_event_msg(cinfo, EVT_WARNING, "Incomplete bitstream\n");
  }

  return image;
}

/* ----------------------------------------------------------------------- */
/* J2K encoder interface                                                       */
/* ----------------------------------------------------------------------- */

opj_j2k_t* j2k_create_compress(opj_common_ptr cinfo) {
  opj_j2k_t *j2k = (opj_j2k_t*)opj_malloc(sizeof(opj_j2k_t));
  if(j2k) {
    j2k->cinfo = cinfo;
  }
  return j2k;
}

void j2k_destroy_compress(opj_j2k_t *j2k) {
  int tileno;

  if(!j2k) return;

  if(j2k->image_info != NULL) {
    opj_image_info_t *image_info = j2k->image_info;
    if (image_info->index_on && j2k->cp) {
      opj_cp_t *cp = j2k->cp;
      for (tileno = 0; tileno < cp->tw * cp->th; tileno++) {
        opj_tile_info_t *tile_info = &image_info->tile[tileno];
        opj_free(tile_info->thresh);
        opj_free(tile_info->packet);
      }
      opj_free(image_info->tile);
    }
    opj_free(image_info);
  }
  if(j2k->cp != NULL) {
    opj_cp_t *cp = j2k->cp;

    if(cp->comment) {
      opj_free(cp->comment);
    }
    if(cp->matrice) {
      opj_free(cp->matrice);
    }
    for (tileno = 0; tileno < cp->tw * cp->th; tileno++) {
      opj_free(cp->tcps[tileno].tccps);
    }
    opj_free(cp->tcps);
    opj_free(cp);
  }

  opj_free(j2k);
}

void j2k_setup_encoder(opj_j2k_t *j2k, opj_cparameters_t *parameters, opj_image_t *image) {
  int i, j, tileno, numpocs_tile;
  opj_cp_t *cp = NULL;

  if(!j2k || !parameters || ! image) {
    return;
  }

  /* create and initialize the coding parameters structure */
  cp = (opj_cp_t*)opj_malloc(sizeof(opj_cp_t));

  /* keep a link to cp so that we can destroy it later in j2k_destroy_compress */
  j2k->cp = cp;

  /* set default values for cp */
  cp->tw = 1;
  cp->th = 1;

  /* 
  copy user encoding parameters 
  */

  cp->disto_alloc = parameters->cp_disto_alloc;
  cp->fixed_alloc = parameters->cp_fixed_alloc;
  cp->fixed_quality = parameters->cp_fixed_quality;

  /* mod fixed_quality */
  if(parameters->cp_matrice) {
    size_t array_size = parameters->tcp_numlayers * parameters->numresolution * 3 * sizeof(int);
    cp->matrice = (int *) opj_malloc(array_size);
    memcpy(cp->matrice, parameters->cp_matrice, array_size);
  }

  /* creation of an index file ? */
  cp->index_on = parameters->index_on;
  if(cp->index_on) {
    j2k->image_info = (opj_image_info_t*)opj_malloc(sizeof(opj_image_info_t));
  }

  /* tiles */
  cp->tdx = parameters->cp_tdx;
  cp->tdy = parameters->cp_tdy;

  /* tile offset */
  cp->tx0 = parameters->cp_tx0;
  cp->ty0 = parameters->cp_ty0;

  /* comment string */
  if(parameters->cp_comment) {
    cp->comment = (char*)opj_malloc(strlen(parameters->cp_comment) + 1);
    if(cp->comment) {
      strcpy(cp->comment, parameters->cp_comment);
    }
  }

  /*
  calculate other encoding parameters
  */

  if (parameters->tile_size_on) {
    cp->tw = int_ceildiv(image->x1 - cp->tx0, cp->tdx);
    cp->th = int_ceildiv(image->y1 - cp->ty0, cp->tdy);
  } else {
    cp->tdx = image->x1 - cp->tx0;
    cp->tdy = image->y1 - cp->ty0;
  }

  /* initialize the mutiple tiles */
  /* ---------------------------- */
  cp->tcps = (opj_tcp_t *) opj_malloc(cp->tw * cp->th * sizeof(opj_tcp_t));

  for (tileno = 0; tileno < cp->tw * cp->th; tileno++) {
    opj_tcp_t *tcp = &cp->tcps[tileno];
    tcp->numlayers = parameters->tcp_numlayers;
    for (j = 0; j < tcp->numlayers; j++) {
      if (cp->fixed_quality) {  /* add fixed_quality */
        tcp->distoratio[j] = parameters->tcp_distoratio[j];
      } else {
        tcp->rates[j] = parameters->tcp_rates[j];
      }
    }
    tcp->csty = parameters->csty;
    tcp->prg = parameters->prog_order;
    tcp->mct = image->numcomps == 3 ? 1 : 0;

    numpocs_tile = 0;
    tcp->POC = 0;
    if (parameters->numpocs) {
      /* initialisation of POC */
      tcp->POC = 1;
      for (i = 0; i < parameters->numpocs; i++) {
        if((tileno == parameters->POC[i].tile - 1) || (parameters->POC[i].tile == -1)) {
          opj_poc_t *tcp_poc = &tcp->pocs[numpocs_tile];
          tcp_poc->resno0    = parameters->POC[numpocs_tile].resno0;
          tcp_poc->compno0  = parameters->POC[numpocs_tile].compno0;
          tcp_poc->layno1    = parameters->POC[numpocs_tile].layno1;
          tcp_poc->resno1    = parameters->POC[numpocs_tile].resno1;
          tcp_poc->compno1  = parameters->POC[numpocs_tile].compno1;
          tcp_poc->prg    = parameters->POC[numpocs_tile].prg;
          tcp_poc->tile    = parameters->POC[numpocs_tile].tile;
          numpocs_tile++;
        }
      }
    }
    tcp->numpocs = numpocs_tile;

    tcp->tccps = (opj_tccp_t *) opj_malloc(image->numcomps * sizeof(opj_tccp_t));
    
    for (i = 0; i < image->numcomps; i++) {
      opj_tccp_t *tccp = &tcp->tccps[i];
      tccp->csty = parameters->csty & 0x01;  /* 0 => one precinct || 1 => custom precinct  */
      tccp->numresolutions = parameters->numresolution;
      tccp->cblkw = int_floorlog2(parameters->cblockw_init);
      tccp->cblkh = int_floorlog2(parameters->cblockh_init);
      tccp->cblksty = parameters->mode;
      tccp->qmfbid = parameters->irreversible ? 0 : 1;
      tccp->qntsty = parameters->irreversible ? J2K_CCP_QNTSTY_SEQNT : J2K_CCP_QNTSTY_NOQNT;
      tccp->numgbits = 2;
      if (i == parameters->roi_compno) {
        tccp->roishift = parameters->roi_shift;
      } else {
        tccp->roishift = 0;
      }
      if (parameters->csty & J2K_CCP_CSTY_PRT) {
        int p = 0;
        for (j = tccp->numresolutions - 1; j >= 0; j--) {
          if (p < parameters->res_spec) {
            if (parameters->prcw_init[p] < 1) {
              tccp->prcw[j] = 1;
            } else {
              tccp->prcw[j] = int_floorlog2(parameters->prcw_init[p]);
            }
            if (parameters->prch_init[p] < 1) {
              tccp->prch[j] = 1;
            } else {
              tccp->prch[j] = int_floorlog2(parameters->prch_init[p]);
            }
          } else {
            int res_spec = parameters->res_spec;
            int size_prcw = parameters->prcw_init[res_spec - 1] >> (p - (res_spec - 1));
            int size_prch = parameters->prch_init[res_spec - 1] >> (p - (res_spec - 1));
            if (size_prcw < 1) {
              tccp->prcw[j] = 1;
            } else {
              tccp->prcw[j] = int_floorlog2(size_prcw);
            }
            if (size_prch < 1) {
              tccp->prch[j] = 1;
            } else {
              tccp->prch[j] = int_floorlog2(size_prch);
            }
          }
          p++;
          /*printf("\nsize precinct for level %d : %d,%d\n", j,tccp->prcw[j], tccp->prch[j]); */
        }
      } else {
        for (j = 0; j < tccp->numresolutions; j++) {
          tccp->prcw[j] = 15;
          tccp->prch[j] = 15;
        }
      }

      dwt_calc_explicit_stepsizes(tccp, image->comps[i].prec);
    }
  }
}

/**
Create an index file
@param j2k
@param cio
@param image_info
@param index Index filename
@return Returns 1 if successful, returns 0 otherwise
*/
static int j2k_create_index(opj_j2k_t *j2k, opj_cio_t *cio, opj_image_info_t *image_info, char *index) {
  int tileno, compno, layno, resno, precno, pack_nb, x, y;
  FILE *stream = NULL;
  double total_disto = 0;

  image_info->codestream_size = cio_tell(cio) + j2k->pos_correction;  /* Correction 14/4/03 suite rmq de Patrick */

  stream = fopen(index, "w");
  if (!stream) {
    opj_event_msg(j2k->cinfo, EVT_ERROR, "failed to open %s for writing\n", index);
    return 0;
  }
  
  fprintf(stream, "%d %d\n", image_info->image_w, image_info->image_h);
  fprintf(stream, "%d\n", image_info->prog);
  fprintf(stream, "%d %d\n", image_info->tile_x, image_info->tile_y);
  fprintf(stream, "%d %d\n", image_info->tw, image_info->th);
  fprintf(stream, "%d\n", image_info->comp);
  fprintf(stream, "%d\n", image_info->layer);
  fprintf(stream, "%d\n", image_info->decomposition);
  
  for (resno = image_info->decomposition; resno >= 0; resno--) {
    fprintf(stream, "[%d,%d] ", 
      (1 << image_info->tile[0].pdx[resno]), (1 << image_info->tile[0].pdx[resno]));  /* based on tile 0 */
  }
  fprintf(stream, "\n");
  fprintf(stream, "%d\n", image_info->main_head_end);
  fprintf(stream, "%d\n", image_info->codestream_size);
  
  for (tileno = 0; tileno < image_info->tw * image_info->th; tileno++) {
    fprintf(stream, "%4d %9d %9d %9d %9e %9d %9e\n",
      image_info->tile[tileno].num_tile,
      image_info->tile[tileno].start_pos,
      image_info->tile[tileno].end_header,
      image_info->tile[tileno].end_pos,
      image_info->tile[tileno].distotile, image_info->tile[tileno].nbpix,
      image_info->tile[tileno].distotile / image_info->tile[tileno].nbpix);
  }
  
  for (tileno = 0; tileno < image_info->tw * image_info->th; tileno++) {
    int start_pos, end_pos;
    double disto = 0;
    pack_nb = 0;
    
    /*
    fprintf(stream, "pkno tileno layerno resno compno precno start_pos   end_pos       deltaSE        \n");
    */
    
    if (image_info->prog == LRCP) {  /* LRCP */
      /*
      fprintf(stream, "pack_nb tileno layno resno compno precno start_pos  end_pos   disto");
      */
      for (layno = 0; layno < image_info->layer; layno++) {
        for (resno = 0; resno < image_info->decomposition + 1; resno++) {
          for (compno = 0; compno < image_info->comp; compno++) {
            int prec_max = image_info->tile[tileno].pw[resno] * image_info->tile[tileno].ph[resno];
            for (precno = 0; precno < prec_max; precno++) {
              start_pos = image_info->tile[tileno].packet[pack_nb].start_pos;
              end_pos = image_info->tile[tileno].packet[pack_nb].end_pos;
              disto = image_info->tile[tileno].packet[pack_nb].disto;
              fprintf(stream, "%4d %6d %7d %5d %6d %6d %9d %9d %8e\n",
                pack_nb, tileno, layno, resno, compno, precno, start_pos, end_pos, disto);
              total_disto += disto;
              pack_nb++;
            }
          }
        }
      }
    } /* LRCP */
    else if (image_info->prog == RLCP) {  /* RLCP */
      /*
      fprintf(stream, "pack_nb tileno resno layno compno precno start_pos  end_pos   disto");
      */
      for (resno = 0; resno < image_info->decomposition + 1; resno++) {
        for (layno = 0; layno < image_info->layer; layno++) {
          for (compno = 0; compno < image_info->comp; compno++) {
            int prec_max = image_info->tile[tileno].pw[resno] * image_info->tile[tileno].ph[resno];
            for (precno = 0; precno < prec_max; precno++) {
              start_pos = image_info->tile[tileno].packet[pack_nb].start_pos;
              end_pos = image_info->tile[tileno].packet[pack_nb].end_pos;
              disto = image_info->tile[tileno].packet[pack_nb].disto;
              fprintf(stream, "%4d %6d %5d %7d %6d %6d %9d %9d %8e\n",
                pack_nb, tileno, resno, layno, compno, precno, start_pos, end_pos, disto);
              total_disto += disto;
              pack_nb++;
            }
          }
        }
      }
    } /* RLCP */
    else if (image_info->prog == RPCL) {  /* RPCL */
      /*
      fprintf(stream, "\npack_nb tileno resno precno compno layno start_pos  end_pos   disto\n"); 
      */
      for (resno = 0; resno < image_info->decomposition + 1; resno++) {
        /* I suppose components have same XRsiz, YRsiz */
        int x0 = image_info->tile_Ox + tileno - (int)floor( (float)tileno/(float)image_info->tw ) * image_info->tw * image_info->tile_x;
        int y0 = image_info->tile_Ox + (int)floor( (float)tileno/(float)image_info->tw ) * image_info->tile_y;
        int x1 = x0 + image_info->tile_x;
        int y1 = y0 + image_info->tile_y;
        for(y = y0; y < y1; y++) {
          for(x = x0; x < x1; x++) {
            for (compno = 0; compno < image_info->comp; compno++) {
              int prec_max = image_info->tile[tileno].pw[resno] * image_info->tile[tileno].ph[resno];
              for (precno = 0; precno < prec_max; precno++) {
                int pcnx = image_info->tile[tileno].pw[resno];
                int pcx = (int) pow( 2, image_info->tile[tileno].pdx[resno] + image_info->decomposition - resno );
                int pcy = (int) pow( 2, image_info->tile[tileno].pdy[resno] + image_info->decomposition - resno );
                int precno_x = precno - (int) floor( (float)precno/(float)pcnx ) * pcnx;
                int precno_y = (int) floor( (float)precno/(float)pcnx );
                if (precno_y*pcy == y ) {
                  if (precno_x*pcx == x ) {
                    for (layno = 0; layno < image_info->layer; layno++) {
                      start_pos = image_info->tile[tileno].packet[pack_nb].start_pos;
                      end_pos = image_info->tile[tileno].packet[pack_nb].end_pos;
                      disto = image_info->tile[tileno].packet[pack_nb].disto;
                      fprintf(stream, "%4d %6d %5d %6d %6d %7d %9d %9d %8e\n",
                        pack_nb, tileno, resno, precno, compno, layno, start_pos, end_pos, disto); 
                      total_disto += disto;
                      pack_nb++; 
                    }
                  }
                }
              } /* precno */
            } /* compno */
          } /* x = x0..x1 */
        } /* y = y0..y1 */
      } /* resno */
    } /* RPCL */
    else if (image_info->prog == PCRL) {  /* PCRL */
      /* I suppose components have same XRsiz, YRsiz */
      int x0 = image_info->tile_Ox + tileno - (int)floor( (float)tileno/(float)image_info->tw ) * image_info->tw * image_info->tile_x;
      int y0 = image_info->tile_Ox + (int)floor( (float)tileno/(float)image_info->tw ) * image_info->tile_y;
      int x1 = x0 + image_info->tile_x;
      int y1 = y0 + image_info->tile_y;
      /*
      fprintf(stream, "\npack_nb tileno precno compno resno layno start_pos  end_pos   disto\n"); 
      */
      for(y = y0; y < y1; y++) {
        for(x = x0; x < x1; x++) {
          for (compno = 0; compno < image_info->comp; compno++) {
            for (resno = 0; resno < image_info->decomposition + 1; resno++) {
              int prec_max = image_info->tile[tileno].pw[resno] * image_info->tile[tileno].ph[resno];
              for (precno = 0; precno < prec_max; precno++) {
                int pcnx = image_info->tile[tileno].pw[resno];
                int pcx = (int) pow( 2, image_info->tile[tileno].pdx[resno] + image_info->decomposition - resno );
                int pcy = (int) pow( 2, image_info->tile[tileno].pdy[resno] + image_info->decomposition - resno );
                int precno_x = precno - (int) floor( (float)precno/(float)pcnx ) * pcnx;
                int precno_y = (int) floor( (float)precno/(float)pcnx );
                if (precno_y*pcy == y ) {
                  if (precno_x*pcx == x ) {
                    for (layno = 0; layno < image_info->layer; layno++) {
                      start_pos = image_info->tile[tileno].packet[pack_nb].start_pos;
                      end_pos = image_info->tile[tileno].packet[pack_nb].end_pos;
                      disto = image_info->tile[tileno].packet[pack_nb].disto;
                      fprintf(stream, "%4d %6d %6d %6d %5d %7d %9d %9d %8e\n",
                        pack_nb, tileno, precno, compno, resno, layno, start_pos, end_pos, disto); 
                      total_disto += disto;
                      pack_nb++; 
                    }
                  }
                }
              } /* precno */
            } /* resno */
          } /* compno */
        } /* x = x0..x1 */
      } /* y = y0..y1 */
    } /* PCRL */
    else {  /* CPRL */
      /*
      fprintf(stream, "\npack_nb tileno compno precno resno layno start_pos  end_pos   disto\n"); 
      */
      for (compno = 0; compno < image_info->comp; compno++) {
        /* I suppose components have same XRsiz, YRsiz */
        int x0 = image_info->tile_Ox + tileno - (int)floor( (float)tileno/(float)image_info->tw ) * image_info->tw * image_info->tile_x;
        int y0 = image_info->tile_Ox + (int)floor( (float)tileno/(float)image_info->tw ) * image_info->tile_y;
        int x1 = x0 + image_info->tile_x;
        int y1 = y0 + image_info->tile_y;
        for(y = y0; y < y1; y++) {
          for(x = x0; x < x1; x++) {
            for (resno = 0; resno < image_info->decomposition + 1; resno++) {
              int prec_max = image_info->tile[tileno].pw[resno] * image_info->tile[tileno].ph[resno];
              for (precno = 0; precno < prec_max; precno++) {
                int pcnx = image_info->tile[tileno].pw[resno];
                int pcx = (int) pow( 2, image_info->tile[tileno].pdx[resno] + image_info->decomposition - resno );
                int pcy = (int) pow( 2, image_info->tile[tileno].pdy[resno] + image_info->decomposition - resno );
                int precno_x = precno - (int) floor( (float)precno/(float)pcnx ) * pcnx;
                int precno_y = (int) floor( (float)precno/(float)pcnx );
                if (precno_y*pcy == y ) {
                  if (precno_x*pcx == x ) {
                    for (layno = 0; layno < image_info->layer; layno++) {
                      start_pos = image_info->tile[tileno].packet[pack_nb].start_pos;
                      end_pos = image_info->tile[tileno].packet[pack_nb].end_pos;
                      disto = image_info->tile[tileno].packet[pack_nb].disto;
                      fprintf(stream, "%4d %6d %6d %6d %5d %7d %9d %9d %8e\n",
                        pack_nb, tileno, compno, precno, resno, layno, start_pos, end_pos, disto); 
                      total_disto += disto;
                      pack_nb++; 
                    }
                  }
                }
              } /* precno */
            } /* resno */
          } /* x = x0..x1 */
        } /* y = y0..y1 */
      } /* comno */
    } /* CPRL */   
  } /* tileno */
  
  fprintf(stream, "%8e\n", image_info->D_max); /* SE max */
  fprintf(stream, "%.8e\n", total_disto);  /* SE totale */
  fclose(stream);

  return 1;
}

bool j2k_encode(opj_j2k_t *j2k, opj_cio_t *cio, opj_image_t *image, char *index) {
  int tileno, compno;
  opj_image_info_t *image_info = NULL;
  opj_cp_t *cp = NULL;

  opj_tcd_t *tcd = NULL;  /* TCD component */

  j2k->cio = cio;  
  j2k->image = image;

  cp = j2k->cp;

  /* j2k_dump_cp(stdout, image, cp); */

  /* INDEX >> */
  image_info = j2k->image_info;
  if (image_info && cp->index_on) {
    image_info->index_on = cp->index_on;
    image_info->tile = (opj_tile_info_t *) opj_malloc(cp->tw * cp->th * sizeof(opj_tile_info_t));
    image_info->image_w = image->x1 - image->x0;
    image_info->image_h = image->y1 - image->y0;
    image_info->prog = (&cp->tcps[0])->prg;
    image_info->tw = cp->tw;
    image_info->th = cp->th;
    image_info->tile_x = cp->tdx;  /* new version parser */
    image_info->tile_y = cp->tdy;  /* new version parser */
    image_info->tile_Ox = cp->tx0;  /* new version parser */
    image_info->tile_Oy = cp->ty0;  /* new version parser */
    image_info->comp = image->numcomps;
    image_info->layer = (&cp->tcps[0])->numlayers;
    image_info->decomposition = (&cp->tcps[0])->tccps->numresolutions - 1;
    image_info->D_max = 0;    /* ADD Marcela */
  }
  /* << INDEX */
  
  j2k_write_soc(j2k);
  j2k_write_siz(j2k);
  j2k_write_cod(j2k);
  j2k_write_qcd(j2k);
  for (compno = 0; compno < image->numcomps; compno++) {
    opj_tcp_t *tcp = &cp->tcps[0];
    if (tcp->tccps[compno].roishift)
      j2k_write_rgn(j2k, compno, 0);
  }
  if (cp->comment != NULL) {
    j2k_write_com(j2k);
  }
  /* INDEX >> */
  if(image_info && image_info->index_on) {
    image_info->main_head_end = cio_tell(cio) - 1;
  }
  /* << INDEX */

  /* create the tile encoder */
  tcd = tcd_create(j2k->cinfo);

  /* encode each tile */

  for (tileno = 0; tileno < cp->tw * cp->th; tileno++) {
    opj_event_msg(j2k->cinfo, EVT_INFO, "tile number %d / %d\n", tileno + 1, cp->tw * cp->th);
    
    j2k->curtileno = tileno;

    /* initialisation before tile encoding  */
    if (tileno == 0) {
      tcd_malloc_encode(tcd, image, cp, j2k->curtileno);
    } else {
      tcd_init_encode(tcd, image, cp, j2k->curtileno);
    }
    
    /* INDEX >> */
    if(image_info && image_info->index_on) {
      image_info->tile[j2k->curtileno].num_tile = j2k->curtileno;
      image_info->tile[j2k->curtileno].start_pos = cio_tell(cio) + j2k->pos_correction;
    }
    /* << INDEX */

    j2k_write_sot(j2k);
    
    for (compno = 1; compno < image->numcomps; compno++) {
      j2k_write_coc(j2k, compno);
      j2k_write_qcc(j2k, compno);
    }
    if (cp->tcps[tileno].numpocs) {
      j2k_write_poc(j2k);
    }

    j2k_write_sod(j2k, tcd);
    
    /* INDEX >> */
    if(image_info && image_info->index_on) {
      image_info->tile[j2k->curtileno].end_pos = cio_tell(cio) + j2k->pos_correction - 1;
    }
    /* << INDEX */
    
    
    /*
    if (tile->PPT) { // BAD PPT !!! 
      FILE *PPT_file;
      int i;
      PPT_file=fopen("PPT","rb");
      fprintf(stderr,"%c%c%c%c",255,97,tile->len_ppt/256,tile->len_ppt%256);
      for (i=0;i<tile->len_ppt;i++) {
        unsigned char elmt;
        fread(&elmt, 1, 1, PPT_file);
        fwrite(&elmt,1,1,f);
      }
      fclose(PPT_file);
      unlink("PPT");
    }
    */
    
  }
  
  /* destroy the tile encoder */
  tcd_free_encode(tcd);
  tcd_destroy(tcd);

  j2k_write_eoc(j2k);
  
  /* Creation of the index file */
  if(image_info && image_info->index_on) {
    if(!j2k_create_index(j2k, cio, image_info, index)) {
      opj_event_msg(j2k->cinfo, EVT_ERROR, "failed to create index file %s\n", index);
      return false;
    }
  }
    
  return true;
}

