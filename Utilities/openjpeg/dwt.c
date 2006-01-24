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

/*
 *  NOTE:
 *  This is a modified version of the openjpeg dwt.c file.
 *  Average speed improvement compared to the original file (measured on
 *  my own machine, a P4 running at 3.0 GHz):
 *  5x3 wavelets about 2 times faster
 *  9x7 wavelets about 3 times faster
 *  for both, encoding and decoding.
 *
 *  The better performance is caused by doing the 1-dimensional DWT
 *  within a temporary buffer where the data can be accessed sequential
 *  for both directions, horizontal and vertical. The 2d vertical DWT was
 *  the major bottleneck in the former version.
 *
 *  I have also removed the "Add Patrick" part because it is not longer
 *  needed.  
 *
 *  6/6/2005
 *  -Ive (aka Reiner Wahler)
 *  mail: ive@lilysoft.com
 */

#include "opj_includes.h"

/** @defgroup DWT DWT - Implementation of a discrete wavelet transform */
/*@{*/

/** @name Local static functions */
/*@{*/

/**
Forward lazy transform (horizontal)
*/
static void dwt_deinterleave_h(int *a, int *b, int dn, int sn, int cas);
/**
Forward lazy transform (vertical)
*/
static void dwt_deinterleave_v(int *a, int *b, int dn, int sn, int x, int cas);
/**
Inverse lazy transform (horizontal)
*/
static void dwt_interleave_h(int *a, int *b, int dn, int sn, int cas);
/**
Inverse lazy transform (vertical)
*/
static void dwt_interleave_v(int *a, int *b, int dn, int sn, int x, int cas);
/**
Forward 5-3 wavelet tranform in 1-D
*/
static void dwt_encode_1(int *a, int dn, int sn, int cas);
/**
Inverse 5-3 wavelet tranform in 1-D
*/
static void dwt_decode_1(int *a, int dn, int sn, int cas);
/**
Forward 9-7 wavelet transform in 1-D
*/
static void dwt_encode_1_real(int *a, int dn, int sn, int cas);
/**
Inverse 9-7 wavelet transform in 1-D
*/
static void dwt_decode_1_real(int *a, int dn, int sn, int cas);
/**
FIXME : comment ???
*/
static void dwt_encode_stepsize(int stepsize, int numbps, opj_stepsize_t *bandno_stepsize);

/*@}*/

/*@}*/

#define S(i) a[(i)*2]
#define D(i) a[(1+(i)*2)]
#define S_(i) ((i)<0?S(0):((i)>=sn?S(sn-1):S(i)))
#define D_(i) ((i)<0?D(0):((i)>=dn?D(dn-1):D(i)))
/* new */
#define SS_(i) ((i)<0?S(0):((i)>=dn?S(dn-1):S(i)))
#define DD_(i) ((i)<0?D(0):((i)>=sn?D(sn-1):D(i)))

/* <summary>                                                              */
/* This table contains the norms of the 5-3 wavelets for different bands. */
/* </summary>                                                             */
static const double dwt_norms[4][10] = {
  {1.000, 1.500, 2.750, 5.375, 10.68, 21.34, 42.67, 85.33, 170.7, 341.3},
  {1.038, 1.592, 2.919, 5.703, 11.33, 22.64, 45.25, 90.48, 180.9},
  {1.038, 1.592, 2.919, 5.703, 11.33, 22.64, 45.25, 90.48, 180.9},
  {.7186, .9218, 1.586, 3.043, 6.019, 12.01, 24.00, 47.97, 95.93}
};

/* <summary>                                                              */
/* This table contains the norms of the 9-7 wavelets for different bands. */
/* </summary>                                                             */
static const double dwt_norms_real[4][10] = {
  {1.000, 1.965, 4.177, 8.403, 16.90, 33.84, 67.69, 135.3, 270.6, 540.9},
  {2.022, 3.989, 8.355, 17.04, 34.27, 68.63, 137.3, 274.6, 549.0},
  {2.022, 3.989, 8.355, 17.04, 34.27, 68.63, 137.3, 274.6, 549.0},
  {2.080, 3.865, 8.307, 17.18, 34.71, 69.59, 139.3, 278.6, 557.2}
};

/* 
==========================================================
   local functions
==========================================================
*/

/* <summary>                       */
/* Forward lazy transform (horizontal).  */
/* </summary>                            */ 
static void dwt_deinterleave_h(int *a, int *b, int dn, int sn, int cas) {
  int i;
    for (i=0; i<sn; i++) b[i]=a[2*i+cas];
    for (i=0; i<dn; i++) b[sn+i]=a[(2*i+1-cas)];
}

/* <summary>                             */  
/* Forward lazy transform (vertical).    */
/* </summary>                            */ 
static void dwt_deinterleave_v(int *a, int *b, int dn, int sn, int x, int cas) {
    int i;
    for (i=0; i<sn; i++) b[i*x]=a[2*i+cas];
    for (i=0; i<dn; i++) b[(sn+i)*x]=a[(2*i+1-cas)];
}

/* <summary>                             */
/* Inverse lazy transform (horizontal).  */
/* </summary>                            */
static void dwt_interleave_h(int *a, int *b, int dn, int sn, int cas) {
    int i;
    int *ai = NULL;
    int *bi = NULL;
    ai = a;
    bi = b + cas;
    for (i = 0; i < sn; i++) {
      *bi = *ai;  
    bi += 2;  
    ai++;
    }
    ai = a + sn;
    bi = b + 1 - cas;
    for (i = 0; i < dn; i++) {
      *bi = *ai;
    bi += 2;
    ai++;
    }
}

/* <summary>                             */  
/* Inverse lazy transform (vertical).    */
/* </summary>                            */ 
static void dwt_interleave_v(int *a, int *b, int dn, int sn, int x, int cas) {
    int i;
    int *ai = NULL;
    int *bi = NULL;
    ai = a;
    bi = b + cas;
    for (i = 0; i < sn; i++) {
      *bi = *ai;
    bi += 2;
    ai += x;
    }
    ai = a + (sn * x);
    bi = b + 1 - cas;
    for (i = 0; i < dn; i++) {
      *bi = *ai;
    bi += 2;  
    ai += x;
    }
}


/* <summary>                            */
/* Forward 5-3 wavelet tranform in 1-D. */
/* </summary>                           */
static void dwt_encode_1(int *a, int dn, int sn, int cas) {
  int i;
  
  if (!cas) {
    if ((dn > 0) || (sn > 1)) {  /* NEW :  CASE ONE ELEMENT */
      for (i = 0; i < dn; i++) D(i) -= (S_(i) + S_(i + 1)) >> 1;
      for (i = 0; i < sn; i++) S(i) += (D_(i - 1) + D_(i) + 2) >> 2;
    }
  } else {
    if (!sn && dn == 1)        /* NEW :  CASE ONE ELEMENT */
      S(0) *= 2;
    else {
      for (i = 0; i < dn; i++) S(i) -= (DD_(i) + DD_(i - 1)) >> 1;
      for (i = 0; i < sn; i++) D(i) += (SS_(i) + SS_(i + 1) + 2) >> 2;
    }
  }
}

/* <summary>                            */
/* Inverse 5-3 wavelet tranform in 1-D. */
/* </summary>                           */ 
static void dwt_decode_1(int *a, int dn, int sn, int cas) {
  int i;
  
  if (!cas) {
    if ((dn > 0) || (sn > 1)) { /* NEW :  CASE ONE ELEMENT */
      for (i = 0; i < sn; i++) S(i) -= (D_(i - 1) + D_(i) + 2) >> 2;
      for (i = 0; i < dn; i++) D(i) += (S_(i) + S_(i + 1)) >> 1;
    }
  } else {
    if (!sn  && dn == 1)          /* NEW :  CASE ONE ELEMENT */
      S(0) /= 2;
    else {
      for (i = 0; i < sn; i++) D(i) -= (SS_(i) + SS_(i + 1) + 2) >> 2;
      for (i = 0; i < dn; i++) S(i) += (DD_(i) + DD_(i - 1)) >> 1;
    }
  }
}

/* <summary>                             */
/* Forward 9-7 wavelet transform in 1-D. */
/* </summary>                            */
static void dwt_encode_1_real(int *a, int dn, int sn, int cas) {
  int i;
  if (!cas) {
    if ((dn > 0) || (sn > 1)) {  /* NEW :  CASE ONE ELEMENT */
      for (i = 0; i < dn; i++)
        D(i) -= fix_mul(S_(i) + S_(i + 1), 12993);
      for (i = 0; i < sn; i++)
        S(i) -= fix_mul(D_(i - 1) + D_(i), 434);
      for (i = 0; i < dn; i++)
        D(i) += fix_mul(S_(i) + S_(i + 1), 7233);
      for (i = 0; i < sn; i++)
        S(i) += fix_mul(D_(i - 1) + D_(i), 3633);
      for (i = 0; i < dn; i++)
        D(i) = fix_mul(D(i), 5038);  /*5038 */
      for (i = 0; i < sn; i++)
        S(i) = fix_mul(S(i), 6659);  /*6660 */
    }
  } else {
    if ((sn > 0) || (dn > 1)) {  /* NEW :  CASE ONE ELEMENT */
      for (i = 0; i < dn; i++)
        S(i) -= fix_mul(DD_(i) + DD_(i - 1), 12993);
      for (i = 0; i < sn; i++)
        D(i) -= fix_mul(SS_(i) + SS_(i + 1), 434);
      for (i = 0; i < dn; i++)
        S(i) += fix_mul(DD_(i) + DD_(i - 1), 7233);
      for (i = 0; i < sn; i++)
        D(i) += fix_mul(SS_(i) + SS_(i + 1), 3633);
      for (i = 0; i < dn; i++)
        S(i) = fix_mul(S(i), 5038);  /*5038 */
      for (i = 0; i < sn; i++)
        D(i) = fix_mul(D(i), 6659);  /*6660 */
    }
  }
}

/* <summary>                             */
/* Inverse 9-7 wavelet transform in 1-D. */
/* </summary>                            */
static void dwt_decode_1_real(int *a, int dn, int sn, int cas) {
  int i;
  if (!cas) {
    if ((dn > 0) || (sn > 1)) {  /* NEW :  CASE ONE ELEMENT */
      for (i = 0; i < sn; i++)
        S(i) = fix_mul(S(i), 10078);  /* 10076 */
      for (i = 0; i < dn; i++)
        D(i) = fix_mul(D(i), 13318);  /* 13320 */
      for (i = 0; i < sn; i++)
        S(i) -= fix_mul(D_(i - 1) + D_(i), 3633);
      for (i = 0; i < dn; i++)
        D(i) -= fix_mul(S_(i) + S_(i + 1), 7233);
      for (i = 0; i < sn; i++)
        S(i) += fix_mul(D_(i - 1) + D_(i), 434);
      for (i = 0; i < dn; i++)
        D(i) += fix_mul(S_(i) + S_(i + 1), 12994);  /* 12993 */
    }
  } else {
    if ((sn > 0) || (dn > 1)) {  /* NEW :  CASE ONE ELEMENT */
      for (i = 0; i < sn; i++)
        D(i) = fix_mul(D(i), 10078);  /* 10076 */
      for (i = 0; i < dn; i++)
        S(i) = fix_mul(S(i), 13318);  /* 13320 */
      for (i = 0; i < sn; i++)
        D(i) -= fix_mul(SS_(i) + SS_(i + 1), 3633);
      for (i = 0; i < dn; i++)
        S(i) -= fix_mul(DD_(i) + DD_(i - 1), 7233);
      for (i = 0; i < sn; i++)
        D(i) += fix_mul(SS_(i) + SS_(i + 1), 434);
      for (i = 0; i < dn; i++)
        S(i) += fix_mul(DD_(i) + DD_(i - 1), 12994);  /* 12993 */
    }
  }
}

static void dwt_encode_stepsize(int stepsize, int numbps, opj_stepsize_t *bandno_stepsize) {
  int p, n;
  p = int_floorlog2(stepsize) - 13;
  n = 11 - int_floorlog2(stepsize);
  bandno_stepsize->mant = (n < 0 ? stepsize >> -n : stepsize << n) & 0x7ff;
  bandno_stepsize->expn = numbps - p;
}

/* 
==========================================================
   DWT interface
==========================================================
*/

/* <summary>                            */
/* Forward 5-3 wavelet tranform in 2-D. */
/* </summary>                           */
void dwt_encode(opj_tcd_tilecomp_t * tilec) {
  int i, j, k;
  int *a = NULL;
  int *aj = NULL;
  int *bj = NULL;
  int w, l;
  
  w = tilec->x1-tilec->x0;
  l = tilec->numresolutions-1;
  a = tilec->data;
  
  for (i = 0; i < l; i++) {
    int rw;      /* width of the resolution level computed                                                           */
    int rh;      /* heigth of the resolution level computed                                                          */
    int rw1;    /* width of the resolution level once lower than computed one                                       */
    int rh1;    /* height of the resolution level once lower than computed one                                      */
    int cas_col;  /* 0 = non inversion on horizontal filtering 1 = inversion between low-pass and high-pass filtering */
    int cas_row;  /* 0 = non inversion on vertical filtering 1 = inversion between low-pass and high-pass filtering   */
    int dn, sn;
    
    rw = tilec->resolutions[l - i].x1 - tilec->resolutions[l - i].x0;
    rh = tilec->resolutions[l - i].y1 - tilec->resolutions[l - i].y0;
    rw1= tilec->resolutions[l - i - 1].x1 - tilec->resolutions[l - i - 1].x0;
    rh1= tilec->resolutions[l - i - 1].y1 - tilec->resolutions[l - i - 1].y0;
    
    cas_row = tilec->resolutions[l - i].x0 % 2;
    cas_col = tilec->resolutions[l - i].y0 % 2;
        
    sn = rh1;
    dn = rh - rh1;
    bj = (int*)opj_malloc(rh * sizeof(int));
    for (j = 0; j < rw; j++) {
      aj = a + j;
      for (k = 0; k < rh; k++)  bj[k] = aj[k*w];
      dwt_encode_1(bj, dn, sn, cas_col);
      dwt_deinterleave_v(bj, aj, dn, sn, w, cas_col);
    }
    opj_free(bj);
    
    sn = rw1;
    dn = rw - rw1;
    bj = (int*)opj_malloc(rw * sizeof(int));
    for (j = 0; j < rh; j++) {
      aj = a + j * w;
      for (k = 0; k < rw; k++)  bj[k] = aj[k];
      dwt_encode_1(bj, dn, sn, cas_row);
      dwt_deinterleave_h(bj, aj, dn, sn, cas_row);
    }
    opj_free(bj);
  }
}


/* <summary>                            */
/* Inverse 5-3 wavelet tranform in 2-D. */
/* </summary>                           */
void dwt_decode(opj_tcd_tilecomp_t * tilec, int stop) {
  int i, j, k;
  int *a = NULL;
  int *aj = NULL;
  int *bj = NULL;
  int w, l;
  
  w = tilec->x1-tilec->x0;
  l = tilec->numresolutions-1;
  a = tilec->data;
  
  for (i = l - 1; i >= stop; i--) {
    int rw;      /* width of the resolution level computed                                                           */
    int rh;      /* heigth of the resolution level computed                                                          */
    int rw1;    /* width of the resolution level once lower than computed one                                       */
    int rh1;    /* height of the resolution level once lower than computed one                                      */
    int cas_col;  /* 0 = non inversion on horizontal filtering 1 = inversion between low-pass and high-pass filtering */
    int cas_row;  /* 0 = non inversion on vertical filtering 1 = inversion between low-pass and high-pass filtering   */
    int dn, sn;
    
    rw = tilec->resolutions[l - i].x1 - tilec->resolutions[l - i].x0;
    rh = tilec->resolutions[l - i].y1 - tilec->resolutions[l - i].y0;
    rw1= tilec->resolutions[l - i - 1].x1 - tilec->resolutions[l - i - 1].x0;
    rh1= tilec->resolutions[l - i - 1].y1 - tilec->resolutions[l - i - 1].y0;
    
    cas_row = tilec->resolutions[l - i].x0 % 2;
    cas_col = tilec->resolutions[l - i].y0 % 2;
    
    sn = rw1;
    dn = rw - rw1;
    bj = (int*)opj_malloc(rw * sizeof(int));
    for (j = 0; j < rh; j++) {
      aj = a + j*w;
      dwt_interleave_h(aj, bj, dn, sn, cas_row);
      dwt_decode_1(bj, dn, sn, cas_row);
      for (k = 0; k < rw; k++)  aj[k] = bj[k];
    }
    opj_free(bj);
    
    sn = rh1;
    dn = rh - rh1;
    bj = (int*)opj_malloc(rh * sizeof(int));
    for (j = 0; j < rw; j++) {
      aj = a + j;
      dwt_interleave_v(aj, bj, dn, sn, w, cas_col);
      dwt_decode_1(bj, dn, sn, cas_col);
      for (k = 0; k < rh; k++)  aj[k * w] = bj[k];
    }
    opj_free(bj);
  }
}


/* <summary>                          */
/* Get gain of 5-3 wavelet transform. */
/* </summary>                         */
int dwt_getgain(int orient) {
  if (orient == 0)
    return 0;
  if (orient == 1 || orient == 2)
    return 1;
  return 2;
}

/* <summary>                */
/* Get norm of 5-3 wavelet. */
/* </summary>               */
double dwt_getnorm(int level, int orient) {
  return dwt_norms[orient][level];
}

/* <summary>                             */
/* Forward 9-7 wavelet transform in 2-D. */
/* </summary>                            */

void dwt_encode_real(opj_tcd_tilecomp_t * tilec) {
  int i, j, k;
  int *a = NULL;
  int *aj = NULL;
  int *bj = NULL;
  int w, l;
  
  w = tilec->x1-tilec->x0;
  l = tilec->numresolutions-1;
  a = tilec->data;
  
  for (i = 0; i < l; i++) {
    int rw;      /* width of the resolution level computed                                                     */
    int rh;      /* heigth of the resolution level computed                                                    */
    int rw1;    /* width of the resolution level once lower than computed one                                 */
    int rh1;    /* height of the resolution level once lower than computed one                                */
    int cas_col;  /* 0 = non inversion on horizontal filtering 1 = inversion between low-pass and high-pass filtering */
    int cas_row;  /* 0 = non inversion on vertical filtering 1 = inversion between low-pass and high-pass filtering   */
    int dn, sn;
    
    rw = tilec->resolutions[l - i].x1 - tilec->resolutions[l - i].x0;
    rh = tilec->resolutions[l - i].y1 - tilec->resolutions[l - i].y0;
    rw1= tilec->resolutions[l - i - 1].x1 - tilec->resolutions[l - i - 1].x0;
    rh1= tilec->resolutions[l - i - 1].y1 - tilec->resolutions[l - i - 1].y0;
    
    cas_row = tilec->resolutions[l - i].x0 % 2;
    cas_col = tilec->resolutions[l - i].y0 % 2;
    
    sn = rh1;
    dn = rh - rh1;
    bj = (int*)opj_malloc(rh * sizeof(int));
    for (j = 0; j < rw; j++) {
      aj = a + j;
      for (k = 0; k < rh; k++)  bj[k] = aj[k*w];
      dwt_encode_1_real(bj, dn, sn, cas_col);
      dwt_deinterleave_v(bj, aj, dn, sn, w, cas_col);
    }
    opj_free(bj);
    
    sn = rw1;
    dn = rw - rw1;
    bj = (int*)opj_malloc(rw * sizeof(int));
    for (j = 0; j < rh; j++) {
      aj = a + j * w;
      for (k = 0; k < rw; k++)  bj[k] = aj[k];
      dwt_encode_1_real(bj, dn, sn, cas_row);
      dwt_deinterleave_h(bj, aj, dn, sn, cas_row);
    }
    opj_free(bj);
  }
}


/* <summary>                             */
/* Inverse 9-7 wavelet transform in 2-D. */
/* </summary>                            */
void dwt_decode_real(opj_tcd_tilecomp_t * tilec, int stop) {
  int i, j, k;
  int *a = NULL;
  int *aj = NULL;
  int *bj = NULL;
  int w, l;
  
  w = tilec->x1-tilec->x0;
  l = tilec->numresolutions-1;
  a = tilec->data;
  
  for (i = l-1; i >= stop; i--) {
    int rw;      /* width of the resolution level computed                       */
    int rh;      /* heigth of the resolution level computed                      */
    int rw1;    /* width of the resolution level once lower than computed one   */
    int rh1;    /* height of the resolution level once lower than computed one  */
    int cas_col;  /* 0 = non inversion on horizontal filtering 1 = inversion between low-pass and high-pass filtering */
    int cas_row;  /* 0 = non inversion on vertical filtering 1 = inversion between low-pass and high-pass filtering   */
    int dn, sn;
    
    rw = tilec->resolutions[l - i].x1 - tilec->resolutions[l - i].x0;
    rh = tilec->resolutions[l - i].y1 - tilec->resolutions[l - i].y0;
    rw1= tilec->resolutions[l - i - 1].x1 - tilec->resolutions[l - i - 1].x0;
    rh1= tilec->resolutions[l - i - 1].y1 - tilec->resolutions[l - i - 1].y0;
    
    cas_col = tilec->resolutions[l - i].x0 % 2; /* 0 = non inversion on horizontal filtering 1 = inversion between low-pass and high-pass filtering */
    cas_row = tilec->resolutions[l - i].y0 % 2; /* 0 = non inversion on vertical filtering 1 = inversion between low-pass and high-pass filtering   */
        
    sn = rw1;
    dn = rw-rw1;
    bj = (int*)opj_malloc(rw * sizeof(int));
    for (j = 0; j < rh; j++) {
      aj = a + j * w;
      dwt_interleave_h(aj, bj, dn, sn, cas_col);
      dwt_decode_1_real(bj, dn, sn, cas_col);
      for (k = 0; k < rw; k++)  aj[k] = bj[k];
    }
    opj_free(bj);
    
    sn = rh1;
    dn = rh-rh1;
    bj = (int*)opj_malloc(rh * sizeof(int));
    for (j = 0; j < rw; j++) {
      aj = a + j;
      dwt_interleave_v(aj, bj, dn, sn, w, cas_row);
      dwt_decode_1_real(bj, dn, sn, cas_row);
      for (k = 0; k < rh; k++)  aj[k * w] = bj[k];
    }
    opj_free(bj);
  }
}


/* <summary>                          */
/* Get gain of 9-7 wavelet transform. */
/* </summary>                         */
int dwt_getgain_real(int orient) {
  (void)orient;
  return 0;
}

/* <summary>                */
/* Get norm of 9-7 wavelet. */
/* </summary>               */
double dwt_getnorm_real(int level, int orient) {
  return dwt_norms_real[orient][level];
}

void dwt_calc_explicit_stepsizes(opj_tccp_t * tccp, int prec) {
  int numbands, bandno;
  numbands = 3 * tccp->numresolutions - 2;
  for (bandno = 0; bandno < numbands; bandno++) {
    double stepsize;
    int resno, level, orient, gain;

    resno = (bandno == 0) ? 0 : ((bandno - 1) / 3 + 1);
    orient = (bandno == 0) ? 0 : ((bandno - 1) % 3 + 1);
    level = tccp->numresolutions - 1 - resno;
    gain = (tccp->qmfbid == 0) ? 0 : ((orient == 0) ? 0 : (((orient == 1) || (orient == 2)) ? 1 : 2));
    if (tccp->qntsty == J2K_CCP_QNTSTY_NOQNT) {
      stepsize = 1.0;
    } else {
      double norm = dwt_norms_real[orient][level];
      stepsize = (1 << (gain + 1)) / norm;
    }
    dwt_encode_stepsize((int) floor(stepsize * 8192.0), prec + gain, &tccp->stepsizes[bandno]);
  }
}
