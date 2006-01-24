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

int int_min(int a, int b) {
  return a < b ? a : b;
}

int int_max(int a, int b) {
  return (a > b) ? a : b;
}

int int_clamp(int a, int min, int max) {
  if (a < min)
    return min;
  if (a > max)
    return max;
  return a;
}

int int_abs(int a) {
  return a < 0 ? -a : a;
}

int int_ceildiv(int a, int b) {
  return (a + b - 1) / b;
}

int int_ceildivpow2(int a, int b) {
  return (a + (1 << b) - 1) >> b;
}

int int_floordivpow2(int a, int b) {
  return a >> b;
}

int int_floorlog2(int a) {
  int l;
  for (l = 0; a > 1; l++) {
    a >>= 1;
  }
  return l;
}

