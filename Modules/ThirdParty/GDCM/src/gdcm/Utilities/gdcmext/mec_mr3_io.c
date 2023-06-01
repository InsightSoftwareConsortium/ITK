/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#include "mec_mr3_io.h"

#include "mec_mr3_dict.h"

#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef _MSC_VER
#include <iconv.h>
#endif
#if defined(_MSC_VER) && (_MSC_VER < 1900)
#define snprintf _snprintf
#endif

struct stream {
  const void *start;
  const void *end;
  void *cur;
  size_t (*read)(void *ptr, size_t size, size_t nmemb, struct stream *in);
};

static size_t stream_read(void *ptr, size_t size, size_t nmemb,
                          struct stream *in) {
  char *cur = (char *)in->cur;
  const char *end = (const char *)in->end;
  const size_t len = size * nmemb;
  if (cur + len <= end) {
    memcpy(ptr, cur, len);
    in->cur = cur + len;
  } else {
    in->cur = NULL;
    return 0;
  }
  return nmemb;
}

struct app {
  struct stream *in;
#ifndef _MSC_VER
  iconv_t conv;
#endif
  void *shift_jis_buffer;
};

static struct app *create_app(struct app *self, struct stream *in) {
  self->in = in;
#ifndef _MSC_VER
  self->conv = iconv_open("utf-8", "shift-jis");
  assert(self->conv != (iconv_t)-1);
#endif
  self->shift_jis_buffer = NULL;

  return self;
}

static void setup_buffer(struct app *self, const void *input, size_t len) {
  self->in->cur = (char *)input;
  self->in->start = input;
  self->in->end = (char *)input + len;
  self->in->read = stream_read;
}

#define ERROR_RETURN(X, Y) \
  if ((X) != (Y)) return false

static size_t fread_mirror(void *ptr, size_t size, size_t nmemb,
                           struct app *self) {
  struct stream *instream = self->in;

  size_t s = instream->read(ptr, size, nmemb, instream);
  if (s == nmemb) {
    return nmemb;
  }
  assert(0);
  return s;
}

static bool write_trailer(struct app *self) {
  assert(self->in->cur <= self->in->end);
  if (self->in->cur == self->in->end) return true;
  // else it is missing one byte (nul byte):
  char padding;
  size_t s = fread_mirror(&padding, sizeof padding, 1, self);
  ERROR_RETURN(s, 1);
  ERROR_RETURN(padding, 0);

  return true;
}

struct mec_mr3_info {
  uint32_t key;
  uint32_t type;
};

struct mec_mr3_item_data {
  uint32_t len;
  void *buffer;
  size_t size;  // aligned/realloc implementation detail
};

static const uint32_t magic2[] = {0, 0, 12, 0, 0};

static bool read_info(struct app *self, const uint8_t group,
                      struct mec_mr3_info *info) {
  // read key and type at once:
  size_t s = fread_mirror(info, sizeof *info, 1, self);
  ERROR_RETURN(s, 1);
#ifdef MEC_MR3_IO_DEBUG
  bool found = check_mec_mr3_info(group, info->key, info->type);
  ERROR_RETURN(found, true);
#endif

  return true;
}

static void *aligned_alloc_impl(size_t alignment, size_t size) {
#ifdef _MSC_VER
  return _aligned_malloc(size, alignment);
#else
  // return aligned_alloc(alignment, size);
  void *allocPtr;
  int error = posix_memalign(&allocPtr, alignment, size);
  // posix_memalign() returns zero on success
  return error == 0 ? allocPtr : NULL;
#endif
}

static struct mec_mr3_item_data *mec_mr3_aligned_realloc(
    struct mec_mr3_item_data *data, size_t size) {
  if (!data) return NULL;
  // fast path
  if (size <= data->size) {
    return data;
  }
  // else need to reallocate
  const size_t guesstimate = size < 4096 ? 4096 : 2 * size;
  void *buffer = aligned_alloc_impl(64u, guesstimate);
  if (data->buffer) free(data->buffer);
  if (!buffer) return NULL;
  data->buffer = buffer;
  data->size = guesstimate;
  return data;
}

static bool is_aligned(const void *pointer, size_t byte_count) {
  // https://stackoverflow.com/questions/1898153/how-to-determine-if-memory-is-aligned
  return (uintptr_t)pointer % byte_count == 0;
}

enum SignatureType { SIG_UNK = 0, SIG_SIMPLE = 1, SIG_COMPLEX = 2 };
static enum SignatureType compute_signature(const uint32_t sig[5]) {
  // fast path: {0, 0, 12, 0, 0}
  const int b = memcmp(sig, magic2, sizeof(magic2));
  if (b == 0) return SIG_SIMPLE;
  // check the complex one:
  // [0,67937544,12,0,235348672] #20
  // [0,226469240,12,0,235348704] #20
  const bool sig_complex = sig[0] == 0 && sig[2] == 12 && sig[3] == 0;
  if (sig_complex) return SIG_COMPLEX;
  return SIG_UNK;
}

static bool read_data(struct app *self, const uint8_t group,
                      const struct mec_mr3_info *info,
                      struct mec_mr3_item_data *data) {
  (void)group;
  (void)info;
  size_t s = fread_mirror(&data->len, sizeof data->len, 1, self);
  ERROR_RETURN(s, 1);
  // in the wild we have: data->len <= 9509
  uint32_t separator[5];
  s = fread_mirror(separator, sizeof *separator,
                   sizeof separator / sizeof *separator, self);
  ERROR_RETURN(s, sizeof separator / sizeof *separator);
  const enum SignatureType sig_ok = compute_signature(separator);
  ERROR_RETURN(sig_ok != SIG_UNK, true);
  data = mec_mr3_aligned_realloc(data, data->len);
  if (data == NULL) {
    return false;
  }

  s = fread_mirror(data->buffer, 1, data->len, self);
  ERROR_RETURN(s, data->len);

  return true;
}

enum Type {
  ISO_8859_1_STRING =
      0x00000300,  // ASCII string / or struct with 'ISO-8859-1' marker
  FLOAT32_VM2N = 0x00000500,  // float/32bits VM:2n
  FLOAT32_VM3N = 0x00000600,  // float/32bits VM:3n
  DATETIME = 0x00000e00,      // Date/Time stored as ASCII VM:1
  STRUCT_136 =
      0x001f4100,  // Fixed struct 136 bytes (struct with ASCII strings)
  STRUCT_436 =
      0x001f4300,  // Fixed struct 436 bytes (struct with ASCII strings)
  STRUCT_516 =
      0x001f4400,  // Fixed struct 516 bytes (struct with ASCII strings)
  STRUCT_325 =
      0x001f4600,  // Fixed struct 325 bytes (struct with ASCII strings)
  UINT32_VM1 = (int)0xff000400,      // uint32_t, range [0, 4] VM:1
  FLOAT32_VM1 = (int)0xff000800,     // float/32bits VM:1
  INT32_VM1N = (int)0xff002400,      // int32_t (signed) VM:1n
  FLOAT32_VM1N = (int)0xff002800,    // float/32bits VM:1n
  FLOAT64_VM1 = (int)0xff002900,     // float/64bits VM:1
  BOOL32_VM1 = (int)0xff002a00,      // bool/32bits VM:1
  UNICODE_STRING = (int)0xff002c00,  // ASCII, UTF-8 or SHIFT-JIS string
};

struct buffer19 {
  char sig1[0x3];
  unsigned char len2;
  char sig2;
  unsigned char len3;
  char sig3;
  char iso[0x9];
  char sig4;
  unsigned char len4;
  char sig5;
};

#if 0
static void dump2file(const char *in, int len) {
  static int debug = 0;
  char buffer[512];
  sprintf(buffer, "out%04d", debug);
  ++debug;
  FILE *f = fopen(buffer, "wb");
  fwrite(in, 1, len, f);
  fclose(f);
}
#endif

// https://stackoverflow.com/questions/28270310/how-to-easily-detect-utf8-encoding-in-the-string
static bool is_valid_utf8(const char *string) {
  if (!string) return true;

  const unsigned char *bytes = (const unsigned char *)string;
  unsigned int cp;
  int num;

  while (*bytes != 0x00) {
    if ((*bytes & 0x80) == 0x00) {
      // U+0000 to U+007F
      cp = (*bytes & 0x7F);
      num = 1;
    } else if ((*bytes & 0xE0) == 0xC0) {
      // U+0080 to U+07FF
      cp = (*bytes & 0x1F);
      num = 2;
    } else if ((*bytes & 0xF0) == 0xE0) {
      // U+0800 to U+FFFF
      cp = (*bytes & 0x0F);
      num = 3;
    } else if ((*bytes & 0xF8) == 0xF0) {
      // U+10000 to U+10FFFF
      cp = (*bytes & 0x07);
      num = 4;
    } else
      return false;

    bytes += 1;
    for (int i = 1; i < num; ++i) {
      if ((*bytes & 0xC0) != 0x80) return false;
      cp = (cp << 6) | (*bytes & 0x3F);
      bytes += 1;
    }

    if ((cp > 0x10FFFF) || ((cp >= 0xD800) && (cp <= 0xDFFF)) ||
        ((cp <= 0x007F) && (num != 1)) ||
        ((cp >= 0x0080) && (cp <= 0x07FF) && (num != 2)) ||
        ((cp >= 0x0800) && (cp <= 0xFFFF) && (num != 3)) ||
        ((cp >= 0x10000) && (cp <= 0x1FFFFF) && (num != 4)))
      return false;
  }

  return true;
}

// return -1 to indicate error:
static int remove_control_character_utf8(char *string) {
  if (!string) return 0;

  unsigned char *bytes = (unsigned char *)string;
  unsigned int cp;
  int num;
  int modified = 0;

  while (*bytes != 0x00) {
    if ((*bytes & 0x80) == 0x00) {
      // U+0000 to U+007F
      cp = (*bytes & 0x7F);
      num = 1;
    } else if ((*bytes & 0xE0) == 0xC0) {
      // U+0080 to U+07FF
      cp = (*bytes & 0x1F);
      num = 2;
    } else if ((*bytes & 0xF0) == 0xE0) {
      // U+0800 to U+FFFF
      cp = (*bytes & 0x0F);
      num = 3;
    } else if ((*bytes & 0xF8) == 0xF0) {
      // U+10000 to U+10FFFF
      cp = (*bytes & 0x07);
      num = 4;
    } else
      return -1;

    bytes += 1;
    for (int i = 1; i < num; ++i) {
      if ((*bytes & 0xC0) != 0x80) return -1;
      cp = (cp << 6) | (*bytes & 0x3F);
      bytes += 1;
    }

    if ((cp > 0x10FFFF) || ((cp >= 0xD800) && (cp <= 0xDFFF)) ||
        ((cp <= 0x007F) && (num != 1)) ||
        ((cp >= 0x0080) && (cp <= 0x07FF) && (num != 2)) ||
        ((cp >= 0x0800) && (cp <= 0xFFFF) && (num != 3)) ||
        ((cp >= 0x10000) && (cp <= 0x1FFFFF) && (num != 4)))
      return -1;

    if (cp <= 0x001F && num == 1) {
      bytes[-1] = '.';
      modified++;
    } else if (cp == 0x007F && num == 1) {
      bytes[-1] = '?';
      modified++;
    }
  }

  return modified;
}

static char *shift_jis_to_utf8(char *str, size_t len, struct app *self) {
  if (len == 0) {
    return "";
  }
  const size_t guesstimate = len < 128 ? 128 : len * 2;
  self->shift_jis_buffer = realloc(self->shift_jis_buffer, guesstimate);
  char *dest_str = self->shift_jis_buffer;
#ifndef _MSC_VER
  char *in_str = str;
  char *out_str = dest_str;
  size_t inbytes = len;
  size_t outbytes = guesstimate;
  if (iconv(self->conv, &in_str, &inbytes, &out_str, &outbytes) == (size_t)-1) {
#if 0
    // at this point both gbk_str & inbytes have been modified, prefer original
    // values:
    dump2file(str, len);
    printf("[%.*s]", (int)len, str);
    fflush(stdout);
#endif
    return NULL;
  }
  dest_str[guesstimate - outbytes] = 0;
#else
  // guesstimate imply at least 128 bytes:
  strcpy(dest_str, "No iconv support");
#endif
  assert(is_valid_utf8(dest_str));
  return dest_str;
}

static bool print_iso(void *ptr, size_t size, size_t nmemb, struct app *self) {
  assert(size == 1);
  static const char magic[] = {0xdf, 0xff, 0x79};
  if (nmemb >= sizeof magic && memcmp(ptr, magic, sizeof(magic)) == 0) {
    // iso
    struct buffer19 b19;
    if (nmemb < sizeof b19) return 0;
    memcpy(&b19, ptr, sizeof b19);
    if (b19.sig2 != 0x1 || b19.sig3 != 0x0 || b19.sig4 != 0x2 ||
        b19.sig5 != 0x0)
      return 0;
    const size_t diff = nmemb - sizeof b19;
    if (b19.len2 != nmemb - 4 || b19.len3 != 9 || b19.len4 != diff) return 0;
    if (strncmp(b19.iso, "ISO8859-1", 9) != 0) return 0;
    char *str = (char *)ptr + sizeof b19;
    {
      char *dest_str = shift_jis_to_utf8(str, b19.len4, self);
      assert(dest_str != NULL);
      const int modified = remove_control_character_utf8(dest_str);
      assert(modified == 0);
      printf("{%.*s : %s}", 9, b19.iso, dest_str);
    }
  } else {
    // raw string buffer
    printf("\"%.*s\"", (int)nmemb, (char *)ptr);
  }
  return true;
}

static bool print_datetime(void *ptr, size_t size, size_t nmemb,
                           struct app *self) {
  // 11/12/2002,11:27:32
  assert(size == 1);
  (void)self;
  assert(nmemb == 19 || nmemb == 20);
  char *str = (char *)ptr;
  size_t i;
  const size_t len = strnlen(str, nmemb);
  assert(len == 19);
  for (i = 0; i < len; ++i) {
    assert((str[i] >= '0' && str[i] <= '9') || str[i] == '/' || str[i] == ',' ||
           str[i] == ':');
  }

  printf("\"%.*s\"", (int)nmemb, str);
  return true;
}

typedef char str16[16 + 1];
typedef char str64[64 + 1];

struct buffer136 {
  uint32_t zero1;
  str64
      uid1;  // Detached Study Management SOP Class (1.2.840.10008.3.1.2.3.1) ?
  str64 uid2;  // 1.2.840.113745.101000.1098000.X.Y.Z
  uint16_t zero2;
};

void print_buffer136(struct buffer136 *b136) {
  assert(b136->zero1 == 0);
  assert(b136->zero2 == 0);
  printf("{%u,%s,%s,%hu}", b136->zero1, b136->uid1, b136->uid2, b136->zero2);
}

struct buffer436 {
  uint32_t zero;
  char iver[0x45];
  char buf3[0x100];  // phi
  str64 buf4;
  str16 buf5;
  char modality[0x15];
  uint32_t val;
};

static void print_buffer436(struct buffer436 *b436) {
#if 0
  static const char vers1[] = "TM_MR_DCM_V1.0";
  static const char vers2[] = "TM_MR_DCM_V2.0";
  static const char vers3[] = "TM_MR_DCM_V1.0_3";
  static const char vers4[] = "TM_MR1_DCM_V1.0";
  assert(strcmp(b436->iver, vers1) == 0 || strcmp(b436->iver, vers2) == 0 ||
         strcmp(b436->iver, vers3) == 0 || strcmp(b436->iver, vers4) == 0);
#endif
  assert(b436->zero == 0);
  assert(strcmp(b436->modality, "MR") == 0);
#if 0
  assert(b436->val == 0 || b436->val == 1 || b436->val == 3 || b436->val == 4);
#endif
  printf("{%u;%s;%s;%s;%s;%s;%u}", b436->zero, b436->iver, b436->buf3,
         b436->buf4, b436->buf5, b436->modality, b436->val);
}

struct buffer516 {
  str64 zero;  // aka 'none'
  char buf2[0x15];
  char buf3[0x100];  // phi
  str16 buf4;
  str64 buf5;  // Study Instance UID
  str64 buf6;
  uint32_t bools[6];
};

static void print_buffer516(struct buffer516 *b516) {
  printf("{%s;%s;%s;%s;%s;%s", b516->zero, b516->buf2, b516->buf3, b516->buf4,
         b516->buf5, b516->buf6);
  uint32_t c;
  for (c = 0; c < 6; ++c) {
    assert(b516->bools[c] == c % 2);
#if 0
    if (c)
      printf(",");
    printf("%d", b516->bools[c]);
#endif
  }
  printf("}");
}

struct buffer325 {
  str64 array[5];
};

static void print_buffer325(struct buffer325 *b325) {
  int c;
  printf("{");
  for (c = 0; c < 5; ++c) {
    if (c) printf(";");
    printf("%s", b325->array[c]);
  }
  printf("}");
}

static bool print_struct(void *ptr, size_t size, size_t nmemb,
                         struct app *self) {
  (void)self;
  assert(size == 1);
  const size_t s = nmemb;
  if (s == 136) {
    struct buffer136 b136;
    memcpy(&b136, ptr, nmemb);
    print_buffer136(&b136);
  } else if (s == 436) {
    struct buffer436 b436;
    memcpy(&b436, ptr, nmemb);
    print_buffer436(&b436);
  } else if (s == 516) {
    struct buffer516 b516;
    memcpy(&b516, ptr, nmemb);
    print_buffer516(&b516);
  } else if (s == 325) {
    struct buffer325 b325;
    memcpy(&b325, ptr, nmemb);
    print_buffer325(&b325);
  } else {
    assert(0);  // programmer error
    return 0;
  }
  return true;
}

static bool print_shift_jis(void *ptr, size_t size, size_t nmemb,
                            struct app *self) {
  assert(size == 1);
  char *dest_str = shift_jis_to_utf8(ptr, nmemb, self);
  if (dest_str) {
    const int modified = remove_control_character_utf8(dest_str);
    assert(modified >= 0);
    printf("|%sSJIS| \"%s\"", modified > 0 ? "?-" : "", dest_str);
  } else {
    char *str = ptr;
    const size_t len = strlen(str);
    assert(len == nmemb || (len + 1 == nmemb && str[len] == 0));
    const bool ok = is_valid_utf8(str);
    if (ok) {
      const int modified = remove_control_character_utf8(str);
      assert(modified >= 0);
      printf("|%sUTF-8| \"%s\"", modified > 0 ? "?-" : "", str);
    } else {
      printf("|FIXME: Invalid SHIFT-JIS/UTF-8|");
    }
  }
  return true;
}

static void print_int(const int32_t *buffer, int len) {
  const int m = sizeof(int32_t);
  assert(is_aligned(buffer, m));
  assert(len % m == 0);
  int i;
  printf("[");
  for (i = 0; i < len / m; i++) {
    if (i) printf(",");
    int32_t cur = -1;
    memcpy(&cur, buffer + i, sizeof cur);
    printf("%d", cur);
  }
  printf("]");
}

static void print_float(const float *buffer, int len) {
  const int m = sizeof(float);
  assert(is_aligned(buffer, m));
  assert(len % m == 0);
  int i;
  printf("[");
  for (i = 0; i < len / m; i++) {
    if (i) printf(",");
    float cur = -1;
    memcpy(&cur, buffer + i, sizeof cur);
    assert(isfinite(cur) && !isnan(cur));
    printf("%f", cur);
  }
  printf("]");
}

static void print_double(const double *buffer, int len) {
  const int m = sizeof(double);
  assert(is_aligned(buffer, m));
  assert(len % m == 0);
  int i;
  printf("[");
  for (i = 0; i < len / m; i++) {
    if (i) printf(",");
    const double cur = buffer[i];
    assert(isfinite(cur) && !isnan(cur));
    printf("%g", cur);
  }
  printf("]");
}

static bool print_int32(void *ptr, size_t size, size_t nmemb,
                        struct app *self) {
  assert(size == 1);
  (void)self;
  // assert(nmemb == 4 || nmemb == 8 || nmemb == 12 || nmembnmemb == 24 || nmemb
  // == 32 || nmemb == 48);
  assert(nmemb % 4 == 0);
  print_int(ptr, nmemb);

  return true;
}

static bool print_float32(void *ptr, size_t size, size_t nmemb,
                          struct app *self) {
  assert(size == 1);
  (void)self;
  assert(nmemb == 4);
  print_float(ptr, nmemb);

  return true;
}

static bool print_float32_vm1n(void *ptr, size_t size, size_t nmemb,
                               struct app *self) {
  assert(size == 1);
  (void)self;
  assert(nmemb % 4 == 0);
  print_float(ptr, nmemb);

  return true;
}

static bool print_float32_vm2n(void *ptr, size_t size, size_t nmemb,
                               struct app *self) {
  assert(size == 1);
  (void)self;
  assert((nmemb / 4) % 2 == 0);
  assert(nmemb == 8 || nmemb == 40 || nmemb == 80 || nmemb == 88);
  // FIXME: low/high value for nmemb==40&80 makes them look like double...
  print_float(ptr, nmemb);

  return true;
}

static bool print_float32_vm3n(void *ptr, size_t size, size_t nmemb,
                               struct app *self) {
  assert(size == 1);
  (void)self;
  assert((nmemb / 4) % 3 == 0);
  assert(nmemb == 12 || nmemb == 36);
  print_float(ptr, nmemb);

  return true;
}

static bool print_float64(void *ptr, size_t size, size_t nmemb,
                          struct app *self) {
  assert(size == 1);
  (void)self;
  assert(nmemb == 8);
  print_double(ptr, nmemb);
  return true;
}

static bool print_uint32(void *ptr, size_t size, size_t nmemb,
                         struct app *self) {
  assert(size == 1);
  assert(is_aligned(ptr, 4));
  (void)self;
  assert(nmemb == 4);
  uint32_t u;
  memcpy(&u, ptr, nmemb);
  assert(u == 0x0 || u == 0x1 || u == 0x4);
  printf("%u", u);
  return true;
}

static bool print_bool32(void *ptr, size_t size, size_t nmemb,
                         struct app *self) {
  assert(size == 1);
  assert(is_aligned(ptr, 4));
  (void)self;
  assert(nmemb == 4);
  uint32_t u;
  memcpy(&u, ptr, nmemb);
  assert(u == 0x0 || u == 0x1);
  printf("%s", u ? "true" : "false");
  return true;
}

static bool print(struct app *self, const uint8_t group,
                  const struct mec_mr3_info *info,
                  struct mec_mr3_item_data *data) {
  const char *name = get_mec_mr3_info_name(group, info->key);
  const uint32_t sign = info->type >> 24;
  const char symb = sign ? '_' : ' ';

  bool ret = true;
  uint32_t mult = 1;
  // print info
  printf("(%01x,%05x) %c%04x ", group, info->key, symb,
         (info->type & 0x00ffff00) >> 8);
  // print data:
  switch (info->type) {
    case ISO_8859_1_STRING:
      ret = print_iso(data->buffer, 1, data->len, self);
      break;
    case FLOAT32_VM2N:
      ret = print_float32_vm2n(data->buffer, 1, data->len, self);
      break;
    case FLOAT32_VM3N:
      ret = print_float32_vm3n(data->buffer, 1, data->len, self);
      break;
    case DATETIME:
      ret = print_datetime(data->buffer, 1, data->len, self);
      break;
    case STRUCT_136:
    case STRUCT_436:
    case STRUCT_516:
    case STRUCT_325:
      ret = print_struct(data->buffer, 1, data->len, self);
      break;
    case UNICODE_STRING:
      ret = print_shift_jis(data->buffer, 1, data->len, self);
      break;
    case FLOAT32_VM1:
      ret = print_float32(data->buffer, 1, data->len, self);
      break;
    case INT32_VM1N:
      ret = print_int32(data->buffer, 1, data->len, self);
      break;
    case FLOAT32_VM1N:
      ret = print_float32_vm1n(data->buffer, 1, data->len, self);
      break;
    case FLOAT64_VM1:
      ret = print_float64(data->buffer, 1, data->len, self);
      break;
    case UINT32_VM1:
      ret = print_uint32(data->buffer, 1, data->len, self);
      break;
    case BOOL32_VM1:
      ret = print_bool32(data->buffer, 1, data->len, self);
      break;
    default:
      printf("|NotImplemented|");
      ret = true;
  }
  // print key name
  if (!name) {
    static char buf[512];
    snprintf(buf, sizeof buf, "Missing: {0x%02x, 0x%08x, 0x%08x, \"\"}, //",
             group, info->key, info->type);
    name = buf;
  }
  printf(" # %u,%u %s\n", data->len, mult, name);

  return ret;
}

static bool read_group(struct app *self, uint8_t group, uint32_t nitems,
                       struct mec_mr3_info *info,
                       struct mec_mr3_item_data *data) {
  bool good = true;
  uint32_t i;
  for (i = 0; i < nitems && good; ++i) {
    good = good && read_info(self, group, info);
    // lazy evaluation:
    good = good && read_data(self, group, info, data);
    good = good && print(self, group, info, data);
  }
  return good;
}

// If the number of element read is below the magic value, this indicate the
// last groups of elements:
#define MAGIC_NUM_ELEMENTS 5

bool mec_mr3_print(const void *input, size_t len) {
  if (!input) return false;
  struct stream sin;
  struct app a;
  struct app *self = create_app(&a, &sin);
  setup_buffer(self, input, len);

  bool good = true;
  struct mec_mr3_info info;
  struct mec_mr3_item_data data;
  data.size = data.len = 0;
  data.buffer = NULL;

  uint32_t remain = 1;
  size_t s;
  bool last_groups = false;
  uint8_t group = 0;
  // read until last set of groups found:
  while (!last_groups && good) {
    uint32_t nitems;
    s = fread_mirror(&nitems, sizeof nitems, 1, self);
    if (s != 1 || nitems == 0) {
      good = false;
    }
    if (good && nitems <= MAGIC_NUM_ELEMENTS) {
      // special case to handle last groups
      remain = nitems;
      last_groups = true;
      s = fread_mirror(&nitems, sizeof nitems, 1, self);
      if (s != 1 || nitems == 0) {
        good = false;
      }
    }
    // lazy evaluation
    ++group;
    good = good && read_group(self, group, nitems, &info, &data);
  }
  // read remaining groups:
  while (good && --remain != 0) {
    uint32_t nitems;
    s = fread_mirror(&nitems, sizeof nitems, 1, self);
    if (s != 1 || nitems <= MAGIC_NUM_ELEMENTS) {
      good = false;
    }
    ++group;
    good = good && read_group(self, group, nitems, &info, &data);
  }
  // release memory:
#ifdef _MSC_VER
  _aligned_free(data.buffer);
#else
  free(data.buffer);
#endif
#ifndef _MSC_VER
  iconv_close(self->conv);
#endif
  free(self->shift_jis_buffer);
  if (!good) return false;

  // write trailer:
  if (!write_trailer(self)) {
    return false;
  }

  // make sure the whole input was processed:
  assert(self->in->cur <= self->in->end);  // programmer error
  if (self->in->cur < self->in->end) {
    return false;
  }
  return true;
}
