/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h> /* realloc */
#include <string.h> /* memcpy */

struct stream {
  const void *start;
  const void *end;
  void *cur;
  size_t (*read)(void *ptr, size_t size, size_t nmemb, struct stream *in);
  size_t (*write)(const void *ptr, size_t size, size_t nmemb,
                  struct stream *outstream);
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

static size_t stream_write(const void *ptr, size_t size, size_t nmemb,
                           struct stream *out) {
  char *cur = (char *)out->cur;
  const char *end = (const char *)out->end;
  const size_t len = size * nmemb;
  if (cur + len <= end) {
    memcpy(cur, ptr, len);
    out->cur = cur + len;
  } else {
    out->cur = NULL;
    return 0;
  }
  return nmemb;
}

enum CSA_TYPE { INVALID = 0, NOMAGIC = 1, SV10 = 2 };

struct app {
  struct stream *in;
  struct stream *out;
  uint32_t nelements;
  enum CSA_TYPE csa_type;
};

static struct app *create_app(struct app *self, struct stream *in,
                              struct stream *out) {
  self->in = in;
  self->out = out;
  self->nelements = 0;
  self->csa_type = INVALID;

  return self;
}

static void setup_buffer(struct app *self, void *output, const void *input,
                         size_t len) {
  self->in->cur = (char *)input;
  self->in->start = input;
  self->in->end = (char *)input + len;
  self->in->read = stream_read;
  self->out->cur = output;
  self->out->start = output;
  self->out->end = (char *)output + len;
  self->out->write = stream_write;
}

//#define CSA_DEBUG

#ifdef CSA_DEBUG
#define ERROR_RETURN(X, Y) \
  if ((X) != (Y)) assert(0);
#else
#define ERROR_RETURN(X, Y) \
  if ((X) != (Y)) return false
#endif

static size_t fread_mirror(void *ptr, size_t size, size_t nmemb,
                           struct app *self) {
  struct stream *instream = self->in;
  struct stream *outstream = self->out;

  size_t s = instream->read(ptr, size, nmemb, instream);
  if (s == nmemb) {
    s = outstream->write(ptr, size, nmemb, outstream);
    if (s == nmemb) return nmemb;
  }
  return 0;
}

static size_t fread_mirror_clean(void *ptr, size_t size, size_t nmemb,
                                 struct app *self) {
  struct stream *instream = self->in;
  struct stream *outstream = self->out;

  if (nmemb != 64) return 0;
  size_t s = instream->read(ptr, size, nmemb, instream);
  if (s == nmemb) {
    char *str = (char *)ptr;
    const size_t len = strnlen(str, nmemb);
    assert(len < nmemb);
    size_t i;
    for (i = len; i < nmemb; ++i) {
      str[i] = 0;
    }
    s = outstream->write(ptr, size, nmemb, outstream);
    if (s == nmemb) return nmemb;
  }
  return 0;
}

static bool read_magic(struct app *self) {
  enum CSA_TYPE magic = INVALID;
  uint32_t n;
  size_t s = fread_mirror(&n, sizeof n, 1, self);
  ERROR_RETURN(s, 1);
  uint32_t unused;
  s = fread_mirror(&unused, sizeof unused, 1, self);
  ERROR_RETURN(s, 1);

  // Handle very special case hopefully no conflict with case of no-magic
  if (n == 0x30315653 && unused == 0x01020304) {  // aka 'SV10'
    // SIEMENS_CSA2.dcm
    s = fread_mirror(&n, sizeof n, 1, self);
    ERROR_RETURN(s, 1);
    ERROR_RETURN(n < 0x100, true);
    s = fread_mirror(&unused, sizeof unused, 1, self);
    ERROR_RETURN(s, 1);
    ERROR_RETURN(unused, 0x4d);  // 'M'
    magic = SV10;
  } else if (n < 0x100 && unused == 0x4d) {  // 'M'
    // SIEMENS_Sonata-16-MONO2-Value_Multiplicity.dcm
    magic = NOMAGIC;
  } else {
    return false;
  }
  self->nelements = n;
  self->csa_type = magic;
  return true;
}

static bool write_trailer(struct app *self) {
  size_t s;
  // trailing byte in SV10
  if (self->csa_type == SV10) {
    uint32_t unused;
    s = fread_mirror(&unused, sizeof unused, 1, self);
    ERROR_RETURN(s, 1);
    ERROR_RETURN(unused, 0);
  } else if (self->csa_type == NOMAGIC) {
    // no magic. seems to contains some kind of data, hopefully no PHI
#if 1
    uint32_t i;
    for (i = 0; i < 28u; ++i) {
      uint32_t unused;
      s = fread_mirror(&unused, sizeof unused, 1, self);
      ERROR_RETURN(s, 1);
    }
#else
    char unused[28 * 4];
    s = fread_mirror(unused, sizeof *unused, sizeof unused / sizeof *unused,
                     self);
    ERROR_RETURN(s, 28 * 4);
    int i;
    for (i = 0; i < 4 * 28; ++i) {
      const char c = unused[i];
      assert(c == 0x0 || c == ' ' || c == '.' || (c >= '0' && c <= '9'));
    }
#endif
  } else {
    assert(0);
    return false;
  }
  return true;
}

struct csa_info {
  char name[64];
  uint32_t vm;
  char vr[4];
  int32_t syngodt;
  uint32_t nitems;
};

struct csa_item_data {
  uint32_t len;
  char *buffer;
};

static bool read_info(struct app *self, struct csa_info *i) {
  // name. This is 64bytes string. It is assumed that there will be a trailing
  // \0
  size_t s = fread_mirror_clean(i->name, sizeof *i->name,
                                sizeof i->name / sizeof *i->name, self);
  ERROR_RETURN(s, 64);
  // vm
  s = fread_mirror(&i->vm, sizeof i->vm, 1, self);
  ERROR_RETURN(s, 1);
  // vm == 115301884 seems to be ok...
  ERROR_RETURN(i->vm < 0x6df5dfd, true);
  // vr
  s = fread_mirror(i->vr, sizeof *i->vr, sizeof i->vr / sizeof *i->vr, self);
  ERROR_RETURN(s, 4);
  {
    const char *s = i->vr;
    ERROR_RETURN(s[0] >= 'A' && s[0] <= 'Z', true);
    ERROR_RETURN(s[1] >= 'A' && s[1] <= 'Z', true);
    ERROR_RETURN(s[2], 0);
    if (self->csa_type == SV10) {
      ERROR_RETURN(s[3], 0);
    }
  }
  // syngodt (signed)
  s = fread_mirror(&i->syngodt, sizeof i->syngodt, 1, self);
  ERROR_RETURN(s, 1);
  // number of items
  s = fread_mirror(&i->nitems, sizeof i->nitems, 1, self);
  if (self->csa_type == SV10) assert(i->nitems % 6 == 0);
  ERROR_RETURN(s, 1);
  {
    uint32_t unused;
    s = fread_mirror(&unused, sizeof unused, 1, self);
    ERROR_RETURN(s, 1);
    ERROR_RETURN(unused == 0x4d || unused == 0xcd, true);
  }

  return true;
}

static bool read_data(struct app *self, struct csa_item_data *d) {
  size_t s = fread_mirror(&d->len, sizeof d->len, 1, self);
  ERROR_RETURN(s, 1);
  int j;
  for (j = 0; j < 3; j++) {
    uint32_t unused;
    s = fread_mirror(&unused, sizeof unused, 1, self);
    ERROR_RETURN(s, 1);
    ERROR_RETURN(unused == d->len || unused == 0x4d || unused == 0xcd, true);
  }

  if (d->len != 0) {
    const uint32_t padding_len = (4 - d->len % 4) % 4;
    const uint32_t padded_len = ((d->len + 3u) / 4u) * 4u;  // (len + 3) & ~0x03
    assert(padded_len == d->len + padding_len);             // programmer error
    assert(padded_len != 0);
    d->buffer = (char *)realloc(d->buffer, padded_len);
    ERROR_RETURN(d->buffer != NULL, true);
    s = fread_mirror(d->buffer, sizeof *d->buffer, padded_len, self);
    ERROR_RETURN(s, padded_len);
  }

  return true;
}

#undef ERROR_RETURN

static bool csa_scrub(void *output, const void *input, size_t len) {
  if (!input || !output) return false;
  struct stream sin;
  struct stream sout;
  struct app a;
  struct app *self = create_app(&a, &sin, &sout);
  setup_buffer(self, output, input, len);
  if (!read_magic(self)) return false;

  bool good = true;
  struct csa_info info;
  struct csa_item_data data;
  data.len = 0;
  data.buffer = NULL;
  uint32_t element;
  for (element = 0; good && element < self->nelements; ++element) {
    // read csa key info
    if (!read_info(self, &info)) {
      good = false;
      break;
    }
    // read all csa item data:
    uint32_t item;
    for (item = 0; good && item < info.nitems; ++item) {
      if (!read_data(self, &data)) {
        good = false;
        break;
      }
    }  // end for items
  }
  // release memory:
  free(data.buffer);
  if (!good) return false;

  // write trailer:
  if (!write_trailer(self)) {
    return false;
  }

  // make sure the whole input was processed:
  assert(self->in->cur <= self->in->end);
  if (self->in->cur < self->in->end) {
    return false;
  }
  assert(self->out->cur == self->out->end);  // programmer error
  return true;
}

void *csa_memcpy(void *dest, const void *src, size_t n) {
  const bool b = csa_scrub(dest, src, n);
  return b ? dest : NULL;
}
