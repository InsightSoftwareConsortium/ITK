/*
  NrrdIO: C library for NRRD file IO (with optional compressions)
  Copyright (C) 2009--2026  University of Chicago
  Copyright (C) 2005--2008  Gordon Kindlmann
  Copyright (C) 1998--2004  University of Utah

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.

  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software. If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.

  3. This notice may not be removed or altered from any source distribution.
*/

#include "NrrdIO.h"

/* removed for TeemV2:
 *  / * this has to default to false in order for airStrtok to be a
 *    functional substitute for strtok() * /
 *  int airStrtokQuoting = AIR_FALSE;
 */

/*
 ******** airStrdup()
 *
 * because they didn't put strdup() in ANSI.
 * This will return NULL if given NULL.
 */
char *
airStrdup(const char *str) {
  char *ret;
  size_t retSize;

  if (!str) {
    ret = NULL;
  } else {
    retSize = strlen(str) + 1;
    ret = AIR_MALLOC(retSize, char);
    if (ret) {
      airStrcpy(ret, retSize, str);
    }
  }
  return ret;
}

/*
******** airStrlen()
**
** just like strlen, but safe to call on NULL (for which return is 0)
*/
size_t
airStrlen(const char *s) {
  size_t ret;

  if (!s) {
    ret = 0;
  } else {
    ret = strlen(s);
  }
  return ret;
}

/*
******** airStrtok()
*
* thread-safe strtok() replacement.  Use just like strtok(), but on each call to parse a
* given string, pass as the last argument the address of a char*, to be used for saving
* state while the string is traversed.  Like strtok(), this will alter the "s" array
* passed to it on the first call, and like strtok(), this returns pointers into this
* string (rather than allocating new strings for each token).
*
* NOTE: TeemV2 removed global airStrtokQuoting and the code in here that used it.
*/
char *
airStrtok(char *s, const char *ct, char **last) {
  char *h, *e;

  if (!(ct && last)) {
    /* can't do any work, bail */
    return NULL;
  }
  h = s ? s : *last;
  if (!airStrlen(h)) return NULL;
  h += strspn(h, ct);
  e = h + strcspn(h, ct);
  if ('\0' == *e) {
    *last = e;
  } else {
    *e = '\0';
    *last = e + 1;
  }
  return h;
}

/*
******** airStrntok()
**
** returns the number of tokens parsable by airStrtok(), but does
** NOT alter the given string
*/
unsigned int
airStrntok(const char *_s, const char *ct) {
  char *s, *t, *l = NULL;
  unsigned int n = 0;

  if (_s && ct) {
    s = airStrdup(_s);
    t = airStrtok(s, ct, &l);
    while (t) {
      n++;
      t = airStrtok(NULL, ct, &l);
    }
    airFree(s); /* no NULL assignment to s, else compile warnings */
  }
  return n;
}

char *
airStrtrans(char *s, char from, char to) {
  size_t i, l;

  if (s) {
    l = strlen(s);
    for (i = 0; i < l; i++) {
      if (s[i] == from) {
        s[i] = to;
      }
    }
  }
  return s;
}

/*
 ******** airStrcpy
 *
 * Like strncpy but logic is different and more useful:
 *    `dst` is allocated for `dstSize` chars. Copy as much of src as can
 *    fit in `dst`, while always 0-terminating the result.
 * instead of strncpy's "Copy at most n characters, blah blah blah,
 * and you still have to 0-terminate the rest yourself".
 * Thus, instead of:
 *    snprintf(dst, dstSize, "%s", src)   or   snprintf(dst, dstSize, src)
 * you can just
 *    airStrcpy(dst, dstSize, src)
 * This is intended to be like the (non-standard) strlcpy, except that we
 * return the given `dst` pointer, rather than the # of chars written.
 *
 * With declaration `buff[AIR_STRLEN_SMALL+1]`, you call
 *    airStrcpy(buff, AIR_STRLEN_SMALL+1, src),
 * and know that this will not run off the end of `buff`,
 * and that strlen(buff) <= AIR_STRLEN_SMALL.
 *
 * Returns NULL if there was a problem (NULL dst or dstSize zero),
 * otherwise returns dst
 */
char *
airStrcpy(char *dst, size_t dstSize, const char *src) {

  if (!(dst && dstSize > 0)) {
    /* don't have usable `dst` to copy into */
    return NULL;
  }
  /* else dst && dstSize > 0 */
  if (!src || dstSize == 1) {
    /* got either NULL src (?) or have only space for null-termination */
    dst[0] = '\0';
  } else {
    /* have non-NULL src AND dstSize > 1 */
    size_t ii;
    for (ii = 0; src[ii]             /* have non-null character to copy */
                 && ii < dstSize - 1 /* can save to dst[ii] w/ space for final '\0' */
         ;
         ii++) {
      dst[ii] = src[ii];
    }
    dst[ii] = '\0';
  }
  return dst;
}

/*
******** airEndsWith
**
** if "s" ends with "suff", then returns 1, 0 otherwise
*/
int
airEndsWith(const char *s, const char *suff) {

  if (!(s && suff)) return 0;
  if (!(strlen(s) >= strlen(suff))) return 0;
  if (!strncmp(s + strlen(s) - strlen(suff), suff, strlen(suff)))
    return 1;
  else
    return 0;
}

/*
******** airUnescape()
**
** unescapes \\ and \n in place in a given string, which is the only form of
** escaping supported by key/value pairs in NRRD headers.
** Always returns the same pointer as given
*/
char *
airUnescape(char *s) {
  size_t i, j, len;
  int found = 0;

  len = airStrlen(s);
  if (!len) return s;

  for (i = 1, j = 0; i < len; i++, j++) {
    if (s[i - 1] == '\\' && s[i] == '\\') {
      s[j] = '\\';
      i++;
      found = 1;
    } else if (s[i - 1] == '\\' && s[i] == 'n') {
      s[j] = '\n';
      i++;
      found = 1;
    } else {
      s[j] = s[i - 1];
      found = 0;
    }
  }
  if (i == len || !found) s[j++] = s[len - 1];
  s[j] = 0;

  return s;
}

static char *
oneLinifyMore(char *str, int keepInternalNewline) {
  size_t ii, jj, len;

  len = airStrlen(str);
  if (!len) return str;
#define SP(I) (' ' == str[(I)])
#define NL(I) ('\n' == str[(I)])
  /* convert white space to space (' '), and delete unprintables
     actually (for TeemV2) sometimes keep internal newlines */
  for (ii = 0; ii < len; ii++) {
    if (isspace(str[ii]) && (!keepInternalNewline || !NL(ii))) {
      str[ii] = ' ';
      continue;
    }
    if (!isprint(str[ii]) && (!keepInternalNewline || !NL(ii))) {
      for (jj = ii; jj < len; jj++) {
        /* this will copy the '\0' at the end */
        str[jj] = str[jj + 1];
      }
      ii--;
      /* string got shorter (prevent ininite loop) */
      len--;
      continue;
    }
  }
  /* compress all contiguous spaces into one
     except also handle any newlines that keepInternalNewline preserved */
  for (ii = 0; ii < len; ii++) {
    while ((SP(ii) || NL(ii)) && (SP(ii + 1) || NL(ii + 1))) {
      /* something probably needs to be compacted */
      size_t kk; /* index of first character to over-write by shifting */
      if ((SP(ii) && SP(ii + 1)) || (NL(ii) && NL(ii + 1))) {
        /* two contiguous spaces, or two contiguous newlines */
        kk = ii + 1;
      } else if (SP(ii) && NL(ii + 1)) {
        /* space at ii should be over-written by newline at ii+1 */
        kk = ii;
      } else {
        /* newline at ii should be preserved; move along */
        break;
      }
      for (jj = kk; jj < len; jj++) {
        str[jj] = str[jj + 1];
      }
    }
  }
  /* lose trailing white space */
  ii = airStrlen(str);
  while (ii && (SP(ii - 1) || NL(ii - 1))) {
    str[ii - 1] = '\0';
    ii--;
  }
#undef SP
#undef NL

  return str;
}

/*
 ******** airOneLinify()
 *
 * converts all contiguous white space (as determined by isspace()) to a single ' ',
 * entirely removes non-printable (as determined by isprint()) characters, and entirely
 * removes white space contiguous with the end of the string, even if that means
 * shrinking the string to "".
 *
 * Useful for cleaning up lines of text to be saved as strings in fields of other
 * structs.
 *
 * Whatever happens, this returns the pointer passed to it
 */
char *
airOneLinify(char *str) {
  return oneLinifyMore(str, AIR_FALSE /* keepInternalNewline */);
}

/*
 ******** airMultiLinify
 *
 * like airOneLinify but non-contiugous internal newlines are preserved
 */
char *
airMultiLinify(char *str) {
  return oneLinifyMore(str, AIR_TRUE /* keepInternalNewline */);
}

/*
******** airToLower()
**
** calls tolower() on all characters in a string, and returns the same
** pointer that it was given
*/
char *
airToLower(char *str) {
  char *c;

  if (str) {
    c = str;
    while (*c) {
      *c = AIR_CAST(char, tolower(AIR_INT(*c)));
      c++;
    }
  }
  return str;
}

/*
******** airToUpper()
**
** calls toupper() on all characters in a string, and returns the same
** pointer that it was given
*/
char *
airToUpper(char *str) {
  char *c;

  if (str) {
    c = str;
    while (*c) {
      *c = AIR_CAST(char, toupper(AIR_INT(*c)));
      c++;
    }
  }
  return str;
}

/*
 ******** airOneLine()
 *
 * gets one line from `file`, putting it into an array of given size. `size` must be the
 * size of line buffer `line`: the size which `line` was allocated for, not the number of
 * non-null characters it was meant to hold.  `size` must be at least 3.  This always
 * null-terminates the contents of the array (except if the arguments are invalid).  The
 * idea is that the null-termination replaces the (text) line termination.
 *
 * Return values:
 * 0 and line[0]=='\0': immediately hit EOF, or arguments are invalid
 * 0 and line[0]!='\0': read something (with strlen() < size-1) into `line`
 *                      but hit EOF before seeing newline;
 *                      caller will need strlen() to find out its length
 *                   1: line was a single newline (or other line termination)
 *      N with N<=size: line was N-1 characters followed by newline
 *                      OR if N==size: N-1 chars followed by EOF
 *              size+1: didn't see a newline within size-1 characters
 *                      and there's more in the file to consume
 *
 * So in normal cases, the return is the number of characters making up the line,
 * including the line termination
 *
 * In case of DOS\Windows\Cygwin: this will quietly pretend that "\r\n" pair is really
 * just "\n", including the way that characters making up the line are counted. However,
 * there is no pretension that on those platforms, "\n" by itself does not actually count
 * as a newline.
 *
 * Or, if trafficking in legacy Mac text files (for which the line termination is
 * only "\r", the same applies- these are also effectively treated the same as a newline.
 *
 * Uses ungetc(), respecting "man ungetc" info that one character of push-back is
 * guaranteed
 */
size_t
airOneLine(FILE *file, char *line, size_t size) {
  int cc = 0;
  size_t ii, ret;

  if (!(size >= 3 /* need room for a character and a Windows newline */
        && line && file)) {
    if (line) line[0] = '\0';
    return 0;
  }
  /* cc is always set at least once, but not so for any char in line[]  */
  for (ii = 0; (ii < size - 1               /* save line[size-1] for '\0' */
                && EOF != (cc = getc(file)) /* didn't hit EOF trying to read char */
                && cc != '\n'               /* char isn't newline */
                && cc != '\r');             /* char isn't carriage return */
       ++ii) {
    line[ii] = AIR_CAST(char, cc);
  }

  if (EOF == cc) {
    /* for-loop terminated because we hit EOF */
    line[ii] = '\0'; /* TeemV2: changed from line[0] = '\0' */
    ret = 0;
  } else if ('\r' == cc || '\n' == cc) {
    /* for-loop terminated because we hit '\n' or '\r' */
    if ('\r' == cc) {
      /* could be either legacy Mac "\r" or DOS "\r\n" */
      cc = getc(file);
      if (EOF != cc && '\n' != cc) {
        /* oops, we got something, and it was not a '\n'
           ==> it was legacy Mac; put `cc` back for later */
        ungetc(cc, file);
      }
      /* else either cc == EOF (legacy Mac line and then EOF)
                  or cc == '\n' (DOS line and then EOF) */
    }
    /* else '\n' == cc: a unix line termination */
    line[ii] = '\0';
    ret = ii + 1;
  } else {
    /* for-loop terminated because we got to end of `line` buffer: ii == size-1
       null-terminate `line` */
    line[size - 1] = '\0';
    /* and then figure out what to return */
    cc = getc(file);
    if (EOF == cc || '\n' == cc) {
      /* with EOF there was nothing more to read anyway, or,
         next up is Unix line termination , either way not maxed out */
      ret = size;
    } else if ('\r' == cc) {
      /* if got to end of `line` buffer BUT then have "\r" or "\r\n",
         we are not maxed out */
      cc = getc(file);
      if (EOF != cc && '\n' != cc) {
        /* oops, legacy Mac line termination with more later, put it back */
        ungetc(cc, file);
      }
      /* else either cc == EOF (legacy Mac line termination and then EOF)
                  or cc == '\n' (DOS line termination and then EOF)
         ==> same as Unix line termination above */
      ret = size;
    } else {
      /* we were NOT about to get a line termination,
         we really did run out of space in output `line` buffer */
      if (EOF != cc) {
        ungetc(cc, file);
      }
      ret = size + 1;
    }
  }
  return ret;
}
