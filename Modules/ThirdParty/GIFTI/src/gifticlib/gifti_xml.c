#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "gifti_io.h"

#define GXML_MIN_BSIZE 2048
#define GXML_DEF_BSIZE 32768

/* local prototypes */
static int  append_to_cdata     (gxml_data *, const char *, int);
static int  append_to_data      (gxml_data *, const char *, int);
static int  append_to_data_ascii(gxml_data *, const char *, int);
static int  append_to_data_b64  (gxml_data *, char*,long long,const char*, int);
/*
static int  append_to_data_b64gz(gxml_data *, const char *, int);
*/

static int  add_label_rgba      (gxml_data *, giiLabelTable *, float *);
static int  append_to_xform     (gxml_data *, const char *, int);
static int  apply_da_list_order (gxml_data *, const int *, int);
static int  int_compare         (const void * v0, const void * v1);
static int  copy_b64_data       (gxml_data *, const char *, char *, int, int*);
static int  decode_ascii       (gxml_data*,char*,int,int,void*,long long*,int*);
static int  decode_b64          (gxml_data*, char*, int, char *, long long *);
static int  disp_gxml_data      (char *, gxml_data *, int);
static int  ename2type          (const char *);
static int  epush               (gxml_data *, int, const char *, const char **);
static int  epop                (gxml_data *, int, const char *);
static int  free_xd_data        (gxml_data *);
static int  get_label_attrs     (gxml_data *, const char **, int *, float *);
static int  init_gxml_data      (gxml_data *, int, const int *, int);
static int  partial_buf_size    (long long);

static int  push_gifti          (gxml_data *, const char **);
static int  push_meta           (gxml_data *);
static int  push_md             (gxml_data *);
static int  push_name           (gxml_data *);
static int  push_value          (gxml_data *);
static int  push_LT             (gxml_data *);
static int  push_label          (gxml_data *, const char **);
static int  push_darray         (gxml_data *, const char **);
static int  push_cstm           (gxml_data *);
static int  push_data           (gxml_data *);
static int  push_dspace         (gxml_data *);
static int  push_xspace         (gxml_data *);
static int  push_xform          (gxml_data *);
static int  reset_xml_buf       (gxml_data *, char **, int *);

static void show_attrs          (gxml_data *,int,const char **);
static void show_depth          (int, int, FILE *);
static void show_enames         (FILE *);
static int  show_stack          (char *, gxml_data *);

static int  short_sorted_da_list(gxml_data *dp, const int * dalist, int len);
static int  stack_is_valid      (gxml_data *);
static int  whitespace_len      (const char *, int);
static int  update_xml_buf_size (gxml_data *, long long);
static int  update_partial_buffer(char **, int *, long long, int);

static int  count_bad_b64_chars (const char *, int);
static int  show_bad_b64_chars  (const char *, int);

static giiMetaData * find_current_MetaData(gxml_data *, int);

#ifndef XMLCALL
/* XMLCALL was added to expat in version 1.95.7 to define a calling convention,
 * as cdecl
 */
#if defined(XML_USE_MSC_EXTENSIONS)
#define XMLCALL __cdecl
#elif defined(__GNUC__) && defined(__i386)
#define XMLCALL __attribute__((cdecl))
#else
#define XMLCALL
#endif
#endif /* not defined XMLCALL */

#ifndef XML_STATUS_ERROR
#define XML_STATUS_ERROR 0
#endif
#ifndef XML_STATUS_OK
#define XML_STATUS_OK 1
#endif

static void XMLCALL cb_start_ele    (void *, const char *, const char **);
static void XMLCALL cb_end_ele      (void *, const char *);
static void XMLCALL cb_char         (void *, const char *, int);
static void XMLCALL cb_instr        (void *, const char *, const char *);
static void XMLCALL cb_comment      (void *, const char *);
static void XMLCALL cb_cdata_start  (void *);
static void XMLCALL cb_cdata_end    (void *);
static void XMLCALL cb_default      (void *, const char *, int);
static void XMLCALL cb_xml_dec      (void *, const char *, const char * , int);
static void XMLCALL cb_start_doctype(void *, const char *, const char *,
                                     const char *, int);
static void XMLCALL cb_end_doctype  (void *);
static void XMLCALL cb_elem_dec     (void *, const char *, XML_Content *);
static XML_Parser init_xml_parser   (void *);

/* writing functions */
static int  gxml_write_gifti        (gxml_data *, FILE *);
static int  gxml_write_preamble     (FILE *);

static int  ewrite_text_ele         (int, const char *, const char *,
                                     int, int, FILE *);
static int  ewrite_coordsys         (gxml_data *, giiCoordSystem *, FILE *);
static int  ewrite_data             (gxml_data *, giiDataArray *, FILE *);
static int  ewrite_data_line        (void *, int, long long, long long,
                                     int, FILE *);
static int  ewrite_double_line      (double *, int, int, FILE *);
static int  ewrite_int_attr         (const char *, int, int, int, FILE *);
static int  ewrite_long_long_attr   (const char *, long long, int, int, FILE *);
static int  ewrite_str_attr         (const char*, const char*, int, int, FILE*);
static int  ewrite_darray           (gxml_data *, giiDataArray *, FILE *);
static int  ewrite_ex_atrs          (gxml_data *, nvpairs *, int, int, FILE *);
static int  ewrite_LT               (gxml_data*, giiLabelTable*, int, FILE*);
static int  ewrite_meta             (gxml_data *, giiMetaData *, FILE *);

static int  gxml_disp_b64_data      (const char *, const void *, int, FILE *);

/* these should match GXML_ETYPE_* defines */
static char * enames[GXML_MAX_ELEN] = {
    "Invalid", "GIFTI", "MetaData", "MD", "Name", "Value", "LabelTable",
    "Label", "DataArray", "CoordinateSystemTransformMatrix", "Data",
    "DataSpace", "TransformedSpace", "MatrixData", "CDATA"
};

/* ---------------------------------------------------------------------- */
/* GIFTI XML global struct and access functions */
static gxml_data GXD = {
    1,          /* verb, default to 1 (0 means quiet)         */
    1,          /* dstore, flag whether to store data         */
    3,          /* indent, spaces per indent level            */
    GXML_DEF_BSIZE, /* buf_size, allocated for XML parsing    */
    GIFTI_B64_CHECK_SKIPNCOUNT, /* b64_check, for b64 errors  */
    1,          /* assume it is okay to update metadata       */
    GZ_DEFAULT_COMPRESSION, /* zlevel, compress level, -1..9  */

    NULL,       /* da_list, list of DA indices to store       */
    0,          /* da_len, length of da_list                  */
    0,          /* da_ind, current index into da_list         */

    0,          /* eleDA, number of DA elements found         */
    0,          /* expDA, number of DA elements expected      */
    0,          /* b64_errors, number of errors found         */
    0,          /* errors, number of encountered errors       */
    0,          /* skip depth (at positive depth, 0 is clear) */
    0,          /* depth, current stack depth                 */
    {0},        /* stack, ints, max depth GXML_MAX_DEPTH      */

    0,          /* dind, index into decoded data array        */
    0,          /* clen, length of current CDATA string       */
    0,          /* xlen, length of current xform buffer       */
    0,          /* dlen, length of current Data buffer        */
    0,          /* doff, offset into current data buffer      */
    0,          /* zlen, length of compression buffer         */
    NULL,       /* cdata, CDATA char pointer                  */
    NULL,       /* xdata, xform buffer pointer                */
    NULL,       /* ddata, Data buffer pointer                 */
    NULL,       /* zdata, compression buffer pointer          */
    NULL        /* gim, gifti_image *, for results            */
};

#ifndef HAVE_ZLIB  /* so we can print a callback message once per file */
    static int g_first_zlib_err_msg = 1;
#endif

/*--- Base64 binary encoding and decoding tables ---*/

/* encoding: converting values 0-63 to characters */
static unsigned char b64_encode_table[64] = {
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',             /* 26 upper case */
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',             /* 26 lower case */
    'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
    'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',   /* 10 digits */
    '+', '/'
};

/* decoding: converting characters A-Z, a-z, 0-9, +, /, to integers 0-63
 * (other characters are considered invalid, though '=' is mapped to 0,
 * to account for the end of input (didn't Arnold make a movie about that?))
 */
static unsigned char b64_decode_table[256] = {
    128, 128, 128, 128, 128, 128, 128, 128,   /*   0 -   7 */
    128, 128, 128, 128, 128, 128, 128, 128,   /*   8 -  15 */
    128, 128, 128, 128, 128, 128, 128, 128,   /*  16 -  23 */
    128, 128, 128, 128, 128, 128, 128, 128,   /*  24 -  31 */
    128, 128, 128, 128, 128, 128, 128, 128,   /*  32 -  39 */

    /* d['+'] = d[43] = 62,   d['/'] = d[47] = 63          */
    128, 128, 128,  62, 128, 128, 128,  63,   /*  40 -  47 */

    /* d['0'] = d[48] = 52,  ...   d['9'] = 61, d['='] = 0 */
     52,  53,  54,  55,  56,  57,  58,  59,   /*  48 -  55 */
     60,  61, 128, 128, 128,   0, 128, 128,   /*  56 -  63 */

    /* d['A'] = d[65] =  0,   ...   d['Z'] = 25            */
    128,   0,   1,   2,   3,   4,   5,   6,   /*  64 -  71 */
      7,   8,   9,  10,  11,  12,  13,  14,   /*  72 -  79 */
     15,  16,  17,  18,  19,  20,  21,  22,   /*  80 -  87 */
     23,  24,  25, 128, 128, 128, 128, 128,   /*  88 -  95 */

    /* d['a'] = d[97] = 26,   ...   d['a'] = 51            */
    128,  26,  27,  28,  29,  30,  31,  32,   /*  96 - 103 */
     33,  34,  35,  36,  37,  38,  39,  40,   /* 104 - 111 */
     41,  42,  43,  44,  45,  46,  47,  48,   /* 112 - 119 */
     49,  50,  51, 128, 128, 128, 128, 128,   /* 120 - 127 */

    /* ... and the rest, are heeere in deecode liiiiiist!  poor Mary Ann :( */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 128 - 135 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 136 - 143 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 144 - 151 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 152 - 159 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 160 - 167 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 168 - 175 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 176 - 183 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 184 - 191 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 192 - 199 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 200 - 207 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 208 - 215 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 216 - 223 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 224 - 231 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 232 - 239 */
    128, 128, 128, 128, 128, 128, 128, 128,   /* 240 - 247 */
    128, 128, 128, 128, 128, 128, 128, 128    /* 248 - 255 */
};

/* note: the buffer needs to be large enough to contain any contiguous
         piece of (CDATA?) text, o.w. it will require parsing in pieces */
gifti_image * gxml_read_image(const char * fname, int read_data,
                             const int * dalist, int dalen)
{
    gxml_data  * xd = &GXD;     /* point to global struct */
    XML_Parser   parser;
    unsigned     blen;
    FILE       * fp;
    char       * buf = NULL;
    int          bsize;    /* be sure it doesn't change at some point */
    int          done = 0, pcount = 1;

    if( init_gxml_data(xd, 0, dalist, dalen) ) /* reset non-user variables */
        return NULL;

    xd->dstore = read_data;  /* store for global access */

    if( !fname ) {
        fprintf(stderr,"** gxml_read_image: missing filename\n");
        return NULL;
    }

    fp = fopen(fname, "r");
    if( !fp ) {
        fprintf(stderr,"** failed to open GIFTI XML file '%s'\n", fname);
        return NULL;
    }

    /* create a new buffer */
    bsize = 0;
    if( reset_xml_buf(xd, &buf, &bsize) ) { fclose(fp); return NULL; }

    if(xd->verb > 1) {
        fprintf(stderr,"-- reading gifti image '%s'\n", fname);
        if(xd->da_list) fprintf(stderr,"   (length %d DA list)\n", xd->da_len);
        fprintf(stderr,"-- using %d byte XML buffer\n",bsize);
        if(xd->verb > 4) show_enames(stderr);
    }

    /* allocate return structure */
    xd->gim = (gifti_image *)calloc(1,sizeof(gifti_image));
    if( !xd->gim ) {
        fprintf(stderr,"** failed to alloc initial gifti_image\n");
        free(buf);
        return NULL;
    }

    /* create parser, init handlers */
    parser = init_xml_parser((void *)xd);

    while( !done )
    {
        if( reset_xml_buf(xd, &buf, &bsize) )  /* fail out */
            { gifti_free_image(xd->gim); xd->gim = NULL; break; }

        blen = fread(buf, 1, bsize, fp);
        done = blen < sizeof(buf);

        if(xd->verb > 3) fprintf(stderr,"-- XML_Parse # %d\n", pcount);
        pcount++;
        if( XML_Parse(parser, buf, blen, done) == XML_STATUS_ERROR) {
            fprintf(stderr,"** %s at line %u\n",
                    XML_ErrorString(XML_GetErrorCode(parser)),
                    (unsigned int)XML_GetCurrentLineNumber(parser));
            gifti_free_image(xd->gim);
            xd->gim = NULL;
            break;
        }
    }

    if(xd->verb > 1) {
        if(xd->gim)
            fprintf(stderr,"-- have gifti image '%s', "
                           "(%d DA elements = %lld MB)\n",
                    fname, xd->gim->numDA, gifti_gim_DA_size(xd->gim,1));
        else fprintf(stderr,"** gifti image '%s', failure\n", fname);
    }

    fclose(fp);
    if( buf ) free(buf);        /* parser buffer */
    XML_ParserFree(parser);

    if( dalist && xd->da_list )
        if( apply_da_list_order(xd, dalist, dalen) ) {
            fprintf(stderr,"** failed apply_da_list_order\n");
            gifti_free_image(xd->gim);
            xd->gim = NULL;
        }

    free_xd_data(xd);  /* free data buffers */

    return xd->gim;
}


/* free da_list and buffers */
static int free_xd_data(gxml_data * xd)
{
    if( xd->da_list ){ free(xd->da_list); xd->da_list = NULL; }

    if( xd->xdata ){ free(xd->xdata); xd->xdata = NULL; } /* xform matrix  */
    if( xd->zdata) { free(xd->zdata); xd->zdata = NULL; } /* compress buff */
    if( xd->ddata ){ free(xd->ddata); xd->ddata = NULL; } /* Data buffer   */

    return 0;
}


/* verify that the current lengths match
 * create a new list of NULL DA pointers
 * create a 'taken' list, matching xd->da_len (but unset)
 * for each index, if it is not set, find one and copy
 */
static int apply_da_list_order(gxml_data * xd, const int * orig, int len)
{
    giiDataArray ** newlist;
    int           * taken;      /* whether xd->da_list[i] was used */
    int             newc, oldc, errors = 0, nDA;

    if( !xd || !xd->gim || !orig || len <= 0 || !xd->da_list || xd->da_len<=0 )
        return 0;  /* do nothing, or start small, diversionary fire, hmmm... */

    nDA = xd->gim->numDA;       /* this may replace da_len */
    if( nDA <= 0 ) return 0;

    /* create new DA list */
    newlist = (giiDataArray **)malloc(len * sizeof(giiDataArray *));
    if(!newlist){ fprintf(stderr,"** ADLO: no alloc for DAlist\n"); return 1; }

    /* create taken list (of all 0) */
    taken = (int *)calloc(nDA, sizeof(int));
    if(!taken){ fprintf(stderr,"** ADLO: no alloc for taken\n"); return 1; }

    /* insert pointers to current or copied DA */
    for( newc = 0; newc < len; newc++ ) {
        newlist[newc] = NULL;      /* in case there are failures */

        /* find old index in da_list */
        for( oldc = 0; oldc < nDA; oldc++ )
            if( xd->da_list[oldc] == orig[newc] )
                break;
        if( oldc >= nDA ) {      /* should never happen */
            fprintf(stderr,"** ADLO: failed to find index %d in da_list\n",
                    orig[newc]);
            errors++;
            continue;
        } else if ( xd->verb > 3 )
            fprintf(stderr,"++ found orig index %d at sorted list %d\n",
                    orig[newc], oldc);

        /* either steal the pointer or copy the DA */
        if( !taken[oldc] ) {
            taken[oldc] = 1;    /* mark it as taken */
            newlist[newc] = xd->gim->darray[oldc];
        } else { /* we must make a new copy */
            newlist[newc] = gifti_copy_DataArray( xd->gim->darray[oldc], 1);
            if( ! newlist[newc] ) errors++;
        }
    }

    /* at this point, we should be good, but check...  */

    /* if anything has not been taken, free them and fail */
    for( oldc = 0; oldc < nDA; oldc++ ) {
        if( !taken[oldc] ) {
            fprintf(stderr,"** ADLO: taken list is not all set\n");
            errors++;
            free(xd->gim->darray[oldc]);
        }
    }
    free(taken);        /* we're done with that */

    /* free old DA list, since all the pointers are free'd or taken */
    free(xd->gim->darray);
    xd->gim->darray = newlist;
    xd->gim->numDA  = len;

    /* last check for errors, is the current darray filled? */
    for( newc = 0; newc < len; newc++ )
        if( ! xd->gim->darray[newc] ) {
            fprintf(stderr,"** ADLO: copied darray not full\n");
            errors++;
        }

    return errors;      /* we just need to free gim on return */
}


/* return 0 on success */
int gxml_write_image(gifti_image * gim, const char * fname, int write_data)
{
    gxml_data * xd = &GXD;     /* point to global struct */
    FILE      * fp;

    if( !gim ) {
        fprintf(stderr,"** GXML write: no gifti_image\n");
        return 1;
    } else if ( !fname ) {
        fprintf(stderr,"** GXML write: no filename\n");
        return 1;
    }

    if(GXD.verb > 1) {
        fprintf(stderr,"++ writing gifti image (%s data) to '%s'",
                write_data?"with":"no", fname);
        if( write_data )
            fprintf(stderr," (%d DA elements = %lld MB)",
                    gim->numDA, gifti_gim_DA_size(gim,1));
        fputc('\n', stderr);
    }

#ifndef HAVE_ZLIB  /* if the data is to be compressed, fail */
    if( gim->darray && gim->darray[0] &&
                       gim->darray[0]->encoding == GIFTI_ENCODING_B64GZ ) {
        fprintf(stderr,"** no ZLIB for compression...\n");
        gifti_set_atr_in_DAs(gim, "Encoding", "Base64Binary", NULL, 0);
    }
#endif

    init_gxml_data(xd, 0, NULL, 0);    /* reset non-user variables */
    xd->dstore = write_data;  /* store for global access */
    xd->gim = gim;

    /* note the library version */
    if( xd->update_ok ) {
        if( xd->verb > 2 )
            fprintf(stderr,"++ setting GIFTI MD: gifticlib-version to %s\n",
                    gifticlib_version());
        gifti_add_to_meta(&gim->meta,"gifticlib-version",gifticlib_version(),1);
    }

    fp = fopen(fname, "w");
    if( !fp ) {
        fprintf(stderr,"** failed to open '%s' for gifti write\n", fname);
        return 1;
    }

    (void)gxml_write_gifti(xd, fp);

    if( xd->xdata ){ free(xd->xdata);  xd->xdata = NULL; }
    if( xd->zdata) { free(xd->zdata);  xd->zdata = NULL; }

    fclose(fp);

    return 0;
}


/*---------- accessor functions for user-controllable variables ----------*/
/*----------     (one can pass -1 to set the default value)     ----------*/

/*! verb is the vebose level, with 0 meaning "only report errors" (up to 7) */
int gxml_get_verb( void    ){ return GXD.verb; }
int gxml_set_verb( int val )
{
    if     ( val == -1 ) GXD.verb = 1;
    else if( val >=  0 ) GXD.verb = val;
    return 0;
}

/*! the dstore flag controls whether data is read from a GIFTI file */
int gxml_get_dstore( void    ){ return GXD.dstore; }
int gxml_set_dstore( int val )
{
    if( val ) GXD.dstore = 1;   /* includes -1 as applying the default */
    else      GXD.dstore = 0;
    return 0;
}

/*! indent is the number of spaces per indent level written to a GIFTI file */
int gxml_get_indent( void    ){ return GXD.indent; }
int gxml_set_indent( int val )
{
    if      ( val == -1 ) GXD.indent = 3;
    else if ( val >=  0 ) GXD.indent = val;
    else return 1;  /* failure - no action */
    return 0;
}

/*! buf_size is the size of the XML I/O buffer given to expat */
int gxml_get_buf_size( void    ){ return GXD.buf_size; }
int gxml_set_buf_size( int val )
{
    if      ( val == -1 ) GXD.buf_size = GXML_DEF_BSIZE;
    else if ( val  >  0 ) GXD.buf_size = val;
    else return 1;      /* failure - no action */
    return 0;
}

/*! b64_check is the checking level for base64 binary data (see gifti_io.h) */
int gxml_get_b64_check( void    ){ return GXD.b64_check; }
int gxml_set_b64_check( int val ) /* -1 means apply default */
{
    if( val == -1 )
        GXD.b64_check = GIFTI_B64_CHECK_SKIPNCOUNT;
    else if ( val > GIFTI_B64_CHECK_UNDEF && val <= GIFTI_B64_CHECK_MAX )
        GXD.b64_check = val;
    else return 1;      /* failure - no action */
    return 0;
}

/*! update_ok tells the library whether it is allowed to modify metadata */
int gxml_get_update_ok( void    ){ return GXD.update_ok; }
int gxml_set_update_ok( int val )
{
    if      ( val == -1 ) GXD.update_ok = 1;    /* default is yes */
    else if ( val >=  0 ) GXD.update_ok = val;
    else return 1;  /* failure - no action */
    return 0;
}

/*! zlevel is the zlib compression level, with -1 implying the default
    (of 6, I believe) and value levels being in {0..9} (with 0 meaning
    no compression) */
int gxml_get_zlevel( void    ){ return GXD.zlevel; }
int gxml_set_zlevel( int val )
{
    if( val == -1 )
        GXD.zlevel = GZ_DEFAULT_COMPRESSION;
    else if ( val >= 0 && val <= 9 )
        GXD.zlevel = val;
    else return 1;      /* failure - no action */
    return 0;
}
/*----------------------- END accessor functions -----------------------*/


static int init_gxml_data(gxml_data *dp, int doall, const int *dalist, int len)
{
    int errs = 0;

    if( doall ) {           /* user modifiable - init all to defaults */
        dp->verb      = 1;
        dp->dstore    = 1;
        dp->indent    = 3;
        dp->buf_size  = GXML_DEF_BSIZE;
        dp->b64_check = GIFTI_B64_CHECK_SKIPNCOUNT;
        dp->update_ok = 1;
        dp->zlevel    = GZ_DEFAULT_COMPRESSION;
    }

    if( dalist && len > 0 ) {
        if( short_sorted_da_list(dp, dalist, len) ) errs++;  /* continue */
    } else {
        dp->da_list = NULL;
        dp->da_len  = 0;
    }
    dp->da_ind  = 0;

    /* maybe show the user */
    if( dp->verb > 2 )
        disp_gxml_data("-- user opts: ", dp, dp->verb > 3);

    dp->eleDA = 0;
    dp->expDA = 0;
    dp->b64_errors = 0;
    dp->errors = 0;
    dp->skip = 0;
    dp->depth = 0;
    memset(dp->stack, 0, sizeof(dp->stack));

    dp->dind = 0;
    dp->clen = 0;
    dp->xlen = 0;
    dp->dlen = 0;
    dp->doff = 0;
    dp->zlen = 0;
    dp->cdata = NULL;
    dp->xdata = NULL;
    dp->ddata = NULL;
    dp->zdata = NULL;
    dp->gim   = NULL;

#ifndef HAVE_ZLIB  /* if we don't have this (and need it), print warnings */
    g_first_zlib_err_msg = 1;
#endif

    return errs;
}

static int short_sorted_da_list(gxml_data *dp, const int * dalist, int len)
{
    int * da_copy, c, cind;

    /* first, duplicate list */
    da_copy = (int *)malloc(len*sizeof(int));
    if( !da_copy ) {
        fprintf(stderr,"** cannot duplicate da_list of %d elements\n", len);
        return 1;
    }
    for( c = 0; c < len; c++ ) da_copy[c] = dalist[c];

    /* now sort */
    qsort(da_copy, len, sizeof(int), int_compare);

    /* remove duplicates */
    for( c = 1, cind = 0; c < len; c++ ) {
        if( da_copy[c] != da_copy[cind] ) {
            cind++;
            if( cind < c ) da_copy[cind] = da_copy[c];
        }
    }

    dp->da_list = da_copy;
    dp->da_len = cind+1;

    if( dp->verb > 1 ) {
        fprintf(stderr,"-- original da_list:");
        for(c = 0; c < len; c++ )
            fprintf(stderr," %d", dalist[c]);
        fputc('\n', stderr);
        fprintf(stderr,"++ unique, sorted da_list:");
        for(c = 0; c < dp->da_len; c++ )
            fprintf(stderr," %d", dp->da_list[c]);
        fputc('\n', stderr);
    }

    return 0;
}

/* for qsort */
static int int_compare(const void * v0, const void * v1)
{
    int * i0 = (int *)v0;
    int * i1 = (int *)v1;

    if( *i0  < *i1 ) return -1;
    if( *i0 == *i1 ) return  0;
    return 1;
}

/* ---------------------------------------------------------------------- */

static void show_depth( int depth, int show, FILE * fp )
{
    if( show ) fprintf(fp, "%*s %02d ", 3*depth, "", depth);
    else       fprintf(fp, "%*s    ", 3*depth, "");
}

static void show_enames( FILE * fp )
{
    int c;
    fprintf(fp, "-------------------------------\n"
                "++ ename list :\n");
    for( c = 0; c <= GXML_ETYPE_LAST; c++ )
        fprintf(fp,"    %02d : %s\n", c, enames[c]);
    fprintf(fp, "-------------------------------\n");
}

static int ename2type( const char * name )
{
    int etype;
    for( etype = GXML_ETYPE_LAST; etype > GXML_ETYPE_INVALID; etype-- )
        if( !strcmp(name, enames[etype]) )
            break;
    return etype;
}

/* name should be null terminated */
static int epush( gxml_data * xd, int etype, const char * ename,
                                                  const char ** attr )
{
    if( xd->depth < 0 || xd->depth > GXML_MAX_DEPTH ) {
        fprintf(stderr,"** push: stack depth %d out of [0,%d] range\n",
                xd->depth, GXML_MAX_DEPTH);
        xd->errors++;
        return 1;
    }

    if( xd->verb > 4 ) {       /* maybe we want to print something */
        show_depth(xd->depth, 1, stderr);
        fprintf(stderr,"++ push %02d: '%s'\n", etype, enames[etype]);
    }

    xd->stack[xd->depth] = etype;
    xd->depth++;

    /* if we are in a skip block, do nothing but monitor stack */
    if( xd->skip ) {
        if( xd->verb > 2 )
            fprintf(stderr,"-- skip=%d, depth=%d, skipping element '%s'\n",
                    xd->skip, xd->depth, ename);
        return 0;
    }

    /* determine whether we should enter a skip block */
    if( etype == GXML_ETYPE_INVALID ) {
        if(xd->verb > 0)
            fprintf(stderr,"** pushed invalid element, '%s', skip depth %d\n",
                    ename, xd->depth);
        xd->skip = xd->depth;
        return 1;
    }

    if ( xd->verb > 5 ) show_stack("++ ", xd);
    if( !stack_is_valid(xd) ) return 1;

    /* call appropriate XML processing function */
    switch( etype ) {
        case GXML_ETYPE_GIFTI      : return push_gifti (xd, attr);
        case GXML_ETYPE_META       : return push_meta  (xd);
        case GXML_ETYPE_MD         : return push_md    (xd);
        case GXML_ETYPE_NAME       : return push_name  (xd);
        case GXML_ETYPE_VALUE      : return push_value (xd);
        case GXML_ETYPE_LABELTABLE : return push_LT    (xd);
        case GXML_ETYPE_LABEL      : return push_label (xd, attr);
        case GXML_ETYPE_DATAARRAY  : return push_darray(xd, attr);
        case GXML_ETYPE_CSTM       : return push_cstm  (xd);
        case GXML_ETYPE_DATA       : return push_data  (xd);
        case GXML_ETYPE_DATASPACE  : return push_dspace(xd);
        case GXML_ETYPE_XFORMSPACE : return push_xspace(xd);
        case GXML_ETYPE_MATRIXDATA : return push_xform (xd);
        case GXML_ETYPE_CDATA      : return 0;  /* do nothing */
        default:
            fprintf(stderr,"** epush, unknow type '%s'\n",enames[etype]);
            break;
    }

    return 1;
}

/* initialize the gifti_element and set attributes */
static int push_gifti(gxml_data * xd, const char ** attr )
{
    gifti_image *     gim;
    int               c;

    if( !xd ) return 1;

    /* be explicit with pointers (struct should be clear) */
    gim = xd->gim;
    gifti_clear_gifti_image(gim);

    if( !attr ) return 0;

    for(c = 0; attr[c]; c+= 2 )
        if( gifti_str2attr_gifti(gim, attr[c], attr[c+1]) )
            if( gifti_add_to_nvpairs(&gim->ex_atrs,attr[c],attr[c+1]) )
                return 1;

    if( xd->verb > 1 ) fprintf(stderr,"++ set %d GIFTI attr(s)\n",c/2);
    if( xd->verb > 3 ) gifti_disp_gifti_image("push:", gim, 0);

    /* now store any gim->numDA, and use gim to count as they are added */
    if( gim->numDA >= 0 ) {
        xd->expDA = gim->numDA;
        gim->numDA = 0;  /* clear for counting */
        if( xd->verb > 1 )
            fprintf(stderr,"-- expecting %d DA elements\n", xd->expDA);
    }

    return 0;
}

/* simply verify that we have not been here before */
static int push_meta(gxml_data * xd)
{
    giiMetaData * md = find_current_MetaData(xd, 0);

    if( md->length != 0 || md->name || md->value ) {
        fprintf(stderr,"** push meta: already initialized??\n");
        return 1;
    }

    return 0;
}

/* find the parent struct, and return its meta field */
static giiMetaData * find_current_MetaData(gxml_data * xd, int cdepth)
{
    giiDataArray * da;
    giiMetaData  * md;
    int            da_ind, parent;

    if( !xd || cdepth < 0 || xd->depth < (2+cdepth) ) {
        fprintf(stderr,"FMeta: bad params (%p,%d)\n",(void *)xd,cdepth);
        return NULL;
    }

    /* find the appropriate parent struct */
    parent = xd->stack[xd->depth-2-cdepth];
    if( parent == GXML_ETYPE_GIFTI )
        md = &xd->gim->meta;
    else if( parent == GXML_ETYPE_DATAARRAY ) {
        if( !xd->gim->darray ) {
            fprintf(stderr,"** FMeta: gim->darry not initialized\n");
            return NULL;
        }
        da_ind = xd->gim->numDA-1;
        da = xd->gim->darray[da_ind];
        if( !da ) {
            fprintf(stderr,"** FMeta: gim->darry[%d] not initialized\n",da_ind);
            return NULL;
        }
        md = &da->meta;
    } else {
        fprintf(stderr,"** FMeta: child of invalid parent '%s'\n",
                enames[parent]);
        return NULL;
    }

    return md;
}

/* we will add a pair, so update length and allocate pointers */
static int push_md(gxml_data * xd)
{
    giiMetaData * md = find_current_MetaData(xd, 1);  /* MD is 1 below */

    if( !md ) return 1;  /* error were printed */

    md->length++;
    md->name = (char **)realloc(md->name, md->length * sizeof(char *));
    md->value = (char **)realloc(md->value, md->length * sizeof(char *));

    if( !md->name || !md->value ) {
        fprintf(stderr,"** failed to realloc %d MD pointers\n",md->length);
        md->length = 0;
        return 1;
    }

    /* and clear the new pointers */
    md->name[md->length-1] = NULL;
    md->value[md->length-1] = NULL;

    return 0;
}

/* set cdata to the current meta->name address, and clear it */
static int push_name(gxml_data * xd)
{
    giiMetaData * md = find_current_MetaData(xd, 2);  /* name is 2 below */

    if( !md ) return 1;

    xd->cdata = &md->name[md->length-1];  /* use cdata to fill */
    *xd->cdata = NULL;                    /* init to empty */
    xd->clen = 0;

    return 0;
}

/* set cdata to the current meta->value address, and clear it */
static int push_value(gxml_data * xd)
{
    giiMetaData * md = find_current_MetaData(xd, 2);  /* value is 2 below */

    if( !md ) return 1;

    xd->cdata = &md->value[md->length-1];  /* use cdata to fill */
    *xd->cdata = NULL;                     /* init to empty */
    xd->clen = 0;

    return 0;
}

/* check that LT is currently empty */
static int push_LT(gxml_data * xd)
{
    giiLabelTable * lt = &xd->gim->labeltable;

    if( lt->length || lt->key || lt->label ) {
        fprintf(stderr,"** multiple giiLabelTables?\n");
    }

    return 0;
}

/* increase LabelTable length by 1, and fill new entries
 * (note that the Key (or Index) attribute is required)
 *
 * 'Index' attribute has been replaced by 'Key'    7 Mar 2010 */
static int push_label(gxml_data * xd, const char ** attr)
{
    giiLabelTable * lt = &xd->gim->labeltable;
    float           rgba[4]={0.0, 0.0, 0.0, 0.0};
    int             key=0, rv;

    lt->length++;
    lt->key = (int *)realloc(lt->key, lt->length * sizeof(int));
    lt->label = (char **)realloc(lt->label, lt->length * sizeof(char *));

    if( !lt->key || !lt->label ) {
        fprintf(stderr,"** gifti alloc failure for label %d\n",lt->length);
        return 1;
    }

    /* set key from the attributes */
    if( !attr ) {
        fprintf(stderr,"** Label %d missing attributes\n", lt->length-1);
        lt->key[lt->length-1] = 0;
    } else {
        /* get any known attributes */
        rv = get_label_attrs(xd, attr, &key, rgba);
        if ( rv == 1 ) {
           lt->key[lt->length-1] = key;
        } else if ( rv == 2 ) {
           lt->key[lt->length-1] = key;
           (void) add_label_rgba(xd, lt, rgba);
        } /* else error already printed */
    }

    xd->cdata = lt->label + (lt->length-1); /* addr of newest (char *) */
    *xd->cdata = NULL;                      /* init to empty */
    xd->clen = 0;

    return 0;
}


/* add the rgba entries to the LabelTable, length is already updated */
static int add_label_rgba(gxml_data * xd, giiLabelTable * lt, float * rgba)
{
    if( !xd || !lt || !rgba ) {
        fprintf(stderr,"** add_label_rgba, bad params\n");
        return 1;
    }

    if( lt->length > 1 && !lt->rgba ) {
        fprintf(stderr,"** first RGBA at Label %d, so table is incomplete\n",
                       lt->length-1);
        return 1;
    }

    lt->rgba = (float *)realloc(lt->rgba, lt->length * 4 * sizeof(float));
    if( !lt->rgba ) {
        fprintf(stderr,"** failed to malloc rgba of length %d\n", lt->length);
        return 1;
    }

    memcpy(lt->rgba + 4*(lt->length-1), rgba, 4*sizeof(float));

    if(xd->verb > 4)
        fprintf(stderr,"-- adding Label RGBA %g %g %g %g\n",
                   lt->rgba[0], lt->rgba[1], lt->rgba[2], lt->rgba[3]);

    return 0;
}

/* get key and RGBA attributes, if they exist
 * return 1 if key, 2 if key+rgba, 0 if neither, -1 on error */
static int get_label_attrs(gxml_data * xd, const char ** attr, int * key,
                           float * rgba)
{
    giiLabelTable  * lt = &xd->gim->labeltable;
    const char    ** aptr;
    char           * endp;  /* for verifying float read */
    int              found, lind;

    if( !xd || !key || !rgba ) {
        fprintf(stderr,"** GLA: missing params\n");
        return -1;
    }
    if( !attr || !*attr ) return 0;

    /* note label index */
    lind = lt->length - 1;

    found = 0;  /* bitmask, key,R,G,B,A (in 0..31, should end as 1 or 31) */
    for( aptr = attr; *aptr ; aptr += 2 ) {
        if( !aptr[1] ) {
            fprintf(stderr,"** label %d, attr %s, missing value\n",lind,*aptr);
            return -1;
        }
        if( !strcmp(*aptr, "Key") ) {
            *key = atoi(aptr[1]);
            found |= (1<<0);
        }
        else if( !strcmp(*aptr, "Index") ) { /* old form of Key */
            *key = atoi(aptr[1]);
            found |= (1<<0);
        }
        else if( !strcmp(*aptr, "Red") ) {
            rgba[0] = strtod(aptr[1], &endp);
            if( endp <= aptr[1] ) {
                fprintf(stderr,"** bad GIFTI label %d Red attr\n", lind);
                show_attrs(xd, GXML_ETYPE_LABEL, attr);
                return -1;
            }
            found |= (1<<1);
        }
        else if( !strcmp(*aptr, "Green") ) {
            rgba[1] = strtod(aptr[1], &endp);
            if( endp <= aptr[1] ) {
                fprintf(stderr,"** bad GIFTI label %d Green attr\n", lind);
                show_attrs(xd, GXML_ETYPE_LABEL, attr);
                return -1;
            }
            found |= (1<<2);
        }
        else if( !strcmp(*aptr, "Blue") ) {
            rgba[2] = strtod(aptr[1], &endp);
            if( endp <= aptr[1] ) {
                fprintf(stderr,"** bad GIFTI label %d Blue attr\n", lind);
                show_attrs(xd, GXML_ETYPE_LABEL, attr);
                return -1;
            }
            found |= (1<<3);
        }
        else if( !strcmp(*aptr, "Alpha") ) {
            rgba[3] = strtod(aptr[1], &endp);
            if( endp <= aptr[1] ) {
                fprintf(stderr,"** bad GIFTI label %d Alpha attr\n", lind);
                show_attrs(xd, GXML_ETYPE_LABEL, attr);
                return -1;
            }
            found |= (1<<4);
        } else {
            fprintf(stderr,"** unknown GIFTI label %d attr\n", lind);
            show_attrs(xd, GXML_ETYPE_LABEL, attr);
            return -1;
        }
    }

    if( found == 0 ) {
        fprintf(stderr,"** GIFTI label %d, missing 'Key' attr\n", lind);
        return 0;
    } else if( found != 1 && found != 31 ) {
        fprintf(stderr,"** GIFTI label %d, partial attributes\n", lind);
        show_attrs(xd, GXML_ETYPE_LABEL, attr);
        return -1;
    }

    if(xd->verb > 2) {
        if( found == 1 ) fprintf(stderr,"-- have Label Key %d\n", *key);
        else fprintf(stderr,"-- have Label Key %d, RGBA %g %g %g %g\n",
                     *key, rgba[0], rgba[1], rgba[2], rgba[3]);
    }

    if( found == 1 ) return 1;
    return 2;
}


/* initialize the gifti_element and set attributes */
static int push_darray(gxml_data * xd, const char ** attr)
{
    giiDataArray * da;

    /* maintain a count of the number seen */
    xd->eleDA++;

    if( xd->da_list ) {
        if( (xd->da_ind < xd->da_len) &&
            (xd->da_list[xd->da_ind] == xd->eleDA-1) )    /* keeper */
        {
            if(xd->verb > 1) fprintf(stderr,"++ keeping DA[%d]\n",xd->eleDA-1);
            xd->da_ind++;
        } else {
            if(xd->verb > 1) fprintf(stderr,"++ skipping DA[%d]\n",xd->eleDA-1);
            xd->skip = xd->depth;
            return 1;   /* return and skip this element */
        }
    }

    if( gifti_add_empty_darray(xd->gim, 1) ) return 1;

    da = xd->gim->darray[xd->gim->numDA-1];  /* get new pointer */

    /* fill the struct from the attributes (store ex_atrs) */
    if( gifti_set_DA_atrs(da, attr, 0, 1) ) return 1;
    (void)gifti_valid_DataArray(da, xd->verb > 1);

    /* make a request to potentially update the XML buffer size */
    if( da->nvals>0 && da->nbyper>0 )
        update_xml_buf_size(xd, da->nvals*da->nbyper);

    if( xd->verb > 4 ) gifti_disp_DataArray("push:", da, 0);

    return 0;
}

/* check for base64 errors, needed uncompression, and byte swapping */
static int pop_darray(gxml_data * xd)
{
    giiDataArray * da = xd->gim->darray[xd->gim->numDA-1]; /* current DA */

    if( !da ) return 1;

    /* check for and clear any b64 errors */
    if( xd->b64_errors > 0 ) {
        if( xd->b64_check == GIFTI_B64_CHECK_DETECT )
            fprintf(stderr,"** bad base64 chars found in DataArray[%d]\n",
                    xd->gim->numDA-1);
        else if( xd->b64_check == GIFTI_B64_CHECK_COUNT ||
                 xd->b64_check == GIFTI_B64_CHECK_SKIPNCOUNT )
            fprintf(stderr,"** %d bad base64 chars found in DataArray[%d]\n",
                    xd->b64_errors, xd->gim->numDA-1);
        xd->b64_errors = 0;
    }

    if( da->encoding == GIFTI_ENCODING_B64GZ && da->data ) {
#ifdef HAVE_ZLIB   /* for compiling, higher level test elsewhere */
        long long olen;  /* to avoid warnings printing outlen */
        uLongf    outlen = da->nvals*da->nbyper;
        int       rv = 0;

        /* unzip zdata to da->data */

        if( xd->verb > 2 )
            fprintf(stderr,"-- uncompressing %lld bytes into %lld\n",
                           xd->dind, (long long)outlen);

        rv = uncompress(da->data, &outlen, (Bytef*)xd->zdata, xd->dind);
        olen = outlen;

        if( rv != Z_OK ) {
            fprintf(stderr,"** uncompress fails for DA[%d]\n",xd->gim->numDA-1);
            if( rv == Z_MEM_ERROR )
                fprintf(stderr,"   (zlib failure, not enough memory)\n");
            else if ( rv == Z_BUF_ERROR )
                fprintf(stderr,"   (zlib failure, output buffer too short)\n");
            else if ( rv == Z_DATA_ERROR )
                fprintf(stderr,"   (zlib failure, corrupted data)\n");
            else if ( rv != Z_OK )
                fprintf(stderr,"   (zlib failure, unknown error %d)\n", rv);
        } else if ( xd->verb > 2 || (xd->verb > 1 && xd->gim->numDA == 1 ))
            fprintf(stderr,"-- uncompressed buffer (%.2f%% of %lld bytes)\n",
                    100.0*xd->dind/olen, olen);

        if( olen != da->nvals*da->nbyper ) {
            fprintf(stderr,"** uncompressed buf is %lld bytes, expected %lld\n",
                    olen, da->nvals*da->nbyper);
        }
#endif

        xd->gim->compressed = 1;   /* flag whether some data was compressed */
    }

    /* possibly read data from an external file */
    if( da->ext_fname && *da->ext_fname )
        (void)gifti_read_extern_DA_data(da); /* nothing to do on failure */

    /* possibly perform byte-swapping on data */
    if( da->data && da->encoding != GIFTI_ENCODING_ASCII ) {
        long long nvals;
        int       swapsize;

        gifti_datatype_sizes(da->datatype, NULL, &swapsize);
        if( swapsize <= 0 ) {
            fprintf(stderr,"** bad swapsize %d for dtype %d\n",
                    swapsize, da->datatype);
            return 1;
        }

        nvals = da->nvals * da->nbyper / swapsize;
        if( gifti_check_swap(da->data, da->endian, nvals, swapsize) )
            xd->gim->swapped = 1;       /* flag that it happened */
    }

    return 0;
}


/* make space for a new CS structure in the current DataArray */
static int push_cstm(gxml_data * xd)
{
    giiDataArray * da = xd->gim->darray[xd->gim->numDA-1]; /* current DA */

    if( da->intent != NIFTI_INTENT_POINTSET && xd->verb > 0 )
        fprintf(stderr,"** DA[%d] has coordsys with intent %s (should be %s)\n",
                xd->gim->numDA-1, gifti_intent_to_string(da->intent),
                gifti_intent_to_string(NIFTI_INTENT_POINTSET));

    if( gifti_add_empty_CS(da) ) return 1;

    return 0;
}

/* verify the processing buffer space, alloc data space */
static int push_data(gxml_data * xd)
{
    giiDataArray * da = xd->gim->darray[xd->gim->numDA-1]; /* current DA */
    int            zsize;

    xd->dind = 0;       /* init for filling */
    xd->doff = 0;

    if( ! xd->dstore ) {
        if( xd->verb > 3 )
            fprintf(stderr,"-- skipping data[%d]\n",xd->gim->numDA-1);
        xd->skip = xd->depth;
        return 1;
    }

    if( update_partial_buffer(&xd->ddata, &xd->dlen, da->nbyper*da->nvals, 0) )
        return 1;

    if( da->encoding == GIFTI_ENCODING_B64GZ ) {

#ifndef HAVE_ZLIB  /* we don't know the encoding until push_darray */
        if( g_first_zlib_err_msg ) {
            fprintf(stderr,"** no ZLIB: skipping all compressed data\n");
            g_first_zlib_err_msg = 0;
        }
        xd->skip = xd->depth;
        return 1;   /* return and skip this element */
#endif

        zsize = da->nbyper*da->nvals * 1.01 + 12; /* zlib.net */

        if( xd->verb > 2 )
            fprintf(stderr,"++ creating extra zdata for zlib extraction\n");
        if( update_partial_buffer(&xd->zdata, &xd->zlen, zsize, 1) )
            return 1;
    }

    /* allocate space for data */
    if( da->nvals <= 0 || da->nbyper <= 0 ) {
        fprintf(stderr,"** PD: bad vals,bytes = %u, %d\n",
                (unsigned)da->nvals,da->nbyper);
        return 1;
    }

    da->data = calloc(da->nvals, da->nbyper);
    if( ! da->data ) {
        fprintf(stderr,"** PD: failed to alloc %lld bytes for darray[%d]\n",
                da->nvals*da->nbyper, xd->gim->numDA-1);
        return 1;
    } else if ( xd->verb > 3 )
        fprintf(stderr,"++ PD: alloc %lld bytes for darray[%d]\n",
                da->nvals*da->nbyper, xd->gim->numDA-1);

    return 0;
}

/* point cdata to the correct location and init */
static int push_dspace(gxml_data * xd)
{
    int CSind = xd->gim->darray[xd->gim->numDA-1]->numCS-1;
    if( CSind < 0 ) {
        fprintf(stderr,"** PD: bad numCS %d in darray %d, skipping...",
                CSind+1, xd->gim->numDA-1);
        xd->skip = xd->depth;
        return 1;
    }

    if( !xd->gim->darray[xd->gim->numDA-1]->coordsys[CSind] ) {
        fprintf(stderr,"** found dataspace without coordsys, skipping...\n");
        xd->skip = xd->depth;
        return 1;
    }

    xd->cdata = &xd->gim->darray[xd->gim->numDA-1]->coordsys[CSind]->dataspace;
    *xd->cdata = NULL;                      /* init to empty */
    xd->clen = 0;
    return 0;
}

/* point cdata to the correct location and init */
static int push_xspace(gxml_data * xd)
{
    int CSind = xd->gim->darray[xd->gim->numDA-1]->numCS-1;
    if( CSind < 0 ) {
        fprintf(stderr,"** PX: bad numCS %d in darray %d, skipping...",
                CSind+1, xd->gim->numDA-1);
        xd->skip = xd->depth;
        return 1;
    }

    if( !xd->gim->darray[xd->gim->numDA-1]->coordsys[CSind] ) {
        fprintf(stderr,"** found xformspace without coordsys, skipping...\n");
        xd->skip = xd->depth;
        return 1;
    }

    xd->cdata = &xd->gim->darray[xd->gim->numDA-1]->coordsys[CSind]->xformspace;
    *xd->cdata = NULL;                      /* init to empty */
    xd->clen = 0;
    return 0;
}

/* verify the processing buffer space */
static int push_xform(gxml_data * xd)
{
    int CSind = xd->gim->darray[xd->gim->numDA-1]->numCS-1;
    if( CSind < 0 ) {
        fprintf(stderr,"** PXform: bad numCS %d in darray %d, skipping...",
                CSind+1, xd->gim->numDA-1);
        xd->skip = xd->depth;
        return 1;
    }

    if( !xd->gim->darray[xd->gim->numDA-1]->coordsys[CSind] ) {
        fprintf(stderr,"** found xform without coordsys, skipping...\n");
        xd->skip = xd->depth;
        return 1;
    }

    /* just make sure we have a text buffer to work with */
    if( !xd->xdata || xd->xlen <= 0 ) {
        xd->xlen = 2048;
        xd->xdata = (char *)malloc(xd->xlen * sizeof(char));
        if( !xd->xdata ) {
            fprintf(stderr,"** cannot alloc %d bytes for xform\n",xd->xlen);
            return 1;
        }
    }

    xd->dind = 0;       /* init for filling */
    xd->doff = 0;

    return 0;
}

static int epop( gxml_data * xd, int etype, const char * ename )
{
    giiDataArray * da;

    xd->cdata = NULL;                   /* clear fields for future use */
    xd->clen = 0;

    if( xd->skip == xd->depth ) {       /* just completed skip element */
        if( xd->verb > 2 )
            fprintf(stderr,"-- popping skip element '%s' at depth %d\n",
                    ename, xd->depth);
        xd->skip = 0;  /* clear skip level */
    } else {    /* may peform pop action for this element */
        switch( etype ) {
            default: /* do nothing special */
                break;
            case GXML_ETYPE_DATA :
                if(xd->verb>3)fprintf(stderr,"-- data dind = %lld\n",xd->dind);
                /* if we have not read data, but allocated for it, free */
                da = xd->gim->darray[xd->gim->numDA-1];
                if( da->data && xd->dind == 0 ) {
                    if( xd->verb > 3 ) fprintf(stderr,"   (freeing data)\n");
                    free(da->data);
                    da->data = NULL;
                }
                break;

            case GXML_ETYPE_DATAARRAY  :
                pop_darray(xd);
                break;

            case GXML_ETYPE_GIFTI      :
                if(xd->eleDA != xd->expDA)
                    fprintf(stderr,"** found %d DAs, expected %d\n",
                            xd->eleDA, xd->expDA);
                else if(xd->da_list && (xd->da_len != xd->da_ind))
                    fprintf(stderr,"** stored %d DAs, wanted %d\n",
                            xd->da_ind, xd->da_len);
                if(xd->verb > 2)
                    gifti_disp_gifti_image("pop:", xd->gim, xd->verb > 4);

                if(xd->verb > 1) {      /* check flags */
                    if(xd->gim->swapped)
                        fprintf(stderr,"++ data was byte-swapped\n");
                    if(xd->gim->compressed)
                        fprintf(stderr,"++ data was compressed\n");
                }

                break;
        }
    }

    xd->depth--;

    if( xd->verb > 5 )
    {
        show_depth(xd->depth, 1, stderr);
        fprintf(stderr,"++ pop %02d : '%s'\n", etype, enames[etype]);
    }

    if( xd->depth < 0 || xd->depth > GXML_MAX_DEPTH ) {
        fprintf(stderr,"** pop: stack depth %d out of [0,%d] range\n",
                xd->depth, GXML_MAX_DEPTH);
        xd->errors++;
        return -1;
    }

    return 0;
}

/* return the number of bytes of leading whitespace, up to a max of len */
static int whitespace_len(const char * str, int len)
{
    int c;
    if( !str || !*str || len < 1 ) return 0;
    for( c = 0; c < len; c++ )
        if( !isspace(str[c]) ) return c;

    return len;
}

static void show_attrs(gxml_data * xd, int etype, const char ** attr)
{
    int count;
    show_depth(xd->depth, 1, stderr);
    fprintf(stderr, ": element %s\n", enames[etype]);
    for( count = 0; attr[count]; count += 2 ){
        show_depth(xd->depth, 0, stderr);
        fprintf(stderr,"      attr: %s='%s'\n", attr[count], attr[count+1]);
    }
}


static void XMLCALL cb_start_ele(void *udata, const char *ename,
                                              const char **attr)
{
    gxml_data * xd = (gxml_data *)udata;
    int         etype;

    etype = ename2type(ename);
    if( xd->verb > 3 ) show_attrs(xd, etype, attr);

    /* process attributes and push() */

    (void)epush(xd, etype, ename, attr);
}

/* if ending Data, clear prev_end_check */
static void XMLCALL cb_end_ele(void *udata, const char * ename)
{
    gxml_data * xd = (gxml_data *)udata;

    epop(xd, ename2type(ename), ename);
}

/* May divide Data, but apparently not attributes, perhaps because
   the Data section is longer than the buffer is wide.

   if in Data:
        if prev_end_check
            if( ! char_is_whitespace(first) && concat_is_number() )
                concatencate as number to adjust previous number
                verify rest is whitespace
                return  (we don't expect to start a new number)
            else
                apply previous number
        if( !char_is_whitespace(last) )
            store trailing non-space in concat_buf
        else
            prev_end_check = 0
        apply number (though it may change later)
*/
static void XMLCALL cb_char(void *udata, const char * cdata, int length)
{
    gxml_data  * xd = (gxml_data *)udata;
    const char * str = cdata;
    int          len = length, wlen = 0, parent;

    if( xd->skip > 0 ) {
        if(xd->verb > 3) fprintf(stderr,"-- skipping char [%d]\n",len);
        return;
    }

    /* act based on the parent type */
    parent = xd->stack[xd->depth-1];
    if( parent == GXML_ETYPE_CDATA ) parent = xd->stack[xd->depth-2];

    if( parent != GXML_ETYPE_DATA ) wlen = whitespace_len(str,length);

    switch( parent ) {
        case GXML_ETYPE_DATA       :
            (void)append_to_data(xd, cdata, length);
            break;
        case GXML_ETYPE_MATRIXDATA :
            (void)append_to_xform(xd, cdata, length);
            break;

        case GXML_ETYPE_GIFTI      :
        case GXML_ETYPE_META       :
        case GXML_ETYPE_MD         :
        case GXML_ETYPE_LABELTABLE :
        case GXML_ETYPE_DATAARRAY  :
        case GXML_ETYPE_CSTM       :
            if( wlen != length && xd->verb ) {
                fprintf(stderr,"** invalid chars under %s: '%.*s'\n",
                        enames[parent], length, cdata);
            }
            break;

        case GXML_ETYPE_NAME       :
        case GXML_ETYPE_VALUE      :
        case GXML_ETYPE_LABEL      :
        case GXML_ETYPE_DATASPACE  :
        case GXML_ETYPE_XFORMSPACE :
            if( xd->verb > 4 )
                fprintf(stderr,"++ append cdata, parent %s\n",enames[parent]);
            (void)append_to_cdata(xd, cdata, length);
            break;

        case GXML_ETYPE_CDATA      :
            fprintf(stderr,"** CDATA is the parent of CDATA???\n");
            return;

        default: /* drop through */
            fprintf(stderr,"** unknown parent of char: %d\n", parent);
            return;
    }

    if( wlen == length ) {      /* if it is all whitespace */
        if( xd->verb < 5 ) return;
        str = "whitespace";     /* just note the whitespace */
        len = strlen(str);
    }

    if( xd->verb > 4 ) {
        show_depth(xd->depth, 1, stderr);
        if( parent == GXML_ETYPE_DATA && len > 40 ) len = 40;
        fprintf(stderr, "char[%d]: %.*s\n", length, len, str);
    }
}


/* ----------------------------------------------------------------------
 * xd->cdata points to one of:
 *      md->name[k], md->value[k], lt->label[k],
 *      da[k]->coordsys->dataspace, da[k]->coordsys->xformspace
 *
 * append the new data and null terminate
 * ---------------------------------------------------------------------- */
static int append_to_cdata(gxml_data * xd, const char * cdata, int len)
{
    int offset;
    if( !xd || !cdata || len <= 0 ) {
        fprintf(stderr,"** A2CD, bad params (%p,%p,%d)\n",
                (void *)xd,(void *)cdata, len);
        return 1;
    }
    if( !*xd->cdata ) {
        offset = 0;
        xd->clen = len + 1;  /* first time, alloc for null */
    }
    else {
        offset = xd->clen - 1;
        xd->clen += len;
    }

    if( xd->verb > 4 )
        fprintf(stderr,"++ a2cdata, len %d, clen %d, data '%.*s'\n",
                len, xd->clen, len, cdata);

    *xd->cdata = (char *)realloc(*xd->cdata, xd->clen*sizeof(char));
    if(!*xd->cdata) {
        fprintf(stderr,"** A2CD, failed to realloc %d bytes\n",xd->clen);
        return 1;
    }

    memcpy(*xd->cdata + offset, cdata, len);    /* append the new data */
    (*xd->cdata)[xd->clen-1] = '\0';            /* and null terminate */

    return 0;
}


/* this must go to the data of the latest darray struct */
static int append_to_data(gxml_data * xd, const char * cdata, int len)
{
    giiDataArray * da = xd->gim->darray[xd->gim->numDA-1]; /* current DA */

    if( !da || !xd->dlen || !xd->ddata || xd->dind < 0 ) {
        fprintf(stderr,"** A2D: bad setup (%p,%d,%p,%lld)\n",
                (void *)da, xd->dlen, (void *)xd->ddata, xd->dind);
        return 1;
    } else if( !da->data ) {
        fprintf(stderr,"** A2D: no data allocated\n");
        return 1;
    }

    switch( da->encoding ){
        case GIFTI_ENCODING_ASCII:
            return append_to_data_ascii(xd, cdata, len);

        case GIFTI_ENCODING_B64BIN:
            return append_to_data_b64(xd, (char *)da->data,
                                      da->nvals*da->nbyper, cdata, len);
        case GIFTI_ENCODING_B64GZ:
            return append_to_data_b64(xd, xd->zdata, xd->zlen, cdata, len);

        default:
            fprintf(stderr,"** A2D: invalid encoding value %d (%s)\n",
                    da->encoding,
                    gifti_list_index2string(gifti_encoding_list,da->encoding));
            return 1;
    }
}


/* decode the b64 data, inserting it into da->data
 *
 * use intermediate buffer (ddata), length dlen+1, reprocess length doff
 * ddata format, in bytes per section:
 *       +------+----------+--------+---+
 *       | doff | copy_len | unused | 1 |  (last 1 assures null termination)
 *       +------+----------+--------+---+
 *
 * - while there are bytes left to process (rem_bytes_in > 0)
 *       copy_len = bytes to process now
 *       copy that many bytes to ddata
 *       rem_bytes_out = number of output bytes left to fill in da->data
 *       doff = decode_b64(xd, ddata, doff, da->data+dind, &rem_bytes_out)
 *            (returns number of unprocessed bytes, in [0..3])
 *       if( doff > 0 ) mv last doff bytes to beginning of ddata
 *       rem_bytes_in -= copy_len
 *       dind = updated offset into output da->data buffer
 */
static int append_to_data_b64(gxml_data * xd, char * dest, long long tot_bytes,
                              const char * cdata, int cdlen)
{
    const char   * cptr;
    long long      rem_bytes_out;        /* remaining length in darray->data */
    int            rem_bytes_in = cdlen; /* remaining length in cdata */
    int            copy_len, apply_len, unused;

    if( xd->verb > 4 )
        fprintf(stderr,"++ appending %d base64 binary bytes to data\n",cdlen);

    /* Copy cdata to local buffer in pieces, for storage of trailing
       characters from a previous call.  Given that, processing data
       as is done with ASCII seems reasonable. */
    while( rem_bytes_in > 0 ) {
        /*--- prepare intermediate buffer ---*/

        /* point to the current location */
        cptr = cdata + cdlen - rem_bytes_in;

        /* decide how many bytes to copy (avail space w/max of rem_bytes_in) */
        copy_len = xd->dlen - xd->doff - 1;
        if( copy_len > rem_bytes_in ) {
            unused = copy_len - rem_bytes_in;  /* unused at end of buffer */
            copy_len = rem_bytes_in;
        } else unused = 0;

        /* copy the data to our intermediate buffer
           (if we allow bad characters, skipping them, do it here) */
        (void)copy_b64_data(xd, cptr, xd->ddata+xd->doff, copy_len, &apply_len);

        /*--- process the data ---*/

        /* note how many bytes remain to be computed */
        rem_bytes_out = tot_bytes - xd->dind;
        if(xd->verb > 5)
            fprintf(stderr,"-- %lld bytes left at offset %lld\n",
                    rem_bytes_out, xd->dind);

        /* convert to binary bytes */
        xd->doff = decode_b64(xd,
                        xd->ddata,              /* data source */
                        xd->doff+apply_len,     /* data length */
                        dest + xd->dind,        /* output destination  */
                        &rem_bytes_out          /* nbytes left to fill */
                        );

        /*--- check results --- */
        if( xd->doff < 0 ) { xd->doff = 0; return 1; } /* error */
        if( xd->doff >= xd->dlen - 1 ) {
            if(xd->verb)
                fprintf(stderr,"** A2Db64: failed to process buffer\n");
            fprintf(stderr,"** rem = %d\n", xd->doff);
            xd->doff = 0;        /* blow away the buffer and continue */
        }

        /*--- adjust intermediate buffer ---*/

        /* move any unused bytes to the beginning (last doff, before unused) */
        if( xd->doff > 0 ) {
            if( xd->verb > 5 )
                fprintf(stderr,"++ A2Db64: move %d bytes from %d (blen %d)\n",
                    xd->doff, xd->dlen - unused - xd->doff, xd->dlen);
            /* (subtract unused+1, since 1 byte is saved for null */
            memmove(xd->ddata, xd->ddata+xd->dlen -(unused+1) - xd->doff,
                    xd->doff);
            if( xd->verb > 6 )
                fprintf(stderr,"   bytes are '%.*s'\n",xd->doff,
                        (char *)xd->ddata);
        }

        /* adjust remaining bytes for next time */
        rem_bytes_in -= copy_len;  /* more than apply_len, if bad chars */
        xd->dind = tot_bytes - rem_bytes_out;
    }

    return 0;
}

/* Copy the base64 data to the dest buffer, possibly noting, counting
 * and/or skipping any invalid characters, based on b64_check.
 *
 * return the number of bad characters noted
*/
static int copy_b64_data(gxml_data * xd, const char * src, char * dest,
                         int src_len, int * dest_len)
{
    const unsigned char * usrc = (const unsigned char *)src;
    int c, errs = 0, apply_len;

    if( xd->verb > 1 ) {  /* in verbose mode, perform automatic check */
        c = count_bad_b64_chars(src, src_len);
        if( c > 0 ) {
            fprintf(stderr, "CB64D: found %d bad b64 chars\n", c);
            if( xd->verb > 5 ) show_bad_b64_chars(src, src_len);
        }
    }

    switch( xd->b64_check ){
        default:
            fprintf(stderr,"** CB64D: b64_check = %d\n", xd->b64_check);
            /* whine and fall through */

        case GIFTI_B64_CHECK_NONE:   /* basic case - just copy the data */
            memcpy(dest, src, src_len);
            apply_len = src_len;
            break;

        case GIFTI_B64_CHECK_DETECT:   /* check for existence of bad chars */
            for(c = 0; c < src_len; c++)
                if( b64_decode_table[usrc[c]] == (unsigned char)0x80 ) {
                    errs++;
                    break;
                }
            memcpy(dest, src, src_len);
            apply_len = src_len;
            break;

        case GIFTI_B64_CHECK_COUNT:   /* count bad characters */
            for(c = 0; c < src_len; c++)
                if( b64_decode_table[usrc[c]] == (unsigned char)0x80 )
                    errs++;
            memcpy(dest, src, src_len);
            apply_len = src_len;
            break;

        case GIFTI_B64_CHECK_SKIP: /* skip bad characters, but don't count */
            apply_len = 0;
            for(c = 0; c < src_len; c++){
                if( b64_decode_table[usrc[c]] != (unsigned char)0x80 )
                    dest[apply_len++] = src[c];
            }
            break;
        case GIFTI_B64_CHECK_SKIPNCOUNT:    /* skip and count bad characters */
            apply_len = 0;
            for(c = 0; c < src_len; c++){
                if( b64_decode_table[usrc[c]] == (unsigned char)0x80 )
                    errs++;
                else
                    dest[apply_len++] = src[c];
            }
            break;
    }

    /* and null terminate */
    xd->ddata[xd->doff+apply_len] = '\0';

    /* note length and any errors */
    *dest_len = apply_len;
    xd->b64_errors += errs;

    return errs;
}

static int count_bad_b64_chars(const char * src, int len)
{
    const unsigned char * usrc = (const unsigned char *)src;
    int c, bad = 0;

    for(c = 0; c < len; c++)
        if( b64_decode_table[usrc[c]] == (unsigned char)0x80 )
            bad++;

    return bad;
}

static int show_bad_b64_chars(const char * src, int len)
{
    const unsigned char * usrc = (const unsigned char *)src;
    int c, bad = 0;

    fprintf(stderr,"-- bad b64 chars:");
    for(c = 0; c < len; c++)
        if( b64_decode_table[usrc[c]] == (unsigned char)0x80 ) {
            bad++;
            fprintf(stderr," 0x%02x", usrc[c]);
        }
    if( bad ) fputc('\n', stderr);
    else      fprintf(stderr," none");

    return bad;
}

/* this must go to the data of the latest darray struct */
static int append_to_data_ascii(gxml_data * xd, const char * cdata, int len)
{
    static int     mod_prev = 0;
    giiDataArray * da = xd->gim->darray[xd->gim->numDA-1]; /* current DA */

    char      * dptr;
    char      * cptr;
    long long   rem_vals;
    int         rem_len = len, copy_len, unused;
    int         type = da->datatype;

    if( xd->verb > 4 )
        fprintf(stderr,"++ appending %d ASCII bytes to data\n",len);

    /* if there is only whitespace, blow outta here */
    if( whitespace_len(cdata, len) == len ) { xd->doff = 0; return 0; }

    /* Copy cdata to local buffer in pieces, for null termination and for
       storage of trailing characters that may need to be processed again
       (after more characters are read by the parser).                    */
    while( rem_len > 0 ) {
        /*--- prepare intermediate buffer ---*/

        /* point to the current location */
        cptr = (char *)cdata + len - rem_len;

        /* if we're looking at whitespace, any unused data is garbage */
        if( isspace(*cptr)) xd->doff = 0;

        /* decide how many bytes to copy (avail space w/max of rem_len) */
        copy_len = xd->dlen - xd->doff - 1;
        if( copy_len > rem_len ) {
            unused = copy_len - rem_len;  /* unused at end of buffer */
            copy_len = rem_len;
        } else unused = 0;

        /* copy it to our buffer and null terminate */
        memcpy(xd->ddata+xd->doff, cptr, copy_len);
        xd->ddata[xd->doff+copy_len] = '\0';

        /*--- process the ascii data ---*/

        /* note how many values remain to be computed */
        rem_vals = da->nvals - xd->dind;
        if(xd->verb > 5)
            fprintf(stderr,"-- %lld vals left at offset %lld, nbyper %d\n",
                    rem_vals, xd->dind, da->nbyper);

        if( xd->dind == 0 ) mod_prev = 0;       /* nothing to modify at first */
        dptr = (char *)da->data + (xd->dind)*da->nbyper;
        xd->doff = decode_ascii(xd,
                        xd->ddata,              /* data source */
                        xd->doff+copy_len,      /* data length */
                        type,                   /* data type */
                        dptr,                   /* starting destination */
                        &rem_vals,              /* nvals to read */
                        &mod_prev               /* can we mod previous val */
                        );

        /*--- check results --- */
        if( xd->doff < 0 ) { xd->doff = 0; return 1; } /* error */
        if( xd->doff >= xd->dlen - 1 ) {
            if(xd->verb) fprintf(stderr,"** A2D: failed to process buffer\n");
            fprintf(stderr,"** rem = %d\n", xd->doff);
            xd->doff = 0;        /* blow away the buffer and continue */
        }

        /*--- adjust intermediate buffer ---*/

        /* move any unused bytes to the beginning (last doff, before unused) */
        if( xd->doff > 0 ) {
            if( xd->verb > 5 )
                fprintf(stderr,"++ A2D: move %d bytes from %d (blen %d)\n",
                    xd->doff, xd->dlen - unused - xd->doff, xd->dlen);
            /* (subtract unused+1, since 1 byte is saved for null */
            memmove(xd->ddata, xd->ddata+xd->dlen -(unused+1) - xd->doff,
                    xd->doff);
            if( xd->verb > 6 )
                fprintf(stderr,"   bytes are '%.*s'\n",xd->doff,
                        (char *)xd->ddata);
        }

        /* adjust rem_len for next time */
        rem_len -= copy_len;
        xd->dind = da->nvals - rem_vals;  /* note remaining values */
    }

    return 0;
}

/* this must go to the xform of the latest darray struct */
/* (process as 1-D array) */
static int append_to_xform(gxml_data * xd, const char * cdata, int len)
{
    static int  mod_prev = 0;
    giiDataArray * da = xd->gim->darray[xd->gim->numDA-1]; /* current DA */

    double    * dptr;
    char      * cptr;
    long long   rem_vals;
    int         rem_len = len, copy_len, unused, CSind;
    int         type = gifti_str2datatype("NIFTI_TYPE_FLOAT64"); /* double */

    if( !da || !xd->xlen || !xd->xdata || xd->dind < 0 || da->numCS <= 0) {
        fprintf(stderr,"** A2X: bad setup (%p,%d,%p,%lld,%d)\n",
                (void *)da, xd->xlen, (void *)xd->xdata, xd->dind, da->numCS);
        return 1;
    } else if( xd->verb > 4 )
        fprintf(stderr,"++ appending %d bytes to xform\n",len);

    CSind = da->numCS-1;
    if( !da->coordsys || !da->coordsys[CSind] ) {
        fprintf(stderr,"** A2X: bad coordsys[%d]\n", CSind);
        return 1;
    }

    /* if there is only whitespace, blow outta here */
    if( whitespace_len(cdata, len) == len ) { xd->doff = 0; return 0; }

    /* Copy cdata to local buffer in pieces, for null termination and for
       storage of trailing characters that may need to be processed again
       (after more characters are read by the parser).                    */
    while( rem_len > 0 ) {
        /*--- prepare intermediate buffer ---*/

        /* point to the current location */
        cptr = (char *)cdata + len - rem_len;

        /* if we're looking at whitespace, any unused data is garbage */
        if( isspace(*cptr)) xd->doff = 0;

        /* decide how many bytes to copy (avail space w/max of rem_len) */
        copy_len = xd->xlen - xd->doff - 1;
        if( copy_len > rem_len ) {
            unused = copy_len - rem_len;  /* unused at end of buffer */
            copy_len = rem_len;
        } else unused = 0;

        /* copy it to our buffer and null terminate */
        memcpy(xd->xdata+xd->doff, cptr, copy_len);
        xd->xdata[xd->doff+copy_len] = '\0';

        /* note how many values remain to be computed */
        rem_vals = 16 - xd->dind;

        /*--- process the ascii data ---*/
        if( xd->dind == 0 ) mod_prev = 0;       /* nothing to modify at first */
        dptr = (double *)da->coordsys[CSind]->xform + (xd->dind); /* as array */
        xd->doff = decode_ascii(xd,
                        xd->xdata,              /* data source */
                        xd->doff+copy_len,      /* data length */
                        type,                   /* data type */
                        dptr,                   /* starting destination */
                        &rem_vals,              /* nvals to read */
                        &mod_prev               /* can we mod previous val */
                        );

        /*--- check results --- */
        if( xd->doff < 0 ) { xd->doff = 0; return 1; } /* error */
        if( xd->doff >= xd->xlen - 1 ) {
            if(xd->verb) fprintf(stderr,"** A2X: failed to process buffer\n");
            fprintf(stderr,"** rem = %d\n", xd->doff);
            xd->doff = 0;        /* blow away the buffer and continue */
        }

        /*--- adjust intermediate buffer ---*/

        /* move any unused bytes to the beginning (last doff, before unused) */
        if( xd->doff > 0 ) {
            if( xd->verb > 5 )
                fprintf(stderr,"++ A2X: move %d bytes from %d (blen %d)\n",
                        xd->doff, xd->dlen - unused - xd->doff, xd->dlen);
                /* (subtract unused+1, since 1 bytes is saved for null */
                memmove(xd->xdata, xd->xdata+xd->xlen -(unused+1) -xd->doff,
                        xd->doff);
            if( xd->verb > 6 )
                fprintf(stderr,"   bytes are '%.*s'\n",xd->doff,
                        (char *)xd->ddata);
        }

        /* adjust rem_len for next time */
        rem_len -= copy_len;
        xd->dind = 16 - rem_vals;  /* note remaining values */
    }

    return 0;
}

#undef GII_B64_decode4
#define GII_B64_decode4(w,x,y,z,a,b,c)                                  \
     ( a = (b64_decode_table[w] << 2) | (b64_decode_table[x] >> 4) ,    \
       b = (b64_decode_table[x] << 4) | (b64_decode_table[y] >> 2) ,    \
       c = (b64_decode_table[y] << 6) | b64_decode_table[z]         )

/*  given: source pointer, length, dest loc and nbytes to set,
          (cdata is null-terminated)
    modify: needed (bytes) left for output
    return: nbytes unprocessed, so 0-3 (< 0 on error)

    Convert the base64 character data into binary.
        - read failure happens only when no characters are processed
        - characters are not checked for validity (maybe already done)

    note: the base64 defaults will be applied
            o EOL use is not allowed
            o padding is expected (using '=')
*/
static int decode_b64(gxml_data * xd, char * cdata, int cdlen,
                      char * dptr, long long * needed)
{
    unsigned char * din = (unsigned char *)cdata;
    unsigned char * dout = (unsigned char *)dptr;
    int             blocks = cdlen/4, rem = cdlen % 4;
    int             ind, assigned;

    if( xd->verb > 4)
        fprintf(stderr,"-- DB64: decode len %d, remain %lld\n", cdlen,*needed);
    if( *needed <= 0 ) {
        if( cdlen > 0 )
            fprintf(stderr,"** DB64: %d bytes left without a home\n", cdlen);
        return 0;
    }

    for( ind = 0; ind < blocks && *needed >= 3; ind++, *needed -= 3 ){
        GII_B64_decode4(din[0],din[1],din[2],din[3], dout[0],dout[1],dout[2]);

        din  += 4;
        dout += 3;
    }
    assigned = 3*ind;

    /* the first blocks-1 sets should just work */
    if( ind < blocks-1 || (ind < blocks && *needed == 0) ){
        if( xd->verb > 6 )
            gifti_disp_hex_data("decoded b64: 0x ", dptr, assigned, stderr);
        fprintf(stderr,"** decode_b64: more data than space\n");
        return -1;
    }

    /* if we didn't finish, try to fill a partial block */
    if( ind < blocks ) { /* so *needed < 3 */
        unsigned char a, b, c;
        GII_B64_decode4(din[0],din[1],din[2],din[3], a, b, c);
        if( *needed >= 1 ) dout[0] = a;
        if( *needed >= 2 ) dout[1] = b;
        assigned += *needed;
        *needed = 0;
    }

    if( xd->verb > 6 )
        gifti_disp_hex_data("decoded b64: 0x ", dptr, assigned, stderr);

    return rem;
}


/* given: source pointer, remaining length, nvals desired, dest loc and type
          (cdata is null-terminated)
   modify: nvals left for output, mod_prev for next call
   return: nbytes that may still need to processed (< 0 on error)

   read failure happens only when no characters are processed
*/
static int decode_ascii(gxml_data * xd, char * cdata, int cdlen, int type,
                        void * dptr, long long * nvals, int * mod_prev)
{
    char * p1, *p2;     /* for strtoX */
    char * prev;        /* for remain */
    double dval;        /* for strtod */
    long   lval;        /* for strtol */
    int    remain = 0;  /* use bytes remaining */
    int    vals = 0;

    if( xd->verb > 4)
        fprintf(stderr,"-- DA: type %s, len %d, nvals %lld\n",
                gifti_datatype2str(type),cdlen,*nvals);

    /* if reprocessing, maybe let the user know */
    if( xd->doff > 0 && *mod_prev ) {
        if( xd->verb > 4)
            fprintf(stderr,"++ DA: re-proc '%.*s' from '%.*s'...\n",
                    xd->doff, cdata, xd->doff+15, cdata);
        vals--;  /* back up */
    }

    switch( type ) {
        default :
            fprintf(stderr,"** decode_ascii cannot decode type %d\n",type);
            return -1;
        case NIFTI_TYPE_UINT8: {
            unsigned char * ptr = (unsigned char *)dptr;
            p1 = cdata;
            prev = p1;
            /* vals could be < 0, but we must care for promotion to size_t */
            while( (vals < 0 || vals < *nvals) && p1 ) {
                lval = strtol(p1, &p2, 10);   /* try to read next value */
                if( p1 == p2 ) break;   /* nothing read, terminate loop */
                prev = p1;              /* store old success ptr */
                p1 = p2;                /* move to next posn */
                ptr[vals] = lval;       /* assign new value  */
                if(xd->verb>6)fprintf(stderr,"  v %d (%ld)",ptr[vals],lval);
                vals++;                 /* count new value   */
            }
            if(xd->verb > 6) fputc('\n', stderr);
            break;
        }
        case NIFTI_TYPE_INT16: {
            short * ptr = (short *)dptr;
            p1 = cdata;
            prev = p1;
            while( (vals < 0 || vals < *nvals) && p1 ) {
                lval = strtol(p1, &p2, 10);   /* try to read next value */
                if( p1 == p2 ) break;   /* nothing read, terminate loop */
                prev = p1;              /* store old success ptr */
                p1 = p2;                /* move to next posn */
                ptr[vals] = lval;       /* assign new value  */
                if(xd->verb>6)fprintf(stderr,"  v %d (%ld)",ptr[vals],lval);
                vals++;                 /* count new value   */
            }
            if(xd->verb > 6) fputc('\n', stderr);
            break;
        }
        case NIFTI_TYPE_INT32: {
            int * ptr = (int *)dptr;
            p1 = cdata;
            prev = p1;
            while( (vals < 0 || vals < *nvals) && p1 ) {
                lval = strtol(p1, &p2, 10);   /* try to read next value */
                if( p1 == p2 ) break;   /* nothing read, terminate loop */
                prev = p1;              /* store old success ptr */
                p1 = p2;                /* move to next posn */
                ptr[vals] = lval;       /* assign new value  */
                if(xd->verb>6)fprintf(stderr,"  v %d (%ld)",ptr[vals],lval);
                vals++;                 /* count new value   */
            }
            if(xd->verb > 6) fputc('\n', stderr);
            break;
        }
        case NIFTI_TYPE_FLOAT32: {
            float * ptr = (float *)dptr;
            p1 = cdata;
            prev = p1;
            while( (vals < 0 || vals < *nvals) && p1 ) {
                dval = strtod(p1, &p2); /* try to read next value */
                if( p1 == p2 ) break;   /* nothing read, terminate loop */
                prev = p1;              /* store old success ptr */
                p1 = p2;                /* move to next posn */
                ptr[vals] = dval;       /* assign new value  */
                if(xd->verb>6)fprintf(stderr,"  v %f (%f)",ptr[vals],dval);
                vals++;                 /* count new value   */
            }
            if(xd->verb > 6) fputc('\n', stderr);
            break;
        }
        case NIFTI_TYPE_FLOAT64: {
            double * ptr = (double *)dptr;
            p1 = cdata;
            prev = p1;
            while( (vals < 0 || vals < *nvals) && p1 ) {
                dval = strtod(p1, &p2); /* try to read next value */
                if( p1 == p2 ) break;   /* nothing read, terminate loop */
                prev = p1;              /* store old success ptr */
                p1 = p2;                /* move to next posn */
                ptr[vals] = dval;       /* assign new value  */
                if(xd->verb>6)fprintf(stderr,"  v %f (%f)",ptr[vals],dval);
                vals++;                 /* count new value   */
            }
            if(xd->verb > 6) fputc('\n', stderr);
            break;
        }
        case NIFTI_TYPE_INT8: {
            char * ptr = (char *)dptr;
            p1 = cdata;
            prev = p1;
            /* vals could be < 0, but we must care for promotion to size_t */
            while( (vals < 0 || vals < *nvals) && p1 ) {
                lval = strtol(p1, &p2, 10);   /* try to read next value */
                if( p1 == p2 ) break;   /* nothing read, terminate loop */
                prev = p1;              /* store old success ptr */
                p1 = p2;                /* move to next posn */
                ptr[vals] = lval;       /* assign new value  */
                if(xd->verb>6)fprintf(stderr,"  v %d (%ld)",ptr[vals],lval);
                vals++;                 /* count new value   */
            }
            if(xd->verb > 6) fputc('\n', stderr);
            break;
        }
        case NIFTI_TYPE_UINT16: {
            unsigned short * ptr = (unsigned short *)dptr;
            p1 = cdata;
            prev = p1;
            while( (vals < 0 || vals < *nvals) && p1 ) {
                lval = strtol(p1, &p2, 10);   /* try to read next value */
                if( p1 == p2 ) break;   /* nothing read, terminate loop */
                prev = p1;              /* store old success ptr */
                p1 = p2;                /* move to next posn */
                ptr[vals] = lval;       /* assign new value  */
                if(xd->verb>6)fprintf(stderr,"  v %d (%ld)",ptr[vals],lval);
                vals++;                 /* count new value   */
            }
            if(xd->verb > 6) fputc('\n', stderr);
            break;
        }
        case NIFTI_TYPE_INT64: {
            long long * ptr = (long long *)dptr;
            long long llval;
            p1 = cdata;
            prev = p1;
            while( (vals < 0 || vals < *nvals) && p1 ) {
#if defined(_WIN32) && !defined(__MINGW32__) && !defined(__CYGWIN__)
                llval = strtol(p1, &p2, 10);   /* There is no strtoll defined in MS VC++ */
#else
                llval = strtoll(p1, &p2, 10);   /* try to read next value */
#endif
                if( p1 == p2 ) break;   /* nothing read, terminate loop */
                prev = p1;              /* store old success ptr */
                p1 = p2;                /* move to next posn */
                ptr[vals] = llval;      /* assign new value  */
                if(xd->verb>6)fprintf(stderr,"  v %lld (%lld)",ptr[vals],llval);
                vals++;                 /* count new value   */
            }
            if(xd->verb > 6) fputc('\n', stderr);
            break;
        }
    }

    /* update the number of values processed */
    if( vals > 0 ) (*nvals) -= vals;

    /* ponder remaining: if *p1 is space, look from there, else from prev */
    if( p1 ){
        if( isspace(*p1) ) {
            remain = cdlen - (p1 - cdata);
            *mod_prev = 0;
        }
        else if( prev ) {
            remain = cdlen - (prev - cdata);
            *mod_prev = 1;  /* still looking at previous val */
        }
    }

    /* if only whitespace left, ignore */
    if( whitespace_len(cdata + (cdlen-remain), remain) == remain )
        remain = 0;

    if(xd->verb > 6) fprintf(stderr,"-- DA: remain = %d\n", remain);

    return remain;
}

static void XMLCALL cb_instr(void *udata, const char *target, const char *data)
{
    gxml_data * xd = (gxml_data *)udata;
    if( xd->verb > 3 ){
        show_depth(xd->depth, 1, stderr);
        fprintf(stderr, "instr: %s='%s'\n",target,data);
    }
}

static void XMLCALL cb_comment(void *udata, const char * str)
{
    gxml_data * xd = (gxml_data *)udata;
    if( xd->verb > 1 ){
        show_depth(xd->depth, 1, stderr);
        fprintf(stderr, "comment: '%s'\n",str);
    }
}

static void XMLCALL cb_cdata_start(void *udata)
{
    gxml_data * xd = (gxml_data *)udata;

    if( xd->verb > 3 ){
        show_depth(xd->depth, 1, stderr);
        fprintf(stderr, "cdata_start\n");
    }
    (void)epush(xd, GXML_ETYPE_CDATA, enames[GXML_ETYPE_CDATA], NULL);
}

static void XMLCALL cb_cdata_end(void *udata)
{
    gxml_data * xd = (gxml_data *)udata;
    epop(xd, GXML_ETYPE_CDATA, enames[GXML_ETYPE_CDATA]);
}

static void XMLCALL cb_default(void *udata, const char * str, int length)
{
    gxml_data * xd = (gxml_data *)udata;
    int wlen = whitespace_len(str,length);
    int len = length;

    if( len == wlen )
    {
        if( xd->verb < 4 ) return;

        str = "whitespace";     /* just note the whitespace */
        len = strlen(str);
    }

    if( xd->verb > 3 ){
        show_depth(xd->depth, 1, stderr);
        fprintf(stderr, "default XML element [%d]: '%.*s'\n",length,len,str);
    }
}

static void XMLCALL cb_xml_dec(void *udata, const char * ver,
                               const char * enc, int standalone)
{
    gxml_data * xd = (gxml_data *)udata;
    if( xd->verb > 2 ){
        show_depth(xd->depth, 1, stderr);
        fprintf(stderr, "xmldec ver = %s, enc = %s, standalone = %d\n",
                ver,enc,standalone);
    }
}

static void XMLCALL cb_start_doctype(void *udata, const char * doctype,
                const char * sysid, const char * pubid, int has_subset )
{
    gxml_data * xd = (gxml_data *)udata;
    if( xd->verb > 2 ){
        show_depth(xd->depth, 1, stderr);
        /* check for NULL in optional strings   4 Mar 2010 */
        fprintf(stderr, "start_doctype, dt='%s', sid='%s',pid='%s', sub=%d\n",
               doctype, sysid?sysid:"NULL", pubid?pubid:"NULL", has_subset);
    }
}

static void XMLCALL cb_end_doctype(void *udata)
{
    gxml_data * xd = (gxml_data *)udata;
    if( xd->verb > 2 ){
        show_depth(xd->depth, 1, stderr);
        fprintf(stderr, "end_doctype\n");
    }
}

static void XMLCALL cb_elem_dec(void *udata, const char * ename,
                                             XML_Content * content)
{
    gxml_data * xd = (gxml_data *)udata;
    if( xd->verb > 2 ){
        show_depth(xd->depth, 1, stderr);
        fprintf(stderr,"%s: type=%d, quant=%d, name=%s, numc=%d, cp=%p\n",
                ename, content->type, content->quant, content->name,
                content->numchildren, (void *)content->children);
    }
}

static XML_Parser init_xml_parser( void * user_data )
{
    XML_Parser parser;

    parser = XML_ParserCreate(NULL);
    XML_SetUserData(parser, user_data);
    XML_SetStartElementHandler(parser, cb_start_ele);
    XML_SetEndElementHandler(parser, cb_end_ele);
    XML_SetCharacterDataHandler(parser, cb_char);
    XML_SetProcessingInstructionHandler(parser, cb_instr);
    XML_SetCommentHandler(parser, cb_comment);
    XML_SetStartCdataSectionHandler(parser, cb_cdata_start);
    XML_SetEndCdataSectionHandler(parser, cb_cdata_end);
    XML_SetDefaultHandler(parser, cb_default);
    XML_SetXmlDeclHandler(parser, cb_xml_dec);
    XML_SetStartDoctypeDeclHandler(parser, cb_start_doctype);
    XML_SetEndDoctypeDeclHandler(parser, cb_end_doctype);
    XML_SetElementDeclHandler(parser, cb_elem_dec);

    if( GXD.verb > 3 ) fprintf(stderr,"-- parser initialized\n");

    return parser;
}


static int show_stack(char * mesg, gxml_data * xd)
{
    int c;
    if( !xd ) return 1;
    if( mesg ) fputs(mesg, stderr);
    fprintf(stderr,"stack[%d]", xd->depth);
    for( c = 0; c < xd->depth; c++ )
        fprintf(stderr," : %s", enames[xd->stack[c]]);
    fputc('\n', stderr);
    return 0;
}

static int stack_is_valid(gxml_data * xd)
{
    int valid, etype, parent, bad_parent;

    if( xd->depth  < 0 ) return 0;
    if( xd->depth == 0 ) return 1;

    etype = xd->stack[xd->depth-1];         /* depth is at least 1 */

    /* process depth 1 separately, so we can assume a parent later */
    if( xd->depth == 1 ) {
        if( etype != GXML_ETYPE_GIFTI ) {
            show_stack("** invalid element on ", xd);
            return 0;
        }
        return 1;
    }

    /* verify proper parent (or invalid type) */
    valid = 1;
    bad_parent = 0;
    parent = xd->stack[xd->depth-2];    /* depth is at least 2 */
    switch( etype ) {
        default:
        case GXML_ETYPE_INVALID:
        case GXML_ETYPE_GIFTI:   /* should only be at depth 1 */
            valid = 0;
            break;
        case GXML_ETYPE_META:
            if( parent != GXML_ETYPE_GIFTI &&
                parent != GXML_ETYPE_DATAARRAY )       bad_parent = 1;
            break;
        case GXML_ETYPE_MD:
            if( parent != GXML_ETYPE_META )            bad_parent = 1;
            break;
        case GXML_ETYPE_NAME:
            if( parent != GXML_ETYPE_MD )              bad_parent = 1;
            break;
        case GXML_ETYPE_VALUE:
            if( parent != GXML_ETYPE_MD )              bad_parent = 1;
            break;
        case GXML_ETYPE_LABELTABLE:
            if( parent != GXML_ETYPE_GIFTI )           bad_parent = 1;
            break;
        case GXML_ETYPE_LABEL:
            if( parent != GXML_ETYPE_LABELTABLE )      bad_parent = 1;
            break;
        case GXML_ETYPE_DATAARRAY:
            if( parent != GXML_ETYPE_GIFTI )           bad_parent = 1;
            break;
        case GXML_ETYPE_CSTM:
            if( parent != GXML_ETYPE_DATAARRAY )       bad_parent = 1;
            break;
        case GXML_ETYPE_DATA:
            if( parent != GXML_ETYPE_DATAARRAY )       bad_parent = 1;
            break;
        case GXML_ETYPE_DATASPACE:
            if( parent != GXML_ETYPE_CSTM )            bad_parent = 1;
            break;
        case GXML_ETYPE_XFORMSPACE:
            if( parent != GXML_ETYPE_CSTM )            bad_parent = 1;
            break;
        case GXML_ETYPE_MATRIXDATA:
            if( parent != GXML_ETYPE_CSTM )            bad_parent = 1;
            break;
        case GXML_ETYPE_CDATA:
            if( parent != GXML_ETYPE_NAME       &&
                parent != GXML_ETYPE_VALUE      &&
                parent != GXML_ETYPE_LABEL      &&
                parent != GXML_ETYPE_DATASPACE  &&
                parent != GXML_ETYPE_XFORMSPACE &&
                parent != GXML_ETYPE_MATRIXDATA )      bad_parent = 1;
            break;
    }

    /* possibly print a message if the stack looks bad */
    if( bad_parent && GXD.verb )
        fprintf(stderr,"** %s: bad parent '%s'\n",enames[etype],enames[parent]);
    if( (!valid || bad_parent) && GXD.verb > 1 ) show_stack("** invalid ", xd);

    return valid;
}

/* if bsize is no longer correct, update it and realloc the buffer */
static int reset_xml_buf(gxml_data * xd, char ** buf, int * bsize)
{
    if( *bsize == xd->buf_size ) {
        if( xd->verb > 3 )
            fprintf(stderr,"-- buffer kept at %d bytes\n", *bsize);
        return 0;
    }

    if( xd->verb > 2 )
        fprintf(stderr,"++ update buf, %d to %d bytes\n",*bsize,xd->buf_size);

    *bsize = xd->buf_size;
    *buf = (char *)realloc(*buf, *bsize * sizeof(char));

    if( ! *buf ) {
        fprintf(stderr,"** failed to alloc %d bytes of xml buf!\n", *bsize);
        *bsize = 0;
        return 1;
    }

    return 0;
}

/* decide how big a processing buffer should be
   (either for a small xform matrix or a Data element)
*/
static int partial_buf_size(long long nbytes)
{
    int ibytes = (int)nbytes;    /* never more than 10 MB, anyway */

    if( ibytes <= GXML_MIN_BSIZE ) return GXML_MIN_BSIZE;
    if( ibytes <= 64*1024 )        return ibytes;

    if( ibytes <= 10*1024*1024 )  /* divide by 10, but round up to a block */
        return (ibytes/10 + 0xfff) & ~0xfff;

    return 1024*1024;
}

/* update xd->buf_size, used prior to reset_xml_buf */
static int update_xml_buf_size(gxml_data * xd, long long bytes)
{
    int new_size;

    if( !xd || bytes < 0 ){
        if( xd->verb > 1 )
            fprintf(stderr,"** bad update_xml_buf_size with %p and %lld\n",
                    (void *)xd,bytes);
        return 0;
    }

    new_size = partial_buf_size(bytes);
    if( new_size != xd->buf_size ){
        if( xd->verb > 2 )
            fprintf(stderr,"++ update XML buf size, %d to %d (for %lld)\n",
                    xd->buf_size, new_size, bytes);
        xd->buf_size = new_size;
    }

    return 0;
}


/* used to update any buffer, as the pointer address is passed in */
static int update_partial_buffer(char ** buf, int * blen, long long bytes,
                                 int full)
{
    int bsize = partial_buf_size(bytes);

    if( full ) bsize = bytes;   /* want entire buffer */

    if( !buf || !blen || bytes <= 0 ) {
        fprintf(stderr,"** UPB: bad params (%p,%p,%lld)\n",
                (void *)buf, (void *)blen, bytes);
        return 1;
    }

    /* just make sure we have a text buffer to work with */
    if( *buf || *blen != bsize ) {
        if( GXD.verb > 2 )
            fprintf(stderr,"++ UPB, alloc %d bytes (from %lld, %d) for buff\n",
                    bsize, bytes, full);
        *buf = (char *)realloc(*buf, bsize * sizeof(char));
        if( !*buf ) {
            fprintf(stderr,"** UPB: cannot alloc %d bytes for buffer\n",bsize);
            return 1;
        }
        *blen = bsize;
    }

    return 0;
}

static int gxml_write_gifti(gxml_data * xd, FILE * fp)
{
    gifti_image * gim = xd->gim;

    int c, offset;
    int first = 1;  /* first attr to print? */

    if( !gim || !fp ) return 1;

    if( xd->verb > 1 )
        fprintf(stderr,"++ gifti image, numDA = %d, size = %lld MB\n",
                gim->numDA, gifti_gim_DA_size(gim,1));

    gxml_write_preamble(fp);
    fprintf(fp,"<%s",enames[GXML_ETYPE_GIFTI]);
    if(gim->version){ fprintf(fp," Version=\"%s\"", gim->version); first = 0; }
    /* add space if no version, requested by E Anderson */
    fprintf(fp,"%sNumberOfDataArrays=\"%d\"", first ? " " : "  ", gim->numDA);

    /* add any extra attributes */
    offset = strlen(enames[GXML_ETYPE_GIFTI]) + 2;
    ewrite_ex_atrs(xd, &gim->ex_atrs, offset, 0, fp);
    fputs(">\n",fp);

    xd->depth++;
    ewrite_meta(xd, &gim->meta, fp);
    ewrite_LT(xd, &gim->labeltable, 1, fp);

    /* write the giiDataArray */
    if(!gim->darray) {
        if( xd->verb > 0 ) fprintf(stderr,"** gifti_image, missing darray\n");
    } else {
        for( c = 0; c < gim->numDA; c++ )
            ewrite_darray(xd, gim->darray[c], fp);
    }

    xd->depth--;
    fprintf(fp,"</%s>\n",enames[GXML_ETYPE_GIFTI]);

    return 0;
}

static int ewrite_darray(gxml_data * xd, giiDataArray * da, FILE * fp)
{
    int  spaces = xd->indent * xd->depth;
    int  offset, c;
    char dimstr[5] = "Dim0";

    if( xd->verb > 3 ) fprintf(stderr,"++ write giiDataArray\n");

    if( !da ) return 0;

    offset = strlen(enames[GXML_ETYPE_DATAARRAY]) + 2 + spaces;
    fprintf(fp, "%*s<DataArray", spaces, "");

    /* print attributes */
    ewrite_str_attr("Intent", gifti_intent_to_string(da->intent), offset,1,fp);
    ewrite_str_attr("DataType", gifti_datatype2str(da->datatype), offset,0,fp);
    ewrite_str_attr("ArrayIndexingOrder",
                    gifti_list_index2string(gifti_index_order_list,da->ind_ord),
                    offset,0,fp);
    ewrite_int_attr("Dimensionality", da->num_dim, offset, 0, fp);
    for( c = 0; c < da->num_dim; c++ ) {
        ewrite_int_attr(dimstr, da->dims[c], offset, 0, fp);
        dimstr[3]++;  /* too devious??  iterate '0', '1', ... */
    }
    ewrite_str_attr("Encoding",
        gifti_list_index2string(gifti_encoding_list,da->encoding),offset,0,fp);
    ewrite_str_attr("Endian",   /* set endian to that of this CPU */
        gifti_list_index2string(gifti_endian_list, gifti_get_this_endian()),
                                offset,0,fp);
    ewrite_str_attr("ExternalFileName", da->ext_fname, offset, 0, fp);
    if( da->ext_fname && *da->ext_fname )
        ewrite_long_long_attr("ExternalFileOffset",da->ext_offset, offset,0,fp);
    else
        ewrite_str_attr("ExternalFileOffset", NULL, offset, 0, fp);
    fprintf(fp, ">\n");

    /* write sub-elements */
    xd->depth++;
    ewrite_meta(xd, &da->meta, fp);
    for( c = 0; c < da->numCS; c++ )
        ewrite_coordsys(xd, da->coordsys[c], fp);
    ewrite_data(xd, da, fp);
    xd->depth--;

    fprintf(fp, "%*s</DataArray>\n", spaces, "");

    return 0;
}


/* this depends on ind_ord, how to write out lines */
static int ewrite_data(gxml_data * xd, giiDataArray * da, FILE * fp)
{
    long long c, rows, cols;
    int       spaces = xd->indent * xd->depth;
    int       errs = 0;

    if( !da ) return 0;         /* okay, may not exist */

    if( xd->verb > 3 )
        fprintf(stderr,"++ write %s Data\n",
                gifti_list_index2string(gifti_encoding_list, da->encoding));

    /* maybe there is no data to write */
    if( !da->data || da->nvals <= 0 || da->nbyper <= 0 ) {
        fprintf(fp, "%*s<%s/>\n", spaces, "", enames[GXML_ETYPE_DATA]);
        return 0;
    }

    if (da->encoding == GIFTI_ENCODING_EXTBIN) /* then write as empty */
        fprintf(fp, "%*s<%s/>\n", spaces, "", enames[GXML_ETYPE_DATA]);
    else  /* write normal Data tag */
        fprintf(fp, "%*s<%s>", spaces, "", enames[GXML_ETYPE_DATA]);

    if( xd->dstore ) {
        if( da->encoding == GIFTI_ENCODING_ASCII ) {
            fprintf(fp, "\n");
            gifti_DA_rows_cols(da, &rows, &cols);  /* product will be nvals */
            for(c = 0; c < rows; c++ )
                ewrite_data_line(da->data,da->datatype,c,cols,
                                 spaces+xd->indent,fp);
            fprintf(fp, "%*s", spaces, "");
        } else if( da->encoding == GIFTI_ENCODING_B64BIN ) {
            gxml_disp_b64_data(NULL, da->data, da->nvals*da->nbyper, fp);
        } else if( da->encoding == GIFTI_ENCODING_B64GZ ) {
#ifdef HAVE_ZLIB   /* for compiling, higher level test elsewhere */
            uLongf blen = da->nvals*da->nbyper * 1.01 + 12; /* zlib.net */
            int    rv = 0;
            if( update_partial_buffer(&xd->zdata, &xd->zlen, blen, 1) )
                return 1;

            rv = compress2((Bytef *)xd->zdata, &blen, da->data,
                           da->nvals*da->nbyper, xd->zlevel);
            if ( xd->verb > 2 )
                fprintf(stderr,"-- compress buffer (%.2f%% of %lld bytes)...\n",
                        100.0*blen/(da->nvals*da->nbyper),da->nvals*da->nbyper);
            if( rv != Z_OK ) {
                fprintf(stderr,"** zlib compression failure: ");
                if( rv == Z_MEM_ERROR ) fprintf(stderr,"not enough memory\n");
                if( rv == Z_BUF_ERROR ) fprintf(stderr,"buffer too short\n");
                else                    fprintf(stderr,"unknown error %d\n",rv);
                errs++;
            } else if ( xd->verb > 2 )
                fprintf(stderr,"-- compression succeeded\n");

            gxml_disp_b64_data(NULL, xd->zdata, blen, fp);
#else
            fprintf(stderr,"** ewrite_data: no ZLIB to compress with\n");
#endif
        } else if (da->encoding == GIFTI_ENCODING_EXTBIN)  {
            /* write to external file */
            if( gifti_write_extern_DA_data(da) ) errs = 1;
        } else {
            fprintf(stderr,"** unknown data encoding, %d\n", da->encoding);
            errs = 1;
        }
    }

    if (da->encoding != GIFTI_ENCODING_EXTBIN)
        fprintf(fp, "</%s>\n", enames[GXML_ETYPE_DATA]);

    return errs;
}

#undef GII_B64_encode3
#define GII_B64_encode3(a,b,c,w,x,y,z)                       \
     ( w = b64_encode_table[(a)>>2]                      ,   \
       x = b64_encode_table[((a & 3) << 4) | (b >> 4)]   ,   \
       y = b64_encode_table[((b & 0xF) << 2) | (c >> 6)] ,   \
       z = b64_encode_table[c & 0x3F]                     )
static int gxml_disp_b64_data(const char *mesg, const void *data, int len,
                              FILE *fp)
{
    const unsigned char * dp = (const unsigned char *)data;
    unsigned char         w, x, y, z;
    FILE                * stream;
    int                   c, rem = len % 3;

    stream = fp ? fp : stdout;

    if( !data || len < 1 ) return -1;

    if( mesg ) fputs(mesg, stream);

    /* first get all of the 3-byte blocks */
    for( c = 0; c < len/3; c++, dp += 3 ) {
        GII_B64_encode3(dp[0], dp[1], dp[2], w, x, y, z);
        fprintf(stream, "%c%c%c%c", w, x, y, z);
    }

    /* finish off the last bytes */
    if( rem == 1 ) {
        GII_B64_encode3(dp[0], 0, 0, w, x, y, z);
        fprintf(stream, "%c%c==", w, x);
    } else if ( rem == 2 ) {
        GII_B64_encode3(dp[0], dp[1], 0, w, x, y, z);
        fprintf(stream, "%c%c%c=", w, x, y);
    }
    /* else we're done */

    return 0;
}


static int ewrite_coordsys(gxml_data * xd, giiCoordSystem * cs, FILE * fp)
{
    int c, spaces = xd->indent * xd->depth;

    if( !cs ) return 0;         /* okay, may not exist */

    if( xd->verb > 3 ) fprintf(stderr,"++ write giiCoordSystem\n");

    fprintf(fp, "%*s<%s>\n", spaces, "", enames[GXML_ETYPE_CSTM]);
    spaces += xd->indent;

    ewrite_text_ele(GXML_ETYPE_DATASPACE, cs->dataspace, NULL, spaces, 1, fp);
    ewrite_text_ele(GXML_ETYPE_XFORMSPACE, cs->xformspace, NULL, spaces, 1, fp);

    fprintf(fp, "%*s<MatrixData>\n", spaces, "");
    for(c = 0; c < 4; c++ )
        ewrite_double_line(cs->xform[c], 4, spaces+xd->indent, fp);
    fprintf(fp, "%*s</MatrixData>\n", spaces, "");

    spaces -= xd->indent;
    fprintf(fp, "%*s</%s>\n", spaces, "", enames[GXML_ETYPE_CSTM]);

    return 0;
}


/* write one 'row' ('cols' values) of data in text */
static int ewrite_data_line(void * data, int type, long long row,
                            long long cols, int space, FILE * fp)
{
    int c;
    if( !data || row < 0 || cols <= 0 || !fp ) return 1;

    fprintf(fp, "%*s", space, "");
    switch( type ) {
        default :
            fprintf(stderr,"** write_data_line, unknown type %d\n",type);
            return -1;
        case NIFTI_TYPE_UINT8: {
            unsigned char * ptr = (unsigned char *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_INT16: {
            short * ptr = (short *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_INT32: {
            int * ptr = (int *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_FLOAT32: {
            float * ptr = (float *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%f ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_COMPLEX64: {
            float * ptr = (float *)data + row * cols;
            for(c = 0; c < 2*cols; c+=2)fprintf(fp, "%f %f   ",ptr[c],ptr[c+1]);
            break;
        }
        case NIFTI_TYPE_FLOAT64: {
            double * ptr = (double *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%f ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_RGB24: {
            unsigned char * ptr = (unsigned char *)data + row * cols;
            for( c = 0; c < 3*cols; c+=3 )
                fprintf(fp, "%u %u %u   ", ptr[c], ptr[c+1], ptr[c+2]);
            break;
        }
        case NIFTI_TYPE_INT8: {
            char * ptr = (char *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_UINT16: {
            unsigned short * ptr = (unsigned short *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_UINT32: {
            unsigned int * ptr = (unsigned int *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_INT64: {
            long long * ptr = (long long *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%lld ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_UINT64: {
            unsigned long long * ptr = (unsigned long long *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%llu ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_FLOAT128: {
            long double * ptr = (long double *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%Lf ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_COMPLEX128: {
            double * ptr = (double *)data + row * cols;
            for(c = 0; c < 2*cols; c+=2)fprintf(fp, "%f %f   ",ptr[c],ptr[c+1]);
            break;
        }
        case NIFTI_TYPE_COMPLEX256: {
            long double * ptr = (long double *)data + row * cols;
            for(c = 0; c<2*cols; c+=2)fprintf(fp, "%Lf %Lf   ",ptr[c],ptr[c+1]);
            break;
        }
    }

    fputc('\n', fp);

    return 0;
}


static int ewrite_double_line(double * data, int nvals, int space, FILE * fp)
{
    int c;
    if( !data || nvals <= 0 || !fp ) return 1;

    fprintf(fp, "%*s", space, "");
    for( c = 0; c < nvals; c++ )        /* duplicate trailing space for diff */
        fprintf(fp, "%f ", data[c]);
    fputc('\n', fp);

    return 0;
}


static int ewrite_text_ele(int ele, const char * cdata, const char * attr,
                           int spaces, int in_CDATA, FILE * fp)
{
    int index = ele;

    if(ele < 0 || ele > GXML_MAX_ELEN) index = 0;  /* be safe */

    fprintf(fp, "%*s<%s%s>%s%s%s</%s>\n",
            spaces, "", enames[index],
            attr ? attr : "",
            in_CDATA ? "<![CDATA[" : "",
            cdata ? cdata : "",
            in_CDATA ? "]]>" : "",
            enames[index]);

    return 0;
}

static int ewrite_LT(gxml_data *xd, giiLabelTable *lt, int in_CDATA, FILE *fp)
{
    char    attr[256] = "";
    float * rgba;
    int     c, spaces = xd->indent * xd->depth;

    if( xd->verb > 3 ) fprintf(stderr,"++ write giiLabelTable\n");

    if( !lt || lt->length == 0 || !lt->key || !lt->label ) {
        fprintf(fp, "%*s<LabelTable/>\n", spaces, "");
        return 0;
    }

    fprintf(fp, "%*s<LabelTable>\n", spaces, "");
    rgba = lt->rgba;
    for( c = 0; c < lt->length; c++ ) {
        if( !lt->label[c] ) {
            if(xd->verb > 1) fprintf(stderr,"** label[%d] unset\n", c);
            continue;
        }

        /* store the Key and optional RGBA attributes */
        if( lt->rgba ) {
           sprintf(attr, " Key=\"%d\""
                         " Red=\"%g\" Green=\"%g\" Blue=\"%g\" Alpha=\"%g\"",
                   lt->key[c], rgba[0], rgba[1], rgba[2], rgba[3]);
           rgba += 4;
        } else
            sprintf(attr, " Key=\"%d\"", lt->key[c]);

        ewrite_text_ele(GXML_ETYPE_LABEL, lt->label[c], attr,
                        spaces+xd->indent, in_CDATA, fp);
    }
    fprintf(fp, "%*s</LabelTable>\n", spaces, "");

    return 0;
}


static int ewrite_meta(gxml_data * xd, giiMetaData * md, FILE * fp)
{
    int c, spaces = xd->indent * xd->depth;

    if( xd->verb > 3 ) fprintf(stderr,"++ write giiMetaData\n");

    if( !md || md->length == 0 || !md->name || !md->value ) {
        fprintf(fp, "%*s<MetaData/>\n", spaces, "");
        return 0;
    }

    if( xd->verb > 3 ) fprintf(stderr,"   MD length = %d\n", md->length);

    fprintf(fp, "%*s<MetaData>\n", spaces, "");
    for( c = 0; c < md->length; c++ ) {
        if( !md->name[c] ) {  /* allow empty value, but not name */
            if(xd->verb > 1) fprintf(stderr,"** MD[%d] unset\n", c);
            continue;
        }

        spaces += xd->indent;
        fprintf(fp,"%*s<MD>\n", spaces, "");

        spaces += xd->indent;
        ewrite_text_ele(GXML_ETYPE_NAME, md->name[c], NULL, spaces, 1,fp);
        ewrite_text_ele(GXML_ETYPE_VALUE,md->value[c],NULL, spaces, 1,fp);
        spaces -= xd->indent;

        fprintf(fp,"%*s</MD>\n", spaces, "");
        spaces -= xd->indent;
    }
    fprintf(fp, "%*s</MetaData>\n", spaces, "");

    return 0;
}


/* print a list of attributes, indented to the same level */
static int ewrite_ex_atrs(gxml_data * xd, nvpairs * nvp, int offset,
                          int first, FILE * fp)
{
    int c, spaces = xd->indent * xd->depth + offset;

    if(xd->verb > 2) fprintf(stderr,"++ write %d ex_atr's\n", nvp->length);

    for( c = 0; c < nvp->length; c++ ) {
        ewrite_str_attr(nvp->name[c], nvp->value[c], spaces, first, fp);
        if( first ) first = 0;
    }

    return 0;
}


static int ewrite_int_attr(const char *name, int value, int spaces,
                           int first, FILE * fp)
{
    fprintf(fp, "%s%*s%s=\"%d\"",
            (first) ? "" : "\n",        /* maybe a newline   */
            (first) ?  1 : spaces, "",  /* 1 or many spaces  */
            name, value);
    return 0;
}


static int ewrite_long_long_attr(const char *name, long long value, int spaces,
                                 int first, FILE * fp)
{
    fprintf(fp, "%s%*s%s=\"%lld\"",
            (first) ? "" : "\n",        /* maybe a newline   */
            (first) ?  1 : spaces, "",  /* 1 or many spaces  */
            name, value);
    return 0;
}


static int ewrite_str_attr(const char * name, const char * value, int spaces,
                           int first, FILE * fp)
{
    fprintf(fp, "%s%*s%s=\"%s\"",
            (first) ? "" : "\n",        /* maybe a newline   */
            (first) ?  1 : spaces, "",  /* 1 or many spaces  */
            name, value ? value : "");
    return 0;
}


static int gxml_write_preamble(FILE * fp)
{
    char * version  = GIFTI_XML_VERSION;
    char * encoding = GIFTI_XML_ENCODING;
    char * dtd      = GIFTI_XML_DTD_SOURCE;

    fprintf(fp, "<?xml version=\"%s\" encoding=\"%s\"?>\n", version, encoding);
    fprintf(fp, "<!DOCTYPE GIFTI SYSTEM \"%s\">\n", dtd);

    return 0;
}

static int disp_gxml_data(char * mesg, gxml_data * dp, int show_all )
{
    if( mesg ) fputs(mesg, stderr);

    if( !dp ) return 1;

    fprintf(stderr,"gxml_data :\n"
                "   verb        : %d\n"
                "   dstore      : %d\n"
                "   indent      : %d\n"
                "   buf_size    : %d\n"
                "   b64_check   : %d\n"
                "   zlevel      : %d\n"
                "   da_len      : %d\n"
           , dp->verb, dp->dstore, dp->indent, dp->buf_size, dp->b64_check,
             dp->zlevel, dp->da_len);

    if( show_all )
        fprintf(stderr,
                "   da_list     : %p\n"
                "   da_ind      : %d\n"
                "   eleDA       : %d\n"
                "   expDA       : %d\n"
                "   b64_errors  : %d\n"
                "   errors      : %d\n"
                "   skip        : %d\n"
                "   depth       : %d\n"
                "   dind        : %lld\n"
                "   clen        : %d\n"
                "   doff        : %d\n"
                "   zlen        : %d\n"
                "   cdata       : %p\n"
                "   xdata       : %p\n"
                "   ddata       : %p\n"
                "   zdata       : %p\n"
                "   gim         : %p\n"
           , (void *)dp->da_list, dp->da_ind, dp->eleDA, dp->expDA,
             dp->b64_errors, dp->errors, dp->skip, dp->depth, dp->dind,
             dp->clen, dp->doff, dp->zlen,
             (void *)dp->cdata, (void *)dp->xdata, (void *)dp->ddata,
             (void *)dp->zdata, (void *)dp->gim);

    return 0;
}
