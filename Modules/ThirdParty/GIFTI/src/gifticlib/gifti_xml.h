#ifndef GIFTI_XML_H
#define GIFTI_XML_H

#define GXML_MAX_DEPTH 10    /* maximum stack depth */
#define GXML_MAX_ELEN  128   /* maximum element length */

#define GIFTI_XML_VERSION       "1.0"
#define GIFTI_XML_ENCODING      "UTF-8"
                                /* use non-changing address  2 Mar 2010 */
#define GIFTI_XML_DTD_SOURCE    "http://gifti.projects.nitrc.org/gifti.dtd"

/* ----------------------------------------------------------------------
   element      depths  parent(s)       children
   -------      ------  --------------  -----------------------
   GIFTI        0                       MetaData, LabelTable, DataArray
   MetaData     1       GIFTI           MD
                2       DataArray
   MD           2,+1    MetaData        Name, Value
   Name         3,+1    MD              CDATA/char
   Value        3,+1    MD              CDATA/char
   LabelTable   1       GIFTI           Label
   Label        2       LabelTable      CDATA/char

   DataArray    1       GIFTI           MetaData, CSTM, Data
   CSTM         2       DataArray       DataSpace, TransformedSpace, MatrixData
   Data         2       DataArray
   DataSpace    3       CSTM            CDATA/char
   TransformedSpace  3  CSTM            CDATA/char
   MatrixData   3       CSTM            char


   CDATA        4,+1    Name, Value     char
                4       DataSpace       char
                4       TransformedSpace char
   char         any     any             whitespace
                5       CDATA

   -- other objects to handle --
   XML declaration:     version, encoding, standalone
   DOCTYPE:             type=GIFTI, sid=.../gifti.dtd, pid, sub
   default:
   ----------------------------------------------------------------------
*/

/* this list must match enames, and is ordered via the above comment */
#define GXML_ETYPE_INVALID      0
#define GXML_ETYPE_GIFTI        1      /* GIFTI element            */
#define GXML_ETYPE_META         2      /* MetaData element         */
#define GXML_ETYPE_MD           3      /* MD element               */
#define GXML_ETYPE_NAME         4      /* Name element             */
#define GXML_ETYPE_VALUE        5      /* Value element            */
#define GXML_ETYPE_LABELTABLE   6      /* LabelTable element       */
#define GXML_ETYPE_LABEL        7      /* Label element            */
#define GXML_ETYPE_DATAARRAY    8      /* DataArray element        */
#define GXML_ETYPE_CSTM         9      /* CSTM element             */
#define GXML_ETYPE_DATA        10      /* Data element             */
#define GXML_ETYPE_DATASPACE   11      /* DataSpace element        */
#define GXML_ETYPE_XFORMSPACE  12      /* TransformedSpace element */
#define GXML_ETYPE_MATRIXDATA  13      /* MatrixData element       */
#define GXML_ETYPE_CDATA       14      /* CDATA element            */
#define GXML_ETYPE_LAST        14      /* should match last entry  */

typedef struct {
    long long   nalloc;                 /* allocation length    */
    long long   nused;                  /* number of bytes used */
    char      * buf;                    /* buffer               */
} gxml_buffer;

typedef struct {
    int            verb;            /* verbose level                */
    int            dstore;          /* flag: store data             */
    int            indent;          /* spaces per depth level       */
    int            buf_size;        /* for XML buffer               */
    int            b64_check;       /* 0=no, 1=check, 2=count, 3=skip */
    int            update_ok;       /* library can update metadata  */
    int            zlevel;          /* compression level -1..9      */

    int          * da_list;         /* DA index list to store       */
    int            da_len;          /* DA index list length         */
    int            da_ind;          /* current DA index list index  */

    int            eleDA;           /* number of elements found     */
    int            expDA;           /* number of elements expected  */
    int            b64_errors;      /* bad chars, per DATA element  */
    int            errors;          /* number of errors encountered */
    int            skip;            /* stack depth to skip          */
    int            depth;           /* current stack depth          */
    int            stack[GXML_MAX_DEPTH+1]; /* stack of etypes      */

    long long      dind;            /* index into data->data/xform  */
    int            clen;            /* length of current CDATA      */
    int            xlen;            /* length of xform buffer       */
    int            dlen;            /* length of Data buffer        */
    int            doff;            /* offset into data buffer      */
    int            zlen;            /* length of compression buffer */
    char        ** cdata;           /* pointer to current CDATA     */
    char         * xdata;           /* xform buffer                 */
    char         * ddata;           /* I/O buffer xml->ddata->data  */
    char         * zdata;           /* zlib compression buffer      */
    gifti_image  * gim;             /* pointer to returning image   */
} gxml_data;

/* protos */

/* main interface */
gifti_image * gxml_read_image (const char * fname, int read_data,
                               const int * dalist, int len);
int           gxml_write_image(gifti_image * gim, const char * fname,
                               int write_data);

int   gxml_set_verb        ( int val );
int   gxml_get_verb        ( void    );
int   gxml_set_dstore      ( int val );
int   gxml_get_dstore      ( void    );
int   gxml_set_indent      ( int val );
int   gxml_get_indent      ( void    );
int   gxml_set_buf_size    ( int val );
int   gxml_get_buf_size    ( void    );
int   gxml_set_b64_check   ( int val );
int   gxml_get_b64_check   ( void    );
int   gxml_set_update_ok   ( int val );
int   gxml_get_update_ok   ( void    );
int   gxml_set_zlevel      ( int val );
int   gxml_get_zlevel      ( void    );


#endif /* GIFTI_XML_H */
