/*@Start***********************************************************/
/****************************************************************/
/*   This header file includes other header files!!!            */
/****************************************************************/
/* GEMSBG Include File
 * Copyright (C) 1987, 1988, 1989 The General Electric Company
 *      Include File Name:  PixelData.h
 \author WR Edens, JL Agle, MJ Benson
 * Source
 * $Revision$  $Date: 2003-06-22 19:49:12 $
 */
/*@Synopsis Description of the PixelData header
 */
/*@Description
 * Description of header for files containing deep pixel medical images
 *      The structures of this file are designed with 32 bit word
 *   word alignment in mind.  This is done to facilitate the easy
 *   access to the data within a 'SPARC' based processor as is found
 *   in the Genesis IP and Frame Buffer.  Additions and changes should
 *   reflect this design requirement on the structures of this file.
 *      An PixelData file contains only the information needed to handle the
 *   physical pixels of an image without concern for worldly relationships
 *   to what they represent.  There is a header, a couple of optional
 *   control tables, and values of data that are representations of the
 *   real pixel values of the image to be set into display hardware.  The
 *   Data Base Header stuff is a block of data that PixelData does NOT
 *   interpet, but keeps for the data base.  (This should only be filled
 *   in on the Optical Disk).
 *      These data values might need to be manipulated to get the real
 *   pixel values via uncompression and/or unpacking depending on the
 *   value of 'img_compress'.  If the file has both methods applied
 *   (img_compress == IC_COMPACK), the uncompression must be performed
 *   before unpacking.
 *      The header contains 'byte displacement' and 'byte length' entries
 *   to access the different control tables or data.  A control
 *   table does not exist if its 'byte length' is zero.  If the
 *   table exists, there is a structure that defines the secondary table.
 *   There is also an unfilled area between the last control table and
 *   Start of data.  This area is there to block align the beginning of
 *   the data.  This will allow for much better read and write performance
 *   when going to block oriented devices (the norm for our application).
 *   The size of this area will vary depending on which control tables
 *   are defined and their sizes and the sector size of the storage
 *   device.  It is important to note that this gap size can change
 *   when moving from one physical device to another if they have different
 *   sector sizes.
 *      One can find a description of the structure of each of the optional
 *   parts in the header file associated with that optional area.
 *   If the file is found to be IC_COMPRESSED or IC_COMPACK, the method is...
 *      The compression algorithm stores in the file a differential
 *   intensity value from the immediately preceeding pixel.  The value
 *   stored as a 7 bit 2's compliment number (8th msbit zero) byte if the
 *   difference is -64 to +63.  If the difference is -8192 to +8191, the
 *   first byte stored is the most significant byte of the 2's compliment
 *   value with the top two bits set to '10'.  If the difference exceeds
 *   13 bit magnitude, the first byte stored is '11xxxxxx' with the next
 *   two bytes containing the actual real pixel value.
 *   If the file is found to be IC_COMP2 or IC_CPK2, the method is ...
 *      The compression algorithm stores in the file a differential
 *   intensity value from the immediately preceeding pixel.  The value
 *   of the first byte being 128 (-128) indicates that the next two bytes
 *   are the actual real pixel value.  Otherwise, the value of the first
 *   byte will be an 8 bit 2's compliment number between -127 to +127 to
 *   be added the value of the previous pixel.  THESE ARE NOT CURRENTLY
 *   SUPPORTED IN GENESIS.
 *      The checksum method of the file as of this date (19-Jan-88) is:
 *   The number stored in 'img_checksum' is the 16 bit (u_short) sum
 *   of all the pixel data of the image adding in the overflows during
 *   the summation.  This is refered to as 'end-around-carry' summation
 *   so that the value of zero really (absolutely) means that there is
 *   no checksum computed for this image.  Note that the checksum is
 *   computed on the real original pixel data values of the image and
 *   not on the 'compressed' or 'compack'd data values in the file.
 *   This implies that the checksum of a rectangular version of a packed
 *   file may well be different from the packed version; since the
 *   background values get added in for the rectangular version and not
 *   for the packed version.
 * NOTES on the image header:
 *  img_magic =       a long integer value to indicate an 'imagefile'
 *  img_hdr_length =  length of all headers in bytes - points to pixels start
 *                      as a byte displacement to the 'pixel data area'
 *  img_width =       x-axis pixel count (256, 320, 512, 1024)
 *  img_height =      y-axis pixel count
 *  img_depth =       number of bits in an uncompressed pixel (1, 8, 16)
 *                      NOTE: NOT magnitude resolution (CT is 16, not 12)
 *  img_compress =    form of compression and packing applied to file (IC_*)
 *  img_dwindow =     default 'window' width (stored image value range)
 *  img_dlevel =      default 'level' value  (stored image value magnitude)
 *  img_bgshade =     default background shade for non-pixels during unpack
 *  img_ovrflow =     pixel value to substitute when overflow occurs in GIP
 *  img_undflow =     pixel value to substitue when underflow occurs in GIP
 *  img_top_offset =  number of lines without entries in 'line_length' table
 *                      at the top of the image.
 *  img_bot_offset =  number of lines without entries in 'line_length' table
 *                      at the bottom of the image.
 *  img_version =     the version of the header structure - initial = 0
 *                      this word is not processed by the IPlib!  Therefore
 *                      all changes to this header must be handled by
 *                      extension and not by alteration of the 3.1 version
 *                      of the PixHdr structure called version 0!
 *  img_checksum =    the 16 bit end-around-carry sum of true image pixels
 *                      (a value of zero indicates that the checksum is not
 *                        defined for this file.)
 *  img_p_id;         a byte displacement to unique image identifier
 *  img_l_id;         a byte length of unique image identifier
 *  img_p_unpack =    a byte displacement to the 'unpack control table'
 *  img_l_unpack =    byte length of the 'unpack control table'
 *  img_p_compress =  a byte displacement to the 'compression control table'
 *  img_l_compress =  byte length of the 'compression control table'
 *  img_p_histo =     a byte displacement to the 'histogram control data'
 *  img_l_histo =     byte length of the 'histogram control data'
 *  img_p_text =      a byte displacement to 'text plane data'
 *  img_l_text =      byte length of 'text plane data'
 *  img_p_graphics =  a byte displacement to 'graphics plane data'
 *  img_l_graphics =  byte length of 'graphics plane data'
 *  img_p_dbHdr =     a byte displacement to 'data base header data'
 *  img_l_dbHdr =     byte length of 'data base header data'
 *  img_levelOffset=  offset to be added to Pixel Values to get correct
 *                      presentation value
 *  img_p_user=       byte displacement to user defined data
 *  img_l_user=       byte length of user defined data
 *      Here is a picture to help visualize the structure of the header.
 *                      ---------------------------------
 *                      |  Magic Number                 |
 *                      ---------------------------------
 *                    --|  Header Length                |
 *                   |  ---------------------------------
 *                   |  |  lots of header stuff         |
 *                   |  ~                               ~
 *                   |  ~                               ~
 *                   |  ---------------------------------
 *                   |  |  Version      |  Checksum     |
 *                   |  ---------------------------------
 *                  -|--|  ID Pointer                   |
 *                 | |  ---------------------------------
 *                 | |  |  ID length                    |
 *                 | |  ---------------------------------
 *                -|-|--|  Unpack Table Pointer         |
 *               | | |  ---------------------------------
 *               | | |  |  Unpack Table length          |
 *               | | |  ---------------------------------
 *              -|-|-|--|  Compression Seed Table Ptr   |
 *             | | | |  ---------------------------------
 *             | | | |  |  Compression Seed Table length|
 *             | | | |  ---------------------------------
 *            -|-|-|-|--|  Histogram Table Pointer      |
 *           | | | | |  ---------------------------------
 *           | | | | |  |  Histogram Table length       |
 *           | | | | |  ---------------------------------
 *          -|-|-|-|-| --  Text Plane data Pointer      |
 *         | | | | | |  ---------------------------------
 *         | | | | | |  |  Text Plane data Length       |
 *         | | | | | |  ---------------------------------
 *        -|-|-|-|-|-| --  Graphics Plane data Pointer  |
 *       | | | | | | |  ---------------------------------
 *       | | | | | | |  |  Graphics Plane data Length   |
 *       | | | | | | |  ---------------------------------
 *      -|-|-|-|-|-|-| --  Data Base Header Pointer     |
 *     | | | | | | | |  ---------------------------------
 *     | | | | | | | |  |  Data Base Header Length      |
 *     | | | | | | | |  ---------------------------------
 *     | | | | | | | |  |  level Offset                 |
 *     | | | | | | | |  ---------------------------------
 *    -|-|-|-|-|-|-|-| --  User Defined Data Pointer    |
 *   | | | | | | | | |  ---------------------------------
 *   | | | | | | | | |  |  User Defined Data Length     |
 *   | | | | | | | | |  ---------------------------------
 *   | | | | | | | | |  |  some Spares                  |
 *   | | | | | | | | |  ~                               ~
 *   | | | | | | | | |  ~                               ~
 *   | | | | | | | | |  ---------------------------------\
 *   | | | | | | | |-|->~  ID stuff                     ~ \
 *   | | | | | | |   |  ~  See PDtext.h for details     ~ /  ID length
 *   | | | | | | |   |  ---------------------------------/\
 *   | | | | | | |---|->~  Unpack Table                 ~  \  Unpack Table
 *   | | | | | |     |  ~  Described below              ~  /   length
 *   | | | | | |     |  ---------------------------------\/
 *   | | | | | |-----|->~  Compression Seed Table       ~ \  Compression Seed
 *   | | | | |       |  ~  See PDcomp.h for details     ~ /   Table len
 *   | | | | |       |  ---------------------------------/\
 *   | | | | |-------|->~  Histogram Table              ~  \  Histogram Table
 *   | | | |         |  ~  See PDhisto.h for details    ~  /   length
 *   | | | |         |  ---------------------------------\/
 *   | | | |---------|->~  Text Plane data              ~ \  Text Plane data
 *   | | |           |  ~  See PD?????.h for details    ~ /   length
 *   | | |           |  ---------------------------------/\
 *   | | |-----------|->~  Graphics Plane data          ~  \  Graphics Plane
 *   | |             |  ~  See PD?????.h for details    ~  /   data length
 *   | |             |  ---------------------------------\/
 *   | |-------------|->~  Data Base Header             ~ \  Data Base Header
 *   |               |  ~  See PD?????.h for details    ~ /   length
 *   |               |  ---------------------------------/\
 *   |---------------|->~  User Defined Data            ~  \ User Defined Data
 *                   |  ~  See PD????.h for details     ~  /  length
 *                   |  --------------------------------- /
 *                   |  ~       Block Alignment Gap     ~
 *                   |  ~                               ~
 *                   |  ---------------------------------
 *                   |->~       Pixel Data              ~
 *                      ~                               ~
 *                      ---------------------------------
 *      If any of the Tables is of zero length, the pointer to that table
 *   and the pointer to the next table would both point to the same place.
 *   The order above is what one will typically find, but one must follow
 *   the pointers and use the lengths to find the tables.  There is no
 *   requirement that they be in this order.
 */
/*@End*********************************************************/
/* only do this once in any given compilation.*/
#ifndef PIXELDATA_INCLUDE
#define PIXELDATA_INCLUDE
namespace itk { // keep this stuff from polluting ITK client program namespaces
/* Add other declaration type for pixel header - Dominic H. Nguyen */
typedef struct dcmp_t
{
  int nextHtEntry;
  int nextPixel;
  int base;
}
dcmp_t;

struct unpack
{
  short up_left;              /* pixels to the left of the image */
  short up_image;             /*   "    within the image         */
};

typedef unsigned char PIXEL;

#define     DC_NOOP                 101
#define     DC_NO_NEED_TO_DC        DC_NOOP
#define     DC_NO_MORE_PIXEL        102
#define     DC_SUCCESS              0
#define     DC_DONE                 1
#define     DC_FAILURE             -1
#define     DC_UNRECOGNIZE_CODE    -101
#define     DC_NO_UNPACK_TABLE     -102
#define     DC_NEED_INPUT          -201
#define     DC_NEED_OUTPUT         -202
/* end - Dominic H. Nguyen */
typedef struct pixhdr
{
  int img_magic;              /* magic number */
  int img_hdr_length;         /* length of total header in bytes and
                                   a byte displacement to the 'pixel data area' */
  int img_width;              /* width (pixels) of image */
  int img_height;             /* height (pixels) of image */
  int img_depth;              /* depth (1, 8, 16, or 24 bits) of pixel */
  int img_compress;           /* type of compression; see IC_* below */
  int img_dwindow;            /* default window setting */
  int img_dlevel;             /* default level setting */
  int img_bgshade;            /* background shade to use for non-image */
  int img_ovrflow;            /* overflow value */
  int img_undflow;            /* underflow value */
  int img_top_offset;         /* number of blank lines at image top */
  int img_bot_offset;         /* number of blank lines at image bottom */
  short img_version;          /* version of the header structure */
  /* and a word to maintain 32 bit alignment */
  unsigned short img_checksum; /* 16 bit end_around_carry sum of pixels */
  int img_p_id;               /* a byte disp to unique image identifier */
  int img_l_id;               /* byte length of unique image identifier */
  int img_p_unpack;           /* a byte disp to 'unpack control' */
  int img_l_unpack;           /* byte length of 'unpack control' */
  int img_p_compress;         /* a byte disp to 'compression control' */
  int img_l_compress;         /* byte length of 'compression control' */
  int img_p_histo;            /* a byte disp to 'histogram control' */
  int img_l_histo;            /* byte length of 'histogram control' */
  int img_p_text;             /* a byte disp to 'text plane data' */
  int img_l_text;             /* byte length of 'text plane data' */
  int img_p_graphics;         /* a byte disp to 'graphics plane data' */
  int img_l_graphics;         /* byte length of 'graphics plane data' */
  int img_p_dbHdr;            /* a byte disp to 'data base header data' */
  int img_l_dbHdr;            /* byte length of 'data base header data' */
  int img_levelOffset;        /* value to add to stored Pixel Data values */
  /* to get the correct presentation value */
  int img_p_user;             /* byte displacement to user defined data */
  int img_l_user;             /* byte length of user defined data */
  int img_p_suite;            /* byte displacement to suite header data */
  int img_l_suite;            /* byte length of suite defined data */
  int img_p_exam;             /* byte displacement to exam header data */
  int img_l_exam;             /* byte length of exam defined data */
  int img_p_series;           /* byte displacement to series header data */
  int img_l_series;           /* byte length of series defined data */
  int img_p_image;            /* byte displacement to image header data */
  int img_l_image;            /* byte length of image defined data */
}
PixHdr;

#define IMG_MAGIC       0x494d4746      /*    this number assigned by GEMS */
#define IMG_HDR_VERSION 3       /* this number raised by GEMS when their
                                   version of the PixHdr structure is changed
                                   or when the format of the unpack table is
                                   changed.  These two are tied together
                                   because they are both used by the IP library
                                   calls.  Changes to either of these structure
                                   formats could cause IP library problems.
                                   ANY CHANGE TO PixHdr OR PixUpk SHOULD CAUSE
                                   IMG_HDR_VERSION TO CHANGE!! */
/* GEMS compress rule to set into 'img_compress'  */
/*  end of header portion */
/* ========== 'unpack control' structures pointed to by 'img_p_unpack'
   *      This table does not have a version number independent of the
   *   PixHdr portion of the header since the IP library expects both
   *   of them to be of a certain format.  We have more freedom with the
   *   other structures, since no non-Genesis code has expectations of
   *   the format.
   *      What is actually stored with the data set is:
   *                      PixUpk  up[(img_l_unpack/sizeof(PixUpk))];
   *      This typically will not exceed 1024 entries.
   *      If one were to malloc this space a useful thing to have around
   *   might be:          PixUpk  *up_ptr;
   *   but we use it as an exercise for the user to understand how this
   *   pointer may be of help.
   *      The 'unpack control' of the file is a new format table that
   *   is compatible with the 'Genesis' hardware requirements.
   *      This table exists in the file if the file is of 'img_compress'
   *   type IC_PACKED, IC_COMPACK, or IC_CPK2.
   *      The number of entry pairs in the table is 'img_height'.
   *   Each pair of (short) entries in the 'line_length' table indicates
   *   the number of pixels at the beginning of the image line to be filled
   *   with the background shade value, and the actual count of pixel values
   *   from the pixel data to be put into the image line.  The image line is
   *   to be filled out with the background shade value to the limit of
   *   'img_width'.
   *      Consider the following few examples.  Assume that the width of
   *   the PixelData is 10 pixels and that an 'x' is a background value
   *   and 'y' is an image value.
   *      xxxxxxxxxx      up_left = 10 up_image = 0  (atypical case,
   *                                                  causes a blank line)
   *      xxxyyyyxxx      up_left = 3  up_image = 4
   *      xxyyyyyxxx      up_left = 2  up_image = 5
   *      xxxyyyyyyx      up_left = 3  up_image = 6
   *      yyyyyyyyyy      up_left = 0  up_image = 10
   */
typedef struct
{
  short up_left;              /* pixels to the left of the image */
  short up_image;             /* pixels within the image line */
}
PixUpk;

/*      The data area of the file is pixel data stored sequentially
   *   starting in the Upper Left Hand Corner (ULHC) of the image filling
   *   the x_axis 'img_width' for each line and continuing down the y_axis
   *   for 'img_height' lines.
   */
#endif                          /* PIXELDATA_INCLUDE */
/*@Start***********************************************************/
/* GEMSBG Include File
   * Copyright (C) 1988 GE Medical Systems
   *      Include File Name:      phonebook
   \author David Carleton
   * Source
   * $Revision$  $Date: 2003-06-22 19:49:12 $
   */
/*@Synopsis     Contains defaults and strings for the Phone Book feature.
   */
/*@Description
    The defaults and strings necessary for the Phone Book feature.
  */
/*@End*********************************************************/
/* only do this once in any given compilation.*/
#ifndef  PB_INCL
#define  PB_INCL

#ifndef lint
/*
    static char pb_sccsid[] = "@(#)phonebook.h      1.6 7/11/90 16:55:37 Copyright 1988 GEMSBG";
  */
#endif

/*============================================================================*/
/* Defines for Phone Book.                                                    */
/*============================================================================*/
/* Bit fields for request flags */
/* Request flag is a long int   */
#define MAGDISK         0x00000001      /* Media type in bits 0-15 */
#define OPTDISK         0x00000002

#define RPMAN           0x00010000      /* Service type in bits 16-31 */
#define DBSER           0x00020000

/* Structure to contain a single phone book entry */
struct PBInfo
{
  int diskType;
  char dispID;
  char dbserv[20];
  char rpman[20];
};

typedef struct PBInfo BookEntry_t;

/* Set names for service processes */
/* Mag DB Servers */
#define DB0     "dbserver"
#define DB1     "dbserver1"
#define DB2     "dbserver2"
#define DB3     "dbserver3"
#define DB4     "dbserver4"

/* Optical DB Servers */
#define ODB0    "odbrpm0"
#define ODB1    "odbrpm1"
#define ODB2    "odbrpm2"
#define ODB3    "odbrpm3"
#define ODB4    "odbrpm4"

/* Mag RPM */
#define RPM0    "mrpm"
#define RPM1    "mrpm1"
#define RPM2    "mrpm2"
#define RPM3    "mrpm3"
#define RPM4    "mrpm4"

/* Optical Mag RPM */
#define ORPM0   "odbrpm0"
#define ORPM1   "odbrpm1"
#define ORPM2   "odbrpm2"
#define ORPM3   "odbrpm3"
#define ORPM4   "odbrpm4"

#ifdef OMIT_THIS_CODE
static char *mag_rpm[5] = {
  RPM0,
  RPM1,
  RPM2,
  RPM3,
  RPM4,
};

static char *opt_rpm[5] = {
  ORPM0,
  ORPM1,
  ORPM2,
  ORPM3,
  ORPM4,
};

static char *mag_dbs[5] = {
  DB0,
  DB1,
  DB2,
  DB3,
  DB4,
};

static char *opt_dbs[5] = {
  ODB0,
  ODB1,
  ODB2,
  ODB3,
  ODB4,
};
#endif

#define DBSERVICE "theDBServer"
#define RPMSERVICE "theRpm"
#define PBVERSION 1
#define PBVERSIONSTR "1"

/* FIX ME -- GET THESE FROM A CONFIG FILE */
#define MAXMAG 1
#define MAXOPT 2

#define MAG_DB  0
#define OPT_DB  1
}
#endif                          /* PB_INCL */
