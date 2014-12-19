/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*
 *
 * Copyright (c) 1986-2006
 * Biomedical Imaging Resource
 * Mayo Clinic
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 *
 * 1) Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2) Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * Neither the name of the Mayo Clinic nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */


/**
 * \file itkAnalyzeDbh.h
 * This file contains notes about the Analyze 7.5 file format gathered from
 * several sources. A special note of thanks to Dennis P. Hanson for his
 * generous contributions in getting this information correct.
 * - NOTE: The comments were addded as part of the Insight Segmentation
 *         And Registration Toolkit.
 *
 * Function:      This file contains the structure definition for Analyze files
 */


#ifndef itkAnalyzeDbh_h
#define itkAnalyzeDbh_h

namespace itk
{
/**
   *  \enum DataTypeKeyValues
   * Acceptable values for hdr.dime.datatype
   */
enum DataTypeKeyValues  {
  ANALYZE_DT_UNKNOWN      =0,  /**< Deontes that the data type is unknon */
  ANALYZE_DT_BINARY       =1,  /**< Deontes that the data type is binary */
  ANALYZE_DT_UNSIGNED_CHAR=2,  /**< Deontes that the data type is unsigned char */
  ANALYZE_DT_SIGNED_SHORT =4,  /**< Deontes that the data type is signed short */
  ANALYZE_DT_SIGNED_INT   =8,  /**< Deontes that the data type is signed int */
  ANALYZE_DT_FLOAT        =16, /**< Deontes that the data type is single precision floating point */
  ANALYZE_DT_COMPLEX      =32, /**< Deontes that the data type is pairs of single precision floating point numbers */
  ANALYZE_DT_DOUBLE       =64, /**< Deontes that the data type is double precision floating point */
  ANALYZE_DT_RGB          =128,/**< Deontes that the data type is triples of unsigned char */
  ANALYZE_DT_ALL          =255,/**< Deontes that the data type is unknon */
  //Obsolete, using SPM, B2ANALYZE_DT_UNSIGNED_SHORT =6,  /**< Deontes that the data type is unsigned short in brains2 analyze extensions*/
  //Obsolete, using SPM, B2ANALYZE_DT_UNSIGNED_INT   =12, /**< Deontes that the data type is unsigned int in brains2 analyze extensions*/
  SPMANALYZE_DT_UNSIGNED_SHORT=132,/**< Deontes that the data type is unsigned short in SPM analyze extensions*/
  SPMANALYZE_DT_UNSIGNED_INT  =136 /**< Deontes that the data type is unsigned int in SPM analyze extensions*/
};

/**
   *  \enum DataTypeIndex
   *  The index into the DataTypes array for each type.
   */
enum DataTypeIndex  {
  ANALYZE_DT_INDEX_UNKNOWN       =0,
  ANALYZE_DT_INDEX_BINARY        =1,
  ANALYZE_DT_INDEX_UNSIGNED_CHAR =2,
  ANALYZE_DT_INDEX_SIGNED_SHORT  =3,
  ANALYZE_DT_INDEX_SIGNED_INT    =4,
  ANALYZE_DT_INDEX_FLOAT         =5,
  ANALYZE_DT_INDEX_COMPLEX       =6,
  ANALYZE_DT_INDEX_DOUBLE        =7,
  ANALYZE_DT_INDEX_RGB           =8,
  ANALYZE_DT_INDEX_ALL           =9,
  //Obsolete, using SPM, B2ANALYZE_DT_INDEX_UNSIGNED_SHORT=10,
  //Obsolete, using SPM, B2ANALYZE_DT_INDEX_UNSIGNED_INT  =11,
  SPMANALYZE_DT_INDEX_UNSIGNED_SHORT   =10,
  SPMANALYZE_DT_INDEX_UNSIGNED_INT     =11
};


/**
   * \var DataTypes
   * An array of the Analyze v7.5 known DataTypes
   * - 0-->"UNKNOWN"
   * - 1-->"BINARY"
   * - 2-->"CHAR"
   * - 3-->"SHORT"
   * - 4-->"INT"
   * - 5-->"FLOAT"
   * - 6-->"COMPLEX"
   * - 7-->"DOUBLE"
   * - 8-->"RGB"
   * - 9-->"ALL"
   * - 10-->"USHORT"
   * - 11-->"UINT"
   * @see DataTypes
   * @see DataTypeSizes
   * @see DataTypeKey
   */
extern const char DataTypes[12][10];

/**
   * \var DataTypeSizes
   * An array with the corresponding number of bits for each image type.
   * - 0-->0
   * - 1-->1
   * - 2-->8
   * - 3-->16
   * - 4-->32
   * - 5-->32
   * - 6-->64
   * - 7-->64
   * - 8-->24
   * - 9-->0
   * - 10-->16
   * - 11->32
   * @see DataTypes
   * @see DataTypeSizes
   * @see DataTypeKey
   */
extern const short int DataTypeSizes[12];

/**
   * \var DataTypeKey
   * An array with Data type key sizes
   * - 0-->ANALYZE_DT_UNKNOWN
   * - 1-->ANALYZE_DT_BINARY
   * - 2-->ANALYZE_DT_UNSIGNED_CHAR
   * - 3-->ANALYZE_DT_SIGNED_SHORT
   * - 4-->ANALYZE_DT_SIGNED_INT
   * - 5-->ANALYZE_DT_FLOAT
   * - 6-->ANALYZE_DT_COMPLEX
   * - 7-->ANALYZE_DT_DOUBLE
   * - 8-->ANALYZE_DT_RGB
   * - 9-->ANALYZE_DT_ALL
   * - 10-->SPMANALYZE_DT_UNSIGNED_SHORT
   * - 11-->SPMANALYZE_DT_UNSIGNED_INT
   * @see DataTypes
   * @see DataTypeSizes
   * @see DataTypeKey
   */
extern const short int DataTypeKey[12];

/**
   * \struct header_key
   * - (c) Copyright, 1986-1995
   * - Biomedical Imaging Resource
   * - Mayo Foundation
   */
struct header_key              /*      header_key       */
{/*off + size*/
  /*0 + 4  */int sizeof_hdr;         /**< Must indicate the byte size of header file, Almost always 348 bytes, but
                                         may be larger for sites that have extended the default implementation.
                                         This feild is used by analyze to determine if the file is big endian or
                                         little endian. If the size of the *.hdr file does not equal this value,
                                         then the structure needs to be byte swapped. */
  /*4 + 10 */char data_type[10];     /**< A convenience character string that has a 1 to 1 correspondence
                                         with the short int datatype field of the image_dimension.
                                         @see DataTypes*/
  /*14 + 18*/char db_name[18];       /**< A convenience character string that should be the same as the
                                         the file name without th e*.hdr or *.img extension. */
  /*32 + 4 */int extents;            /**< Should be 16384, the image file is created as contiguous with a
                                         minium extent size.  This field may be used to check endedness of
                                         the file */
  /*36 + 2 */short int session_error;/**< This feild is not used for anything other than internal use in Pre 1995
                                         versions of Analyze.  Setting this to 0 causes no problems. */
  /*38 + 1 */char regular;           /**< This must be 'r' to indicate that all images and volumes are the same size. */
  /*39 + 1 */char hkey_un0;          /**< Unused field for future expansion. */
};/* total=40  */
  /**
   * \struct image_dimension struct describes the organization and
   *    size of images. These elements enable IO routines to reference
   *    images by volume and slice number.
   * - (c) Copyright, 1986-1995
   * - Biomedical Imaging Resource
   * - Mayo Foundation
   */
struct image_dimension         /*      image_dimension  */
{                              /* off + size*/
  /*0 + 16 */ short int dim[8];  /**< Array of image dimensions
                                     - dim[0]    number of dimensions; usually 4
                                     - dim[1]    image X dimension, i.e. number of voxels per row (adjacent memory locations)
                                     - dim[2]    image Y dimension, i.e. number of rows per slice
                                     - dim[3]    Volume Z dimension, i.e. number of slices per volume
                                     - dim[4]    Number of time points, i.e. number of volumes per series
                                     -   . . .
                                     - dim[7]    volumes in file, i.e. number of volumes per series
                                    */
  /*16 + 4 */ char vox_units[4]; /**< Specifies the spatial units of measure for a voxel, valid values are "mm","cm" and "in".
NOTE:  if no match is found, "mm" is assumed. */
  /*20 + 4 */ char cal_units[8]; /**< Specifies the name of the calibration unit, valid values are "mm","cm" and "in".
NOTE: if no match is found, "mm" is assumed. */
  /*24 + 2 */ short int unused1; /**<  Unused field for future expansion. */
  /*30 + 2 */ short int datatype;/**<  A short int that defines the type of data being stored.
NOTE: THIS MUST BE FILLED IN.  This is the field that most applications
use to determine the data storage type.
Valid values are defined by DataTypeKeyValues.  @see DataTypeKeyValues */
  /*32 + 2 */ short int bitpix;  /**<  Bits per pixel.  This field must agree with the datatype and
                                     data_type fields.    @see DataTypeSizes */
  /*34 + 2 */ short int dim_un0; /**< Unused value for future expansion. */
  /*36 + 32*/ float pixdim[8];   /**< Parallel array to dim giving voxel dimensions
NOTE: pixdim[0] is ignored, and the number of dims are taken from dims[0],
and pixdim[1..7] are the actual pixdims.
- pixdim[0]  this field is not used
- pixdim[1]    voxel width
- pixdim[2]    voxel height
- pixdim[3]    voxel depth or slice thickness
- pixdim[4]
- . . .
- pixdim[7]
                                    */
  /*68 + 4 */ float vox_offset;  /**< Byte offset in the .img file at which voxels start. If value is
                                     negative specifies that the absolute value is applied for every
                                     image in the file. */
  /*72 + 4 */ float roi_scale;   /**< The "ROI Scale Factor" was really added in by the developers of SPM.
                                     This is used as a multiplicative factor to scale all values measured
                                     in the ROI module by this multiplicative factor.  This really
                                     shouldn't be set by any other application outside of Analyze and SPM.
                                    */
  /*76 + 4 */ float funused1;    /**< Unused, for future expansion */
  /*80 + 4 */ float funused2;    /**< Unused, for future expansion */
  /*84 + 4 */ float cal_max;     /**< The parameters 'cal_max' and 'cal_min' provided a mechanism for rescaling
                                     the voxel values into a different range of value representation. This string
                                     allowed whatever the range of value representation was to be expressed with these
                                     string characters. Never really used in a robust manner. */
  /*88 + 4 */ float cal_min;     /**< @see image_dimension::cal_max */
  /*92 + 4 */ int compressed;    /**< Valid values are only 0 and 1.  A 0 value indicates that the file is
                                     uncompressed.  A 1 value indicates that the *.img file has been compress
                                     with the standard Unix 'compress' utility.
                                     - filename, compressed value, description
                                     - basename.img,    0, uncompressed image file of size
                                     - basename.img,    1, compressed image file using unix 'compress' utility
                                     - basename.img.Z,  0, compressed image using unix 'compress' utiltiy
                                     - basename.img.gz, 0, compressed image file gzip utility
                                     - basename.img.Z,  1, invalid setting
                                     - basename.img.gz, 1, invalid setting
                                    */
  /*96 + 4 */ int verified;      /**< This was only used internally by Pre 1995 versions of analyze. */
  /*100 + 4*/ int glmax;         /**< This is the global max/min for the entire volume image (or sets of volume
                                     images if there are multiple volumes in this file).  The cal_max/min are
                                     used only if the desired representation range of the data is different from
                                     the true range of the data.  For example, CT data may be stored in signed
                                     shorts with a range from 0 - 2000, but an appropriate representation range
                                     for these values in in Hounsfield units from -1000 to 1000.  Any reported
                                     values in Analyze were scaled via this value scaling function.
                                     UNSIGNED INT and COMPLEX were really not supported under ANALYZE 7.5.,
                                     so these should be avoided in the Analyze 7.5 file format.  The max/min
                                     for FLOATs was the nearest integer to the floating point values. */
  /*104 + 4*/ int glmin;         /**< @see image_dimension::glmax */
}/*total=108 */;

/**
   * \struct data_history
   * Most of these are historical, relevant to use with the Dynamic Spatial
   * Reconstructor scanner developed at the Mayo BIR in the late 70's and through
   * the 80's.  However, the 'orient' field is very important and
   * is used to indicate individual slice orientation and determines whether
   * the Analyze 'Movie' program will attempt to flip the images before
   * displaying a movie sequence.  It is perfectly acceptable to put any
   * values you want into those fields because they do not(should not)
   * affect image processing.
   * - (c) Copyright, 1986-1995
   * - Biomedical Imaging Resource
   * - Mayo Foundation
   */
struct data_history   /* data_history     */
{/*off + size*/
  /*0 + 80  */char descrip[80];   /**<  A place to put a short description of the data */
  /*80 + 24 */char aux_file[24];  /**<  A place to put the name of an auxiliary file to
                                      use instead of the default .img file.  This
                                      is not currently used by the Analyze program. */
  /*104 + 1 */char orient;        /**< The 'orient' field in the data_history structure specifies the primary
                                      orientation of the data as it is stored in the file on disk.  This
                                      usually corresponds to the orientation in the plane of acquisition,
                                      given that this would correspond to the order in which the data is
                                      written to disk by the scanner or other software application.

                                      It would be vary rare that you would ever encounter any old ANALYZE 7.5
                                      files that contain values of 'orient' which indicate that the data has been
                                      'flipped'.  The 'flipped flag' values were really only used internal to the
                                      Analyze program to precondition data for fast display in the Movie module,
                                      where the images were actually flipped vertically in order to accommodate
                                      the raster paint order on older graphics devices.  The only cases you will
                                      encounter will have values of 0, 1, or 2.

                                      - hdr->orient  "MayoClinic/Analyze"   Origin   dims[1]  dims[2]  dims[3]
                                      - ======================================================================
                                      - 0            transverse-unflipped   IRP       R->L     P->A    I->S
                                      - 1            coronal-unflipped      IRP       R->L     I->S    P->A
                                      - 2            sagittal-unflipped     IRP       P->A     I->S    R->L
                                      - 3            transverse-flipped     IRA       R->L     A->P    I->S
                                      - 4            coronal-flipped        SRP       R->L     S->I    P->A
                                      - 5            sagittal-flipped       ILP       P->A     I->S    L->R
                                      - Where the Origin disignators are with respect to the patient
                                      - [(I)nferior|(S)uperior] [(L}eft|(R)ight] [(A)nterior|(P)osterior]

                                      SPECIAL NOTE:  THE BEHAVIOR OF THIS IO ROUTINE DIFFERS FROM THAT OF ANALYZE!
                                      NO RE_ORIENTATION OF THE DATA WILL OCCUR IN THE ITK IMPLEMENTATION.
                                      Upon loading into the Analyze program, all data is reoriented into the
                                      3D Analyze Coordinate System.
                                      The orientation of patient anatomy in the 3D Analyze Coordinate System
                                      has a fixed orientation relative to each of the orthogonal axes.
                                      This coordinate system does fix the origin in the subject's
                                      Inferior-Right-Posterior location relative to anatomy, with a
                                      left-handed coordinate system for specification of the other
                                      anatomy relative to the axes as given here:
                                      - X-Y plane is Transverse
                                      - X-Z plane is Coronal
                                      - Y-Z plane is Sagittal
where:
- X axis runs from patient right (low X) to patient left (high X)
- Y axis runs from posterior (low Y) to anterior (high Y)
- Z axis runs from inferior (low Z) to superior (high Z)
                                     */
  /*105 + 10*/char originator[10];/**< The person or group that generated this image */
  /*115 + 10*/char generated[10]; /**< The date the image was generated. */
  /*125 + 10*/char scannum[10];   /**< An instituion independent identifier for the image */
  /*135 + 10*/char patient_id[10];/**< An instituion independent identifier for the subject of the scan */
  /*145 + 10*/char exp_date[10];  /**< Experiment date - these were used for DSR experiments */
  /*155 + 10*/char exp_time[10];  /**< Experiment time - these were used for DSR experiments */
  /*165 + 3 */char hist_un0[3];   /**< Unused padding of the structure */
  /*168 + 4 */int views;          /**< Number of views in reconstruction - these were used for DSR experiments */
  /*172 + 4 */int vols_added;     /**< Number of time points summed together - these were used for DSR experiments */
  /*176 + 4 */int start_field;    /**< Video field for first view used in reconstruction - these were used for DSR experiments */
  /*180 + 4 */int field_skip;     /**< Video field skip factor used in the reconstructed image - these were used for DSR experiments */
  /*184 + 4 */int omax;           /**< The omax/omin and smax/smin relate to rescaling of
                                      the value range via the Load process, where both the
                                      'original' omax/omin and the 'scaled' omax/omin could
                                      be set to rescale the value range of the data when it is loaded. */
  /*188 + 4 */int omin;           /**< @see data_history::omax */
  /*192 + 4 */int smax;           /**< @see data_history::omax */
  /*196 + 4 */int smin;           /**< @see data_history::omax */
};/* total=200 */

/**
 * \struct dsr
 * Analyze 7.5 header structure
 * The header file is a 'C' structure which describes the dimensions
 * and properties of the voxel data.
 * NOTE: Values in convenience fields should have no
 *       effect on how this file is interpreted.
 *       These fields are not used read by the Analyze Program,
 *       but are written as a convenience for interpreting the
 *       file without an analyze compliant file reader.
 * - (c) Copyright, 1986-1995
 * - Biomedical Imaging Resource
 * - Mayo Foundation
 */
struct dsr/*      dsr              */
{/* off + size*/
  /*0 + 40   */ struct header_key hk;        /**<  The header_key structure. @see header_key */
  /*40 + 108 */ struct image_dimension dime; /**<  The image_dimension structure. @see image_dimension */
  /*148 + 200*/ struct data_history hist;    /**<   The data_history structure. @see data_history */
}/*total=348*/;
} //End namespace itk
#endif                           /* __dbh_h__ */
