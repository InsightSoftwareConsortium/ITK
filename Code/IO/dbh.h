/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    dbh.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __dbh_h__
#define __dbh_h__

#ifdef  __cplusplus
extern "C"
{
#endif
  /*************************************************************************
   * Programmer:    Hans J. Johnson  The following are notes gathered from several sources
   * e-mail:        hans-johnson@uiowa.edu
   * Organization:  The University of Iowa
   * Function:      This file contains the structure definition for Analyze files
   **************************************************************************/
  /* Analyze 7.5 From Mayo  --Comments From Medical image formats web page in 1999:
   * -----------------------------------------------------------------------------------------
   * This very popular software package is produced by the Biomedical Imaging Resource
   * group at the Mayo Clinic/Foundation. I have always thought they should give it away
   * but they don't, it is moderately expensive, though less so than some other alternatives.
   * If you want to test or buy it try contacting
   * Denny Hanson <A HREF="mailto:dph@mayo.edu">dph@mayo.edu</A> who is extremely helpful.
   * See also the web site at <A HREF="part7.html#AnalyzeFromMayoWebSite">Analyze from Mayo</A>.
   * Anyway, importing images into Analyze is a drag and you have to convert your files to their
   * format, but it isn't very difficult. I hear that some other programs also use their format
   * but haven't encountered them myself. Anyway, the package is sufficiently commonly used that
   * it seems appropriate to include the format here.
   * This information is included verbatim from what was sent to me by
   * Ellis Workman <A HREF="mailto:elw@mayo.edu">elw@mayo.edu</A> and if you have problems I
   * am sure he will be able to help.
   * -------------------------------------------------------------------------------------------
   */
  /*
   * Analyze IMAGE FILE FORMAT -- As much information as I can determine from the Medical image
   * formats web site, and the Analyze75.pdf file provided from the Mayo clinic.
   *
   * Analyze image file sets consist of at least 2 files:
   * REQUIRED:
   *    - an image file  ([basename].img or [basename].img.gz or [basename].img.Z)
   *          This contains the binary represenation of the raw voxel values.  If the file 
   *          is compressed,
   *          it should be of of size (sizeof(storagetype)*NX*NY*NZ(*NT).
   *          The format of the image file is very simple; containing usually
   *          uncompressed voxel data for the images in one of the several
   *          possible voxel formats:
   *             - 1 bit  packed binary (slices begin on byte boundaries)
   *             - 8 bit  (unsigned char) gray scale unless .lkup file present
   *             - 16 bit signed short
   *             - 32 bit signed integers or float
   *             - 24 bit RGB, 8 bits per channel
   *    - a header file  ([basename].hdr)
   *          This a 348byte file that contains a binary represenation of the C-struct 
   *          defined in this file
   * OPTIONAL:
   *    - a color lookup file ([basename].lkup)
   *      This is only relevant for unsigned char images.  The .lkup file conatins 
   *      255*3 floating point numbers between the value of (0.0F-1.0F) that can be 
   *      read directly into an array float [255][3].  Each element of the array represents 
   *      the Red Green and Blue components to be associated with the unsigned char value 
   *      at that point.
   *    - a vol file ([basename].vol)
   *    - an object file ([basename].obj)
   *      A specially formated file with a mapping between object name and image code used to associate
   *      image voxel locations with a label.  This file is run length encoded to save disk storage.
   */
  /*
   * Analyze 7.5 header structure
   * The header file is a 'C' structure which describes the dimensions
   * and properties of the voxel data.  This structure follows:
   * NOTE: many programs claim to write analyze 7.5 file format, but fail
   *       to fill in much of this information.
   *
   *    struct header_key
   *        int sizeof_header   // must indicate the byte size of header file, Always 348 bytes ?????
   *        char data_type[10]  //??????A character string that has a 1 to 1 correspondence 
   *                            //with the short int datatype field of the image_dimension.
   *                            //Acceptable values are:  DataTypes[10][12]= { "UNKNOWN",
   *                            //"BINARY","CHAR","SHORT", "INT","FLOAT","COMPLEX",
   *                            //"DOUBLE","RGB", "USHORT" };
   *                            //NOTE: Some programs that claim to use the analyze format
   *                            //      do not fill this field in. i.e. AIR
   *                            //NOTE: Some documentation claims that this field should contain
   *                            //information about the type of data stored i.e. MRI-T1, MRI-T2, MRI-PD, CT, etc...
   *        char db_name[18]    //??????????Don't know what this is
   *        int extents;        //Should be 16384, the image file is created as contiguous with a 
   *                            //minium extent size.
   *                            //This is the key for determining byte endedness of the file.
   *        short int session_error; // ????????????Don't know what this is.
   *        char regular;       // This must be 'r' to indicate that all images and volumes are the same size.
   *        char hkey_un0;      // Unused field for future expansion.
   *
   *    struct image_dimension struct decribes the organization and
   *    size of images. These elements enable IO routines to reference
   *    images by volume and slice number.
   *
   *        short int dim[]  // array of image dimensions
   *            dim[0]        // number of dimensions; usually 4
   *            dim[1]        // image X dimension, i.e. number of voxels per row (adjacent memory locations)
   *            dim[2]        // image Y dimension, i.e. number of rows per slice
   *            dim[3]        // Volume Z dimension, i.e. number of slices per volume
   *            dim[4]        // Number of time points, i.e. number of volumes per series
   *              . . .
   *            dim[7]        // volumes in file, i.e. number of volumes per series
   *
   *        char vox_units[4] // specifies the spatial units of measure for a voxel, valid values are "mm","cm" and "in"
   *                          // NOTE:  if no match is found, "mm" is assumed
   *        char cal_units[4] // specifies the name of hte calibration unit, valid values are "mm","cm" and "in"
   *                          //??????????How do cal_unit differ from vox_units?
   *        short int unused; //Unused field for future expansion
   *                          //  NOTE: if no match is found, "mm" is assumed
   *        short int datatype //A short int that has a 1 to 1 correspondence 
   *                           //with the string datatype field of the image_dimension.
   *                           //Acceptable values are:
   *                           // enum DataTypeKeyValues  {
   *                           //  ANALYZE_DT_UNKNOWN        =0,
   *                           //  ANALYZE_DT_BINARY         =1,
   *                           //  ANALYZE_DT_UNSIGNED_CHAR  =2,
   *                           //  ANALYZE_DT_SIGNED_SHORT   =4,
   *                           //  ANALYZE_DT_SIGNED_INT     =8,
   *                           //  ANALYZE_DT_FLOAT          =16,
   *                           //  ANALYZE_DT_COMPLEX        =32,
   *                           //  ANALYZE_DT_DOUBLE         =64,
   *                           //  ANALYZE_DT_RGB            =128,
   *                           //  ANALYZE_DT_ALL            =255
   *                           //  ANALYZE_DT_UNSIGNED_SHORT =6,  ##NOTE: This is not officially supported by the ANALYZE 7.5 file format!!
   *                           // };
   *                          //NOTE: THIS MUST BE FILLED IN.  This is the field that most
   *                          //applications use to determine the type.
   * short int bitpix   // bits per pixel.  This field must agree with the datatype and
   *                    // data_type feilds.
   *                    //DataTypeSizes[11]={0,1,8,16,32,32,64,64,24,0,16};
   * short int dim_un0; // Unused value for future expansion.
   * float pixdim[]     // parallel array to dim giving voxel dimensions
   * NOTE: pixdim[0] is ignored, and the number of dims are taken from dims[0], 
   * and pixdim[1..7] are the actual pixdims.
   *       pixdim[1]    // voxel width
   *       pixdim[2]    // voxel height
   *       pixdim[3]    // voxel depth or slice thickness
   *       pixdim[4]    //
   *       . . .
   *       pixdim[7]    //
   *
   * float vox_offset;  //   byte offset in the .img file at which
   *                    //   voxels start. If value is negative
   *                    //   specifies that the absolute value
   *                    //   is applied for every image in the file.
   * float roi_scale    // ???????????Don't know what this does.
   * float funused1     // Unused, for future expansion
   * float funused2     // Unused, for future expansion
   * float cal_max      //Specify the minimum of the range of the calibration values.
   * float cal_min      //Specify the minimum of the range of the calibration values.
   * float compressed   //?????Have not figured out what the valid values are.
   *                    // Most applications ignore this field.
   * int verified       //??????????? Don't know what this field is for.
   * int glmax, glmin   //the max and min values for entire data set. ??What entire data set?
   *                    //??How does this relate to the cal_min,cal_max,omax,omin,smax,smin?
   *                    //??????? What if the datatype is unsigned int or float or RGB or complex?
   */
  /*
   *The data_history substructure is not required, but the 'orient' element
   *is used to indicate individual slice orientation and determines whether
   *the Analyze 'Movie' program will attempt to flip the images before
   *displaying a movie sequence.
   *???????many of the fields in the data_history section are no longer
   *???????meaningful.  It is perfectly acceptable to put any values you want
   *???????into those fields because they do not(should not) affect image
   *???????processing.
   * char descrip[80]   //A place to put a short description of the data.
   * char aux_file[24]  //A place to put the name of an auxillary file to
   *                    //use instead of the default .img file????????????
   * ????????????????????????????????????????????????
   * A "Radiologic" view of the data and places the spatial
   * origin at the subjects Inferior-Right-Posterior.  Presumably this is because
   * it is the view a radiologist would have of a patient in an MR/PET/CT scanner.
   *    orient:
   *            0 - transverse unflipped (Radiolgic)
   *            1 - coronal unflipped    (Radiolgic)
   *            2 - sagittal unflipped   (Radiolgic)
   *            3 - transverse flipped
   *            4 - coronal flipped
   *            5 - sagittal flipped
   * Where the Origin disignators are are with respect to the patient
   * [(I)nferior|(S)uperior] [(L}eft|(R)ight] [(A)nterior|(P)osterior]
   * hdr->orient  "MayoClinic/Analyze"   Origin   dims[1]  dims[2]  dims[3]
   * ======================================================================
   * 0            transverse-unflipped   IRP       R->L     P->A    I->S
   * 1            coronal-unflipped      IRP       R->L     I->S    P->A
   * 2            sagittal-unflipped     IRP       P->A     I->S    R->L
   * 3            transverse-flipped     ???       R->L     A->P    I->S
   * 4            coronal-flipped        ???       R->L     S->I    P->A
   * 5            sagittal-flipped       ???       P->A     I->S    L->R
   * char originator[10]   //The person or group that generated this image
   * char generated[10]    //The data the image was generated????????????
   * char scannum[10]      //An instituion independent identifier for the image
   * char patient_id[10]   //An instituion independant identifier for the subject of the scan
   * exp_date[10]          //??????????????????????
   * exp_time[10]          //??????????????????????
   * char hist_un0[3]      //Unused padding of the structure
   * int views             //??????????????????????
   * int vols_added;       //??????????????????????
   * int start_field       //??????????????????????
   * int field_skip        //??????????????????????
   * int omax              //??????????????????????
   * int omin              //??????????????????????
   * int smax              //??????????????????????
   * int smin              //??????????????????????
   */

enum {
  ITKA_TRANSVERSE=0,
  ITKA_CORONAL   =1,
  ITKA_SAGITTAL  =2,
  ITKA_TRANSVERSE_FLIPPED=3,
  ITKA_CORONAL_FLIPPED=4,
  ITKA_SAGITTAL_FLIPPED=5
};

/*
 *
 * (c) Copyright, 1986-1995
 * Biomedical Imaging Resource
 * Mayo Foundation
 *
 * dbh.h
 * database sub-definitions
 */

struct header_key              /*      header_key       */
{                              /* off + size*/
  int sizeof_hdr;              /* 0 + 4     */
  char data_type[10];          /* 4 + 10    */
  char db_name[18];            /* 14 + 18   */
  int extents;                 /* 32 + 4    */
  short int session_error;     /* 36 + 2    */
  char regular;                /* 38 + 1    */
  char hkey_un0;               /* 39 + 1    */
};                             /* total=40  */

struct image_dimension         /*      image_dimension  */
{                              /* off + size*/
  short int dim[8];            /* 0 + 16    */
  char vox_units[4];           /* 16 + 4    */
  char cal_units[8];           /* 20 + 4    */
  short int unused1;           /* 24 + 2    */
  short int datatype;          /* 30 + 2    */
  short int bitpix;            /* 32 + 2    */
  short int dim_un0;           /* 34 + 2    */
  float pixdim[8];             /* 36 + 32   */
  float vox_offset;            /* 68 + 4    */
  float roi_scale;             /* 72 + 4    */
  float funused1;              /* 76 + 4    */
  float funused2;              /* 80 + 4    */
  float cal_max;               /* 84 + 4    */
  float cal_min;               /* 88 + 4    */
  int compressed;              /* 92 + 4    */
  int verified;                /* 96 + 4    */
  int glmax, glmin;            /* 100 + 8   */
};                             /* total=108 */

struct data_history            /*      data_history     */
{                              /* off + size*/
  char descrip[80];            /* 0 + 80    */
  char aux_file[24];           /* 80 + 24   */
  char orient;                 /* 104 + 1   */
  char originator[10];         /* 105 + 10  */
  char generated[10];          /* 115 + 10  */
  char scannum[10];            /* 125 + 10  */
  char patient_id[10];         /* 135 + 10  */
  char exp_date[10];           /* 145 + 10  */
  char exp_time[10];           /* 155 + 10  */
  char hist_un0[3];            /* 165 + 3   */
  int views;                   /* 168 + 4   */
  int vols_added;              /* 172 + 4   */
  int start_field;             /* 176 + 4   */
  int field_skip;              /* 180 + 4   */
  int omax,omin;               /* 184 + 8   */
  int smax,smin;               /* 192 + 8   */
};                             /* total=200 */

struct dsr                     /*      dsr              */
{                              /* off + size*/
  struct header_key hk;        /* 0 + 40    */
  struct image_dimension dime; /* 40 + 108  */
  struct data_history hist;    /* 148 + 200 */
};                             /* total=348 */

#ifdef  __cplusplus
}
#endif                           /*#ifdef  __cplusplus*/
#endif                           /* __dbh_h__ */
