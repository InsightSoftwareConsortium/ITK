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
   * Programmer:    Hans J. Johnson
   * e-mail:        hans-johnson@uiowa.edu
   * Organization:  The University of Iowa
   * Copyright:     Hans J. Johnson, 1999
   *    Created:    6/1/99
   * Function:      This file contains the structure definition for Analyze files
   **************************************************************************/
  /* Analyze From Mayo  --NOTES From Medical image formats web page:
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
   * am sure he will be able to help. I haven't tested it because I can't afford to buy 
   * a copy myself :( That's a hint, Denny.
   *
   * Analyze IMAGE FILE FORMAT
   *
   * Analyze image file sets consist of at least 2 files:
   *    - an image file
   *    - a header file
   *    - a color lookup file   * optional
   *
   * For the Analyze image file set "foo" there are two files:
   *    foo.img & foo.hdr  (optionally foo.lkup)
   *
   * The Analyze programs refer to this file set as a single entity.
   *
   *      The Image File (foo.img)
   *
   * The format of the image file is very simple; containing usually
   * uncompressed voxel data for the images in one of the several
   * possible voxel formats:
   *    - 1 bit  packed binary (slices begin on byte boundaries)
   *    - 8 bit  (unsigned char) gray scale unless .lkup file present
   *    - 16 bit signed short
   *    - 32 bit signed integers or float
   *    - 24 bit RGB, 8 bits per channel
   *
   * The header file is a 'C' structure which describes the dimensions
   * and properties of the voxel data.  This structure follows:
   *
   */
  /*
   *Comments:
   *    struct header_key
   *        int sizeof_header   // must indicate size of header file
   *        int extents;        // should be 16384    // This is the key for determining byte endedness
   *          char regular;       // 'r'
   *
   *
   *    struct image_dimension struct decribes the organization and
   *    side of images. These elements enable IO routines to reference
   *    images by volume and slice number.
   *
   *        short int dim[]  // array of image dimensions
   *            dim[0]        // number of dimensions; usually 4
   *            dim[1]        // image width
   *            dim[2]        // image height
   *            dim[3]        // volume depth
   *            dim[4]        // volumes in file
   *
   *        char vox_units[4] // labels voxerl spatial unit
   *        char cal_units[4] // labels voxel calibration unit
   *        short int datatype // Acceptable values are
   */
  /* 
   * short int bitpix   // bits per pixel
   * float pixdim[]     // parallel array to dim giving voxel dimensions
   *                  // in each dimension
   *       pixdim[1]    // voxel width
   *       pixdim[2]    // voxel height
   *       pixdim[3]    // voxel depth or slice thickness
   *
   * float vox_offset;  //   byte offset in the .img file at which
   *                  //   voxels start. If value is negative
   *                    //   specifies that the absolute value
   *                    //   is applied for every image in the file.
   *
   * float calibrated Max & Min // specify range of calibration values
   * int glmax, glmin           // the max and min values for entire data set
   */
  /*    
   *The data_history substructure is not required, but the 'orient' element
   *is used to indicate individual slice orientation and determines whether
   *the Analyze 'Movie' program will attempt to flip the images before
   *displaying a movie sequence.
   *    orient:
   *            0 - transverse unflipped
   *            1 - coronal unflipped
   *            2 - sagittal unflipped
   *            3 - transverse flipped
   *            4 - coronal flipped
   *            5 - sagittal flipped
   * hdr->orient      "MayoClinic/Analyze"        Origin
   * =======================================================
   * 0                transverse-unflipped        ILA
   * 1                coronal-unflipped           ILA
   * 2                sagittal-unflipped          ILA
   * 3                transverse-flipped          SLA
   * 4                coronal-flipped             ILP
   * 5                sagittal-flipped            ILA
   * where the Origin disignators are
   * [(I)nferior|(S)uperior] [(L}eft|(R)ight] [(A)nterior|(P)osterior]
   *
   */
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
  /* 
    pixdim[] specifies the voxel dimensions:
    pixdim[1] - voxel width
    pixdim[2] - voxel height
    pixdim[3] - interslice distance
    ..etc
   */
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
