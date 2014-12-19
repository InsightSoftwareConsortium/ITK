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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkSpatialOrientation_h
#define itkSpatialOrientation_h

namespace itk
{
namespace SpatialOrientation
{
//  Coordinate orientation codes have a place-value organization such that
//  an ImageDimension-al sequence of subcodes says both which varies fastest
//  through which varies slowest, but also which end of the frame of
//  reference
//  is considered zero for each of the coordinates.  For example, 'RIP'
//  means
//  Right to Left varies fastest, then Inferior to Superior, and Posterior
//  to
//  Anterior varies the slowest.
typedef enum {
  ITK_COORDINATE_UNKNOWN = 0,
  ITK_COORDINATE_Right = 2,
  ITK_COORDINATE_Left = 3,
  ITK_COORDINATE_Posterior = 4, //back
  ITK_COORDINATE_Anterior = 5,  //front
  ITK_COORDINATE_Inferior = 8,  //below
  ITK_COORDINATE_Superior = 9   //above
                                // ITK_COORDINATE_Historical=16,
                                // ITK_COORDINATE_Future=17
  } CoordinateTerms;

typedef enum {
  // These code place values have to be far enough apart to
  // separate the CoordinateTerms above.
  // However, if we added History/Future direction in time,
  // we would need at least 5 bits per.
  ITK_COORDINATE_PrimaryMinor = 0,
  ITK_COORDINATE_SecondaryMinor = 8,
  ITK_COORDINATE_TertiaryMinor = 16
                                 // Majorness is in terms of rank-minor because
                                 // a
                                 // fourth dimension, time, would be even More
                                 // major than the
                                 // PrimaryMajor==TertiaryMinor.
                                 // ITK_COORDINATE_QuaternaryMinor=24
  } CoordinateMajornessTerms;
// Adding time IN GENERAL would make these 8 x 6 = 48 triples into 16
// x 24 = 384 4-tuples.
// A general fourth dimension would need a unique pair of letters to
// add to the code;
// Maybe use H and F, from History to Future? Maybe use 48 x 2 with
// time ALWAYS highest
// order, or 48 x 2 x 2, with 3-space always highest or lowest order?
// Multispectra might be
// co-registered and pieced together.... PD-T2 interleaving of
// slices is handled with choosing
// which spectrum to load via the prototypical file name.
typedef enum {
  ITK_COORDINATE_ORIENTATION_INVALID = ITK_COORDINATE_UNKNOWN,
  ITK_COORDINATE_ORIENTATION_RIP = ( ITK_COORDINATE_Right
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_LIP = ( ITK_COORDINATE_Left
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_RSP = ( ITK_COORDINATE_Right
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_LSP = ( ITK_COORDINATE_Left
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_RIA = ( ITK_COORDINATE_Right
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_LIA = ( ITK_COORDINATE_Left
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_RSA = ( ITK_COORDINATE_Right
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_LSA = ( ITK_COORDINATE_Left
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_TertiaryMinor ),

  ITK_COORDINATE_ORIENTATION_IRP = ( ITK_COORDINATE_Inferior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_ILP = ( ITK_COORDINATE_Inferior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_SRP = ( ITK_COORDINATE_Superior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_SLP = ( ITK_COORDINATE_Superior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_IRA = ( ITK_COORDINATE_Inferior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_ILA = ( ITK_COORDINATE_Inferior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_SRA = ( ITK_COORDINATE_Superior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_SLA = ( ITK_COORDINATE_Superior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_TertiaryMinor ),

  ITK_COORDINATE_ORIENTATION_RPI = ( ITK_COORDINATE_Right
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_LPI = ( ITK_COORDINATE_Left
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_RAI = ( ITK_COORDINATE_Right
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_LAI = ( ITK_COORDINATE_Left
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_RPS = ( ITK_COORDINATE_Right
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_LPS = ( ITK_COORDINATE_Left
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_RAS = ( ITK_COORDINATE_Right
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_LAS = ( ITK_COORDINATE_Left
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_TertiaryMinor ),

  ITK_COORDINATE_ORIENTATION_PRI = ( ITK_COORDINATE_Posterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_PLI = ( ITK_COORDINATE_Posterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_ARI = ( ITK_COORDINATE_Anterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_ALI = ( ITK_COORDINATE_Anterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_PRS = ( ITK_COORDINATE_Posterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_PLS = ( ITK_COORDINATE_Posterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_ARS = ( ITK_COORDINATE_Anterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_ALS = ( ITK_COORDINATE_Anterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_TertiaryMinor ),

  ITK_COORDINATE_ORIENTATION_IPR = ( ITK_COORDINATE_Inferior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_SPR = ( ITK_COORDINATE_Superior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_IAR = ( ITK_COORDINATE_Inferior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_SAR = ( ITK_COORDINATE_Superior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_IPL = ( ITK_COORDINATE_Inferior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_SPL = ( ITK_COORDINATE_Superior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_IAL = ( ITK_COORDINATE_Inferior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_SAL = ( ITK_COORDINATE_Superior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Anterior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_TertiaryMinor ),

  ITK_COORDINATE_ORIENTATION_PIR = ( ITK_COORDINATE_Posterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_PSR = ( ITK_COORDINATE_Posterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_AIR = ( ITK_COORDINATE_Anterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_ASR = ( ITK_COORDINATE_Anterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Right << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_PIL = ( ITK_COORDINATE_Posterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_PSL = ( ITK_COORDINATE_Posterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_AIL = ( ITK_COORDINATE_Anterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Inferior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_TertiaryMinor ),
  ITK_COORDINATE_ORIENTATION_ASL = ( ITK_COORDINATE_Anterior
                                     << ITK_COORDINATE_PrimaryMinor )
                                   + ( ITK_COORDINATE_Superior << ITK_COORDINATE_SecondaryMinor )
                                   + ( ITK_COORDINATE_Left << ITK_COORDINATE_TertiaryMinor )
  } ValidCoordinateOrientationFlags;
// ^^^
// |||
// ||\Sequential indexes are separated by (planes=rows*columns) memory
// locations
// |\Sequential indexes are separated by rows memory locations (sweep out
// a plane)
// \Sequential indexes are adjacent memory locations (sweep out a row)

#ifndef __TEMPORARILY_INCLUDED_IN_COMPILATIONS__
typedef enum {
  ITK_ORIGIN_IRP  = 0,        /**< Denotes a zeroCorner (image origin) */
  /* is Inferior Right Posterior */
  ITK_ORIGIN_IRA  = 1,        /**< Denotes a zeroCorner (image origin) */
  /* is Inferior Right Anterior */
  ITK_ORIGIN_ILP  = 2,        /**< Denotes a zeroCorner (image origin) */
  /* is Inferior Left Posterior */
  ITK_ORIGIN_ILA  = 3,        /**< Denotes a zeroCorner (image origin) */
  /* is Inferior Left Anterior */
  ITK_ORIGIN_SRP  = 4,        /**< Denotes a zeroCorner (image origin) */
  /* is Superior Right Posterior */
  ITK_ORIGIN_SRA  = 5,        /**< Denotes a zeroCorner (image origin) */
  /* is Superior Right Anterior */
  ITK_ORIGIN_SLP  = 6,        /**< Denotes a zeroCorner (image origin) */
  /* is Superior Left Posterior */
  ITK_ORIGIN_SLA  = 7 /**< Denotes a zeroCorner (image origin) */
                      /* is Superior Left Anterior */
  } ValidOriginFlags;
#endif
} // end of namespace SpatialOrientation
} // end namespace itk

#endif
