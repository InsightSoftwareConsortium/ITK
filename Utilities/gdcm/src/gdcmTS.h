/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmTS.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
                                                                                
  Copyright (c) CREATIS (Centre de Recherche et d'Applications en Traitement de
  l'Image). All rights reserved. See Doc/License.txt or
  http://www.creatis.insa-lyon.fr/Public/Gdcm/License.html for details.
                                                                                
     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.
                                                                                
=========================================================================*/

#ifndef GDCMTS_H
#define GDCMTS_H

#include "gdcmCommon.h"

#include <map>
#include <string>
#include <iostream>

namespace gdcm 
{
//-----------------------------------------------------------------------------
typedef std::string TSKey;
typedef std::string TSAtr;
typedef std::map<TSKey, TSAtr> TSHT;    // Transfer Syntax Hash Table


//-----------------------------------------------------------------------------
/**
 * \brief Container for dicom 'Transfer Syntax' Hash Table
 * \note   This is a singleton
 */
class GDCM_EXPORT TS
{
public:
enum SpecialType {
  ImplicitVRLittleEndian = 0,
  ImplicitVRLittleEndianDLXGE,
  ExplicitVRLittleEndian,
  DeflatedExplicitVRLittleEndian,
  ExplicitVRBigEndian,
  JPEGBaselineProcess1,
  JPEGExtendedProcess2_4,
  JPEGExtendedProcess3_5,
  JPEGSpectralSelectionProcess6_8,
  JPEGFullProgressionProcess10_12,
  JPEGLosslessProcess14,
  JPEGLosslessProcess14_1,
  JPEGLSLossless,
  JPEGLSNearLossless,
  JPEG2000Lossless,  
  JPEG2000,
  RLELossless,
  UnknownTS
};

public:
   TS();
   ~TS();

   void Print(std::ostream &os = std::cout);

   int Count(TSKey const &key);
   TSAtr const &GetValue(TSKey const &key);
   bool IsTransferSyntax(TSKey const &key);
   bool IsRLELossless(TSKey const &key);
   bool IsJPEGLossless(TSKey const&key);
   bool IsJPEGLossy(TSKey const&key);
   bool IsJPEG2000(TSKey const &key);
   bool IsJPEG(TSKey const &key);
   bool IsJPEGLS(TSKey const &key);

   // This should be deprecated very soon
   SpecialType GetSpecialTransferSyntax(TSKey const &key);
   const char* GetSpecialTransferSyntax(SpecialType t);

private:
   TSHT TsMap;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
