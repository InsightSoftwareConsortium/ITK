/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmGlobal.cxx
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

#include "gdcmGlobal.h"

#include "gdcmDebug.h"
#include "gdcmVR.h"
#include "gdcmTS.h"
#include "gdcmDictGroupName.h"
#include "gdcmDictSet.h"
#include "gdcmDicomDirElement.h"

namespace gdcm 
{
//-----------------------------------------------------------------------------
/// \brief Those global string that are returned by reference everywhere in 
/// gdcm code used to be in gdcmCommon.h but due to a 'bug' in gcc/MacOSX
/// you cannot have static initialization in a multithreaded environment
/// since there is a lazy construction everything got skrew up somehow
/// Therefore the actual initialization is done in a cxx file (avoid
/// duplicated symbol), and an extern is used in gdcmCommon.h

const std::string GDCM_UNKNOWN   = "gdcm::Unknown";
const std::string GDCM_UNFOUND   = "gdcm::Unfound";
const std::string GDCM_BINLOADED = "gdcm::Binary data loaded";
const std::string GDCM_NOTLOADED = "gdcm::NotLoaded";
const std::string GDCM_UNREAD    = "gdcm::UnRead";
const std::string GDCM_NOTASCII  = "gdcm::NotAscii";
const std::string GDCM_PIXELDATA = "gdcm::Pixel Data to be loaded";
//-----------------------------------------------------------------------------
DictSet         *Global::Dicts     = (DictSet *)0;
VR              *Global::ValRes    = (VR *)0;
TS              *Global::TranSyn   = (TS *)0;
DictGroupName   *Global::GroupName = (DictGroupName *)0;
DicomDirElement *Global::ddElem    = (DicomDirElement *)0;

//-----------------------------------------------------------------------------
/**
 * \brief   Global container
 */
Global Glob;

//-------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief   constructor : populates the various H Tables
 */
Global::Global()
{
   if (ValRes || TranSyn || Dicts || ddElem)
   {
      gdcmWarningMacro( "VR or TS or Dicts already allocated");
      return;
   }
   Dicts     = new DictSet();
   ValRes    = new VR();
   TranSyn   = new TS();
   GroupName = new DictGroupName();
   ddElem    = new DicomDirElement();
}

/**
 * \brief   canonical destructor 
 */
Global::~Global()
{
   delete Dicts;
   delete ValRes;
   delete TranSyn;
   delete GroupName;
   delete ddElem;
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   returns a pointer to Dictionaries Table 
 */
DictSet *Global::GetDicts()
{
   return Dicts;
}

/**
 * \brief   returns a pointer to the 'Value Representation Table' 
 */
VR *Global::GetVR()
{
   return ValRes;
}

/**
 * \brief   returns a pointer to the 'Transfer Syntax Table' 
 */
TS *Global::GetTS()
{
   return TranSyn;
}

/**
 * \brief   returns a pointer to the Group name correspondance table
 */
DictGroupName *Global::GetDictGroupName()
{
   return GroupName;
}

/**
 * \brief   returns a pointer to the DicomDir related elements Table 
 */
DicomDirElement *Global::GetDicomDirElements()
{
   return ddElem;
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print

//-----------------------------------------------------------------------------
} // end namespace gdcm
