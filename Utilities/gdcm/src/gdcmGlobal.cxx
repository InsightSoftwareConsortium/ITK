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

namespace gdcm 
{

/**
 * \ingroup Globals
 * \brief Pointer to a container, holding _all_ the Dicom Dictionaries.
 */
DictSet         *Global::Dicts  = (DictSet *)0;

/**
 * \ingroup Globals
 * \brief   Pointer to a hash table containing the 'Value Representations'.
 */
VR              *Global::ValRes     = (VR *)0;

/**
 * \ingroup Globals
 * \brief   Pointer to a hash table containing the Transfer Syntax codes
 *          and their english description 
 */
TS              *Global::TranSyn     = (TS *)0;

/**
 * \ingroup Globals
 * \brief   Pointer to the hash table containing the Dicom Elements
 *          necessary to describe each part of a DICOMDIR 
 */
DicomDirElement *Global::ddElem = (DicomDirElement *)0;

/**
 * \ingroup Globals
 * \brief   Global container
 */
Global Glob;

/**
 * \ingroup Global
 * \brief   constructor : populates the various H Tables
 */
Global::Global()
{
   if (ValRes || TranSyn || Dicts || ddElem)
   {
      dbg.Verbose(0, "Global::Global : VR or TS or Dicts already allocated");
      return;
   }
   Dicts   = new DictSet();
   ValRes  = new VR();
   TranSyn = new TS();
   ddElem  = new DicomDirElement();
}

/**
 * \ingroup Global
 * \brief   canonical destructor 
 */
Global::~Global()
{
   delete Dicts;
   delete ValRes;
   delete TranSyn;
   delete ddElem;
}
/**
 * \ingroup Global
 * \brief   returns a pointer to the 'Value Representation Table' 
 */
VR *Global::GetVR()
{
   return ValRes;
}
/**
 * \ingroup Global
 * \brief   returns a pointer to the 'Transfert Syntax Table' 
 */
TS *Global::GetTS()
{
   return TranSyn;
}
/**
 * \ingroup Global
 * \brief   returns a pointer to Dictionaries Table 
 */
DictSet *Global::GetDicts()
{
   return Dicts;
}
/**
 * \ingroup Global
 * \brief   returns a pointer to the DicomDir related elements Table 
 */
DicomDirElement *Global::GetDicomDirElements()
{
   return ddElem;
}
} // end namespace gdcm
