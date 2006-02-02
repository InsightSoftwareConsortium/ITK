/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDictSet.cxx
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

#include "gdcmDictSet.h"
#include "gdcmDebug.h"
#include <fstream>
#include <stdlib.h>  // For getenv
#include <stdio.h>   // For sprintf

namespace gdcm 
{

//-----------------------------------------------------------------------------
// Constructor / Destructor
/** 
 * \brief   The Dictionary Set obtained with this constructor simply
 *          contains the Default Public dictionary.
 */
DictSet::DictSet() 
{
   DictPath = BuildDictPath();
   std::string pubDictFile(DictPath);
   pubDictFile += PUB_DICT_FILENAME;
   Dicts[PUB_DICT_NAME] = new Dict(pubDictFile);
}

/**
 * \brief  Destructor 
 */
DictSet::~DictSet() 
{
   // Remove dictionaries
   for (DictSetHT::iterator tag = Dicts.begin(); tag != Dicts.end(); ++tag) 
   {
      Dict *entryToDelete = tag->second;
      if ( entryToDelete )
      {
         delete entryToDelete;
      }
      tag->second = NULL;
   }
   Dicts.clear();

   // Remove virtual dictionary entries
   VirtualEntries.clear();
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   Loads a dictionary from a specified file, and add it
 *          to already the existing ones contained in this DictSet.
 * @param   filename Absolute or relative filename containing the
 *          dictionary to load.
 * @param   name Symbolic name that be used as identifier of the newly 
 *          created dictionary.
 */
Dict *DictSet::LoadDictFromFile(std::string const &filename, 
                                DictKey const &name) 
{
   Dict *newDict = new Dict(filename);
   AppendDict(newDict, name);

   return newDict;
}

/**
 * \brief   Retrieve the specified dictionary (when existing) from this
 *          DictSet.
 * @param   dictName The symbolic name of the searched dictionary.
 * \result  The retrieved dictionary.
 */
Dict *DictSet::GetDict(DictKey const &dictName) 
{
   DictSetHT::iterator dict = Dicts.find(dictName);
   if ( dict != Dicts.end() )
   {
      return dict->second;
   }
   return NULL;
}

/**
 * \brief   Create a DictEntry which will be referenced in no dictionary
 * @param   group   Group number of the Entry 
 * @param   elem  Element number of the Entry
 * @param   vr  Value Representation of the Entry
 * @param   vm  Value Multiplicity of the Entry
 * @param   name  English name of the Entry
 * @return  virtual entry
 */
DictEntry *DictSet::NewVirtualDictEntry( uint16_t group,
                                         uint16_t elem,
                                         TagName vr,
                                         TagName vm,
                                         TagName name)
{
   DictEntry *entry;

  // Let's follow 'Purify' advice
  //
  // const std::string tag = DictEntry::TranslateToKey(group,elem)
  //                         + "#" + vr + "#" + vm + "#" + name;
#if FASTTAGKEY && 0
   // FIXME
   TagKey tag;
   tag.tab[0] = group;
   tag.tab[1] = elem;
#else
   char res[10];
   sprintf(res,"%04x|%04x", group, elem);
   ExtendedTagKey tag = res;
   tag += "#" + vr + "#" + vm + "#" + name;  
#endif
  
   ExtendedTagKeyHT::iterator it;
   
   it = VirtualEntries.find(tag);
   if ( it != VirtualEntries.end() )
   {
      entry = &(it->second);
   }
   else
   {
      DictEntry ent(group, elem, vr, vm, name);
      VirtualEntries.insert(
         ExtendedTagKeyHT::value_type(tag, ent) );
      entry = &(VirtualEntries.find(tag)->second);
   }

   return entry;
}

/**
 * \brief   Get the first entry while visiting the DictSet
 * \return  The first Dict if found, otherwhise NULL
 */
Dict *DictSet::GetFirstEntry()
{
   ItDictHt = Dicts.begin();
   if ( ItDictHt != Dicts.end() )
      return ItDictHt->second;
   return NULL;
}

/**
 * \brief   Get the next entry while visiting the Hash table (DictSetHT)
 * \note : meaningfull only if GetFirstEntry already called
 * \return  The next Dict if found, otherwhise NULL
 */
Dict *DictSet::GetNextEntry()
{
   gdcmAssertMacro (ItDictHt != Dicts.end());
  
   ++ItDictHt;
   if ( ItDictHt != Dicts.end() )
      return ItDictHt->second;
   return NULL;
}

/**
 * \brief   Obtain from the GDCM_DICT_PATH environnement variable the
 *          path to directory containing the dictionaries. When
 *          the environnement variable is absent the path is defaulted
 *          to "../Dicts/".
 * @return  path to directory containing the dictionaries
 */
std::string DictSet::BuildDictPath() 
{
   std::string resultPath;
   const char *envPath;
   envPath = getenv("GDCM_DICT_PATH");

   if (envPath && (strlen(envPath) != 0)) 
   {
      resultPath = envPath;
      gdcmWarningMacro( "Dictionary path set from environnement");
   } 
   else
   {
      resultPath = PUB_DICT_PATH;
   }
   if ( resultPath[resultPath.length()-1] != '/' )
   {
      resultPath += '/';
   }

   return resultPath;
}

//-----------------------------------------------------------------------------
// Protected
/**
 * \brief   Adds a Dictionary to a DictSet
 * \return  always true
 */
bool DictSet::AppendDict(Dict *newDict, DictKey const &name)
{
   Dicts[name] = newDict;

   return true;
}

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Print, in an informal fashion, the list of all the dictionaries
 *          contained is this DictSet, along with their respective content.
 * @param   os Output stream used for printing.
 * @param indent Indentation string to be prepended during printing
 */
void DictSet::Print(std::ostream &os, std::string const & )
{
   for (DictSetHT::iterator dict = Dicts.begin(); dict != Dicts.end(); ++dict)
   {
      os << "Printing dictionary " << dict->first << std::endl;
      dict->second->Print(os);
   }
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
