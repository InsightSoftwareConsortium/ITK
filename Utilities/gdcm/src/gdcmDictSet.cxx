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

namespace gdcm 
{

//-----------------------------------------------------------------------------
// Constructor / Destructor
/** 
 * \ingroup DictSet
 * \brief   The Dictionnary Set obtained with this constructor simply
 *          contains the Default Public dictionnary.
 */
DictSet::DictSet() 
{
   DictPath = BuildDictPath();
   std::string pubDictFile(DictPath);
   pubDictFile += PUB_DICT_FILENAME;
   Dicts[PUB_DICT_NAME] = new Dict(pubDictFile);
}

/**
 * \ingroup DictSet
 * \brief  Destructor 
 */
DictSet::~DictSet() 
{
   // Remove dictionnaries
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

   // Remove virtual dictionnary entries
   VirtualEntry.clear();
}

//-----------------------------------------------------------------------------
// Print
/**
 * \ingroup DictSet
 * \brief   Print, in an informal fashion, the list of all the dictionaries
 *          contained is this DictSet, along with their respective content.
 * @param   os Output stream used for printing.
 */
void DictSet::Print(std::ostream& os) 
{
   for (DictSetHT::iterator dict = Dicts.begin(); dict != Dicts.end(); ++dict)
   {
      os << "Printing dictionary " << dict->first << std::endl;
      dict->second->Print(os);
   }
}

//-----------------------------------------------------------------------------
// Public
/** 
 * \ingroup DictSet
 * \brief   Consider all the entries of the public dicom dictionnary. 
 *          Build all list of all the tag names of all those entries.
 * \sa DictSet::GetPubDictTagNamesByCategory
 * @return  A list of all entries of the public dicom dictionnary.
 */
EntryNamesList * DictSet::GetPubDictEntryNames() 
{
   return GetDefaultPubDict()->GetDictEntryNames();
}

/** 
 * \ingroup DictSet
 * \brief   
 *          - Consider all the entries of the public dicom dictionnary.
 *          - Build an hashtable whose keys are the names of the groups
 *           (fourth field in each line of dictionary) and whose corresponding
 *           values are lists of all the dictionnary entries among that
 *           group. Note that apparently the Dicom standard doesn't explicitely
 *           define a name (as a string) for each group.
 *          - A typical usage of this method would be to enable a dynamic
 *           configuration of a Dicom file browser: the admin/user can
 *           select in the interface which Dicom tags should be displayed.
 * \warning 
 *          - Dicom *doesn't* define any name for any 'categorie'
 *          (the dictionnary fourth field was formerly NIH defined
 *           -and no longer he is-
 *           and will be removed when Dicom provides us a text file
 *           with the 'official' Dictionnary, that would be more friendly
 *           than asking us to perform a line by line check of the dictionnary
 *           at the beginning of each year to -try to- guess the changes)
 *          - Therefore : please NEVER use that fourth field :-(
 * *
 * @return  An hashtable: whose keys are the names of the groups and whose
 *          corresponding values are lists of all the dictionnary entries
 *          among that group.
 */
EntryNamesByCatMap * DictSet::GetPubDictEntryNamesByCategory() 
{
   return GetDefaultPubDict()->GetDictEntryNamesByCategory();
}

/**
 * \ingroup DictSet
 * \brief   Loads a dictionary from a specified file, and add it
 *          to already the existing ones contained in this DictSet.
 * @param   filename Absolute or relative filename containing the
 *          dictionary to load.
 * @param   name Symbolic name that be used as identifier of the newly 
 *          created dictionary.
 */
Dict *DictSet::LoadDictFromFile(std::string const & filename, 
                                DictKey const & name) 
{
   Dict *newDict = new Dict(filename);
   AppendDict(newDict, name);

   return newDict;
}

/**
 * \ingroup DictSet
 * \brief   Retrieve the specified dictionary (when existing) from this
 *          DictSet.
 * @param   dictName The symbolic name of the searched dictionary.
 * \result  The retrieved dictionary.
 */
Dict *DictSet::GetDict(DictKey const & dictName) 
{
   DictSetHT::iterator dict = Dicts.find(dictName);
   if(dict != Dicts.end())
   {
      return dict->second;
   }
   return NULL;
}

/**
 * \brief   Create a DictEntry which will be reference 
 *          in no dictionnary
 * @return  virtual entry
 */
DictEntry *DictSet::NewVirtualDictEntry( uint16_t group,
                                         uint16_t element,
                                         TagName vr,
                                         TagName fourth,
                                         TagName name)
{
   DictEntry *entry;
   const std::string tag = DictEntry::TranslateToKey(group,element)
                           + "#" + vr + "#" + fourth + "#" + name;
   TagKeyHT::iterator it;
   
   it = VirtualEntry.find(tag);
   if(it != VirtualEntry.end())
   {
      entry = &(it->second);
   }
   else
   {
      DictEntry ent(group, element, vr, fourth, name);
      VirtualEntry.insert(
         std::map<TagKey, DictEntry>::value_type
            (tag, ent));
      entry = &(VirtualEntry.find(tag)->second);
   }

   return entry;
}

/**
 * \brief   Obtain from the GDCM_DICT_PATH environnement variable the
 *          path to directory containing the dictionnaries. When
 *          the environnement variable is absent the path is defaulted
 *          to "../Dicts/".
 * @return  path to directory containing the dictionnaries
 */
std::string DictSet::BuildDictPath() 
{
   std::string resultPath;
   const char *envPath = 0;
   envPath = getenv("GDCM_DICT_PATH");

   if (envPath && (strlen(envPath) != 0)) 
   {
      resultPath = envPath;
      if ( resultPath[resultPath.length()-1] != '/' )
      {
         resultPath += '/';
      }
      dbg.Verbose(1, "DictSet::BuildDictPath:",
                     "Dictionary path set from environnement");
   } 
   else
   {
      resultPath = PUB_DICT_PATH;
   }

   return resultPath;
}

//-----------------------------------------------------------------------------
// Protected
bool DictSet::AppendDict(Dict *newDict, DictKey const & name)
{
   Dicts[name] = newDict;

   return true;
}

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------

} // end namespace gdcm
