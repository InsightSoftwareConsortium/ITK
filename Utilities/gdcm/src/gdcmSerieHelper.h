/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmSerieHelper.h
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

#ifndef GDCMSERIEHELPER_H
#define GDCMSERIEHELPER_H

#include "gdcmCommon.h" 
#include "gdcmDebug.h"  // for LEGACY
 
#include <vector>
#include <iostream>
#include <map>

namespace gdcm 
{
class File;
typedef std::vector<File* > FileList;
#ifndef GDCM_LEGACY_REMOVE
typedef std::vector<File* > GdcmFileList;
#endif
   /// XCoherent stands for 'Extra Coherent', 
   /// (The name 'Coherent' would be enough but it was used before;
   /// I don't want to put a bomb in the code)
   /// Any 'better name' is welcome !
typedef std::map<std::string, FileList *> XCoherentFileSetmap;
   
typedef bool (*BOOL_FUNCTION_PFILE_PFILE_POINTER)(File *, File *);

//-----------------------------------------------------------------------------
/**
 * \brief  
 * - This class should be used for a stack of 2D dicom images.
 *   It allows to explore (recursively or not) a directory and 
 *   makes a set of 'Coherent Files' lists (coherent : same SerieUID)
 *   It allows :
 *   - to sort any of the Coherent File list on the image position.
 *   - to split any of the Single SerieUID Filesets (better use this name than
 *   'Coherent File List' : it's NOT a std::list, files are NOT coherent ...)
 *    into several XCoherent Filesets 
 *   XCoherent stands for 'Extra Coherent' (same orientation, or same position)
 */
class GDCM_EXPORT SerieHelper 
{
public:
   // SingleSerieUIDFileSetmap replaces the former CoherentFileListmap
   // ( List were actually std::vectors, and wher no coherent at all :
   //   They were only Single SeriesInstanceUID File sets)
   typedef std::map<std::string, FileList *> SingleSerieUIDFileSetmap;

   typedef std::vector<File* > FileVector;
   
   SerieHelper();
   ~SerieHelper();
   void Print(std::ostream &os = std::cout, std::string const &indent = "" );

   /// \todo should return bool or throw error ?
   void AddFileName(std::string const &filename);
   bool AddFile(File *header);
#ifndef GDCM_LEGACY_REMOVE
   bool AddGdcmFile(File* header) { return AddFile(header); }
#endif

   void SetDirectory(std::string const &dir, bool recursive=false);
   bool IsCoherent(FileList *fileSet);
   void OrderFileList(FileList *fileSet);
   void Clear() { ClearAll(); }

   /// \brief Gets the FIRST Single SerieUID Fileset.
   ///        Deprecated; kept not to break the API
   /// \note Caller must call OrderFileList first
   /// @return the (first) Single SerieUID Fileset
   const FileList &GetFileList()
                           { return *SingleSerieUIDFileSetHT.begin()->second; }
  
   GDCM_LEGACY(   FileList *GetFirstCoherentFileList()  );
   GDCM_LEGACY(   FileList *GetNextCoherentFileList()   );
   GDCM_LEGACY(   FileList *GetCoherentFileList(std::string serieUID)  );

   FileList *GetFirstSingleSerieUIDFileSet();
   FileList *GetNextSingleSerieUIDFileSet();
   FileList *GetSingleSerieUIDFileSet(std::string serieUID);

   /// All the following allow user to restrict DICOM file to be part
   /// of a particular serie
   GDCM_LEGACY( void AddRestriction(TagKey const &key, std::string const &value) );
   /// Allow user to specify that the serie should also be consistent (== operation),
   /// on the particular tag (group,element)
   void AddRestriction(uint16_t group, uint16_t elem); 
   /// Same as above accept use the format: "0x1234|0x5678"
   void AddRestriction(const std::string & tag)
     {
     unsigned int group, element;
     sscanf( tag.c_str(), "%04x|%04x", &group, &element);
     AddRestriction( group, element );
     }
   /// Allow user to refine the selection of a serie by specifying operation (op) on a 
   /// particular tag (group, elem) with a particular value (value).
   void AddRestriction(uint16_t group, uint16_t elem, 
                       std::string const &value, int op);

   /// \brief Use additional series information such as ProtocolName
   ///        and SeriesName to identify when a single SeriesUID contains
   ///        multiple 3D volumes - as can occur with perfusion and DTI imaging
   void SetUseSeriesDetails( bool useSeriesDetails )
     {
     m_UseSeriesDetails = useSeriesDetails;
     }
   bool GetUseSeriesDetails( void )
     {
     return m_UseSeriesDetails;
     }
   /// \brief This function will add the following DICOM tag as being part of a
   /// 'fake' uid. This is usefull when the Serie UID is not enough to disseminate
   /// into multiple sub serie when needed:
   /// 0020 0011 Series Number
   /// 0018 0024 Sequence Name
   /// 0018 0050 Slice Thickness
   /// 0028 0010 Rows
   /// 0028 0011 Columns
   void CreateDefaultUniqueSeriesIdentifier();
 
/**
 * \brief Sets the LoadMode as a boolean string. 
 *        LD_NOSEQ, LD_NOSHADOW, LD_NOSHADOWSEQ
 *        ... (nothing more, right now)
 *        WARNING : before using LD_NOSHADOW, be sure *all* your files
 *        contain accurate values in the 0x0000 element (if any) 
 *        of *each* Shadow Group. The parser will fail if the size is wrong !
 * @param   mode Load mode to be used    
 */
   void SetLoadMode (int mode) { LoadMode = mode; }

/// Brief User wants the files to be sorted Direct Order (default value)
   void SetSortOrderToDirect()  { DirectOrder = true;  }

/// Brief User wants the files to be sorted Reverse Order 
   void SetSortOrderToReverse() { DirectOrder = false; }

   /// to allow user to give is own comparison function
   void SetUserLessThanFunction( BOOL_FUNCTION_PFILE_PFILE_POINTER userFunc ) 
                        { UserLessThanFunction = userFunc; }  

   XCoherentFileSetmap SplitOnOrientation(FileList *fileSet); 
   XCoherentFileSetmap SplitOnPosition(FileList *fileSet); 
   XCoherentFileSetmap SplitOnTagValue(FileList *fileSet,
                                                 uint16_t group, uint16_t element);

   /// \brief Create a string that uniquely identifies a series.   By default
   //         uses the SeriesUID.   If UseSeriesDetails(true) has been called,
   //         then additional identifying information is used.
   std::string CreateUniqueSeriesIdentifier( File * inFile );

private:
   void ClearAll();
   bool UserOrdering(FileList *fileSet);
   bool ImagePositionPatientOrdering(FileList *fileSet);
   bool ImageNumberOrdering(FileList *fileSet);
   bool FileNameOrdering(FileList *fileSet);
   
   static bool ImageNumberLessThan(File *file1, File *file2);
   static bool ImageNumberGreaterThan(File *file1, File *file2);
   static bool FileNameLessThan(File *file1, File *file2);
   static bool FileNameGreaterThan(File *file1, File *file2);

//Attributes:
   
   SingleSerieUIDFileSetmap SingleSerieUIDFileSetHT;
   SingleSerieUIDFileSetmap::iterator ItFileSetHt;

#ifndef GDCM_LEGACY_REMOVE
   typedef std::pair<TagKey, std::string> Rule;
   typedef std::vector<Rule> SerieRestrictions;
   SerieRestrictions Restrictions;
#endif

   // New style for (extented) Rules (Moreover old one doesn't compile)
   typedef struct {
      uint16_t group;
      uint16_t elem;
      std::string value;
      int op;
   } ExRule;
   typedef std::vector<ExRule> SerieExRestrictions;
   SerieExRestrictions ExRestrictions;
   SerieExRestrictions ExRefine;

   /// \brief Bit string integer (each one considered as a boolean)
   ///        Bit 0 : Skip Sequences,    if possible
   ///        Bit 1 : Skip Shadow Groups if possible
   ///        Probabely, some more to add
   int LoadMode;

   /// \brief whether we want to sort in direct order or not (reverse order).
   ///        To be used by aware user only
   bool DirectOrder;

   /// \brief If user knows more about his images than gdcm does,
   ///        he may supply his own comparison function.
    BOOL_FUNCTION_PFILE_PFILE_POINTER UserLessThanFunction;

    bool m_UseSeriesDetails;

};

} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
