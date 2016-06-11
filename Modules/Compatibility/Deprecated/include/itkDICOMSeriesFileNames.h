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
#ifndef itkDICOMSeriesFileNames_h
#define itkDICOMSeriesFileNames_h
#if !defined( ITK_LEGACY_REMOVE )


#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkMacro.h"
#include <vector>

#include "DICOMParser.h"
#include "DICOMAppHelper.h"

namespace itk
{
/** \class DICOMSeriesFileNames
 * \brief Generate an ordered sequence of filenames.
 *
 * This class generates an ordered sequence of filenames based on the
 * DICOM tags in the files. Files can be sorted based on image number,
 * slice location, or patient position. The files in the specified
 * directory are grouped by SeriesUID.  The list of SeriesUIDs can be
 * queried and the filenames for a specific series extracted.
 *
 * \deprecated
 * \ingroup ITKDeprecated
 * \ingroup IOFilters
 */
class DICOMSeriesFileNames:public Object
{
public:
  /** Standard class typedefs. */
  typedef DICOMSeriesFileNames Self;
  typedef Object               Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DICOMSeriesFileNames, Object);

  /* -------- Define the API for DICOMSeriesFileNames ----------- */

  /** Set the directory containing the DICOM
   * files. DICOMSeriesFileNames caches information about filenames
   * and series UIDs. Calling SetDirectory() causes this information
   * to be cleared. This is the behavior even if SetDirectory() is
   * called with the same directory as the previous call.  In this
   * case, a second call to SetDirectory() with the same directory
   * name forces the directory to be rescanned.  This is useful if
   * DICOM files have been added to a directory since the last time
   * the directory was scanned. */
  void SetDirectory(const std::string & dir)
  {
    if ( m_Directory != dir )
      {
      m_Directory = dir;
      this->Modified();

      // Clear the SeriesUIDs and FileNames
      m_SeriesUIDs.clear();
      m_FileNames.clear();
      m_AppHelper.Clear();
      }

    // Keep track of when the directory name was set so we can
    // compare it when the directory was last scanned. We set this
    // modified time each time SetDirectory() is called. This allows
    // a call to SetDirectory() to force a directory to be rescanned
    // the next time GetFileNames() or GetSeriesUIDs() is called.
    m_DirectorySetTime.Modified();
  }

  /** Get the directory containing the DICOM files. */
  itkGetStringMacro(Directory);

  /** The sorting order can be ascending or descending. The default
   * sort order is ascending. */
  itkSetMacro(Ascending, bool);
  itkGetConstMacro(Ascending, bool);
  itkBooleanMacro(Ascending);

  /** Type of the array used for returning filenames */
  typedef std::vector< std::string > FileNamesArrayType;

  /** Returns a vector containing the UIDs for each series in the
   * directory. If parameter "recursive" is true, subdirectories will
   * be scanned. */
  const FileNamesArrayType & GetSeriesUIDs(bool recursive = false);

  /** Returns a vector containing the Descriptions for each series in the
   * directory. GetSeriesUIDs() should be called before */
  const FileNamesArrayType & GetSeriesDescriptions(){ return m_SeriesDescriptions; }
  const FileNamesArrayType & GetSeriesBodyParts(){ return m_BodyParts; }
  const FileNamesArrayType & GetSeriesScanOptions(){ return m_ScanOptions; }

  /** Returns a vector containing the series file names. The file
   * names are sorted based on the current sorting mode. If parameter
   * "recursive" is true, subdirectories will be scanned. */
  const FileNamesArrayType & GetFileNames(bool recursive = false);

  /** Returns a vector containing the file names for a specified
   * series UID. The file names are sorted based on the current
   * sorting mode. If parameter "recursive" is true, subdirectories
   * will be scanned. */
  const FileNamesArrayType & GetFileNames(const std::string & seriesUID,
                                          bool recursive = false);

  /** Set the filename sorting order to sorting images based on the
   * DICOM field of slice number, the DICOM field of slice location,
   * or the position of the image computed using the
   * ImagePositionPatient and ImageOrientationPatient DICOM fields. */
  typedef enum { SortByImageNumber, SortBySliceLocation, SortByImagePositionPatient } FileNameSortingOrderType;
  itkSetEnumMacro(FileNameSortingOrder, FileNameSortingOrderType);
  itkGetEnumMacro(FileNameSortingOrder, FileNameSortingOrderType);
  void SetFileNameSortingOrderToSortByImageNumber()
  { this->SetFileNameSortingOrder(SortByImageNumber); }
  void SetFileNameSortingOrderToSortBySliceLocation()
  { this->SetFileNameSortingOrder(SortBySliceLocation); }
  void SetFileNameSortingOrderToSortByImagePositionPatient()
  { this->SetFileNameSortingOrder(SortByImagePositionPatient); }

  /** Get the filename associated with a specific instance UID. This
   * requires the internal database has already been built via a call
   * to GetFileNames() */
  std::string GetFileName(const std::string & instanceUID);

protected:
  DICOMSeriesFileNames();
  ~DICOMSeriesFileNames() {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DICOMSeriesFileNames);

  int CanReadFile(const char *fname);

  void RecurseDirectory(std::string directory, FileNamesArrayType & filenames);

  itkdicomparser::DICOMParser    m_Parser;
  itkdicomparser::DICOMAppHelper m_AppHelper;

  bool               m_Ascending;
  std::string        m_Directory;
  FileNamesArrayType m_FileNames;
  FileNamesArrayType m_SeriesUIDs;
  FileNamesArrayType m_SeriesDescriptions;
  FileNamesArrayType m_BodyParts;
  FileNamesArrayType m_ScanOptions;

  FileNameSortingOrderType m_FileNameSortingOrder;

  TimeStamp m_DirectorySetTime;
  TimeStamp m_DirectoryScanTime;
};
} //namespace ITK

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif // itkDICOMSeriesFileNames_h
