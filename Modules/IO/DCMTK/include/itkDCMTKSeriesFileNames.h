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
#ifndef itkDCMTKSeriesFileNames_h
#define itkDCMTKSeriesFileNames_h
#include "ITKIODCMTKExport.h"

#include "itkProcessObject.h"
#include "itkObjectFactory.h"
#include "itkMacro.h"
#include <vector>

namespace itk
{
/** \class DCMTKSeriesFileNames
 * \brief Generate a sequence of filenames from a DICOM series.
 *
 * This class generate a sequence of files whose filenames points to
 * a DICOM file. The ordering is based on the following strategy:
 * Read all images in the directory (assuming there is only one study/serie)
 *
 *   1. Extract Image Orientation & Image Position from DICOM images, and then
 *      calculate the ordering based on the 3D coordinate of the slice
 *   2. If for some reason this information is not found or failed, another
 *      strategy is used: the ordering is based on 'Image Number'
 *   3. If this strategy also failed, then the filenames are ordered by
 *      lexicographical order.
 *
 *  If multiple volumes are being grouped as a single series for your
 *    dicom objects, you may want to try calling ->SetUseSeriesDetails(true)
 *    prior to calling SetDirectory().
 *
 * \ingroup IOFilters
 *
 * \ingroup ITKIODCMTK
 */
class ITKIODCMTK_EXPORT DCMTKSeriesFileNames:public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef DCMTKSeriesFileNames Self;
  typedef ProcessObject        Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Type of the container that holds the UID's for the series. */
  typedef std::vector< std::string > SeriesUIDContainerType;
  /** For backwards compatibility. */
  typedef SeriesUIDContainerType SeriesUIDContainer;

  /** Type of the container that holds the file names in the series. */
  typedef std::vector< std::string > FileNamesContainerType;
  /** For backwards compatibility */
  typedef FileNamesContainerType FilenamesContainer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DCMTKSeriesFileNames, ProcessObject);

  /* -------- Define the API for DCMTKSeriesFileNames ----------- */

  /** Set the directory that contains the DICOM series. */
  void SetInputDirectory(const char *name);

  /** Set the directory that contains the DICOM series. */
  void SetInputDirectory(std::string const & name);

  /** Set the directory that contains the DICOM series. */
  void SetDirectory(std::string const & name)
  {
    SetInputDirectory(name);
  }

  /** Returns a vector containing the series' file names. The file
   * names are ordered by the strategy define in header.
   * No sorting is done based on UID */
  const FileNamesContainerType & GetInputFileNames();

  /** Set the directory where the output DICOM serie should be written. */
  void SetOutputDirectory(std::string const & name)
  {
    m_OutputDirectory = name;
    this->Modified();
  }

  /** Returns a vector containing the series' file names. The file
   * names are ordered in the same extact order as the input one.
   * This could be dangerous if the writing has changed 3rd position
   * or some other DICOM tag in the header
   */
  const FileNamesContainerType & GetOutputFileNames();

  /** Returns a vector containing the series' file names. The file
   * names are ordered by the strategy define in header.
   * All DICOM files have the same exact UID equal to the one user's
   * specified.  An extended UID may be returned/used if
   * SetUseSeriesDetails(true) has been called.
   */
  const FileNamesContainerType & GetFileNames(const std::string serie);

  /** Returns a vector containing all the UIDs found when parsing the
   * direcory specified via SetDirectory. If no direcory is specified
   * return an empty vector.  An extended UID may be returned/used if
   * SetUseSeriesDetails(true) has been called.
   */
  const SeriesUIDContainerType & GetSeriesUIDs();

  /** Recursively parse the input directory */
  itkSetMacro(Recursive, bool);
  itkGetConstMacro(Recursive, bool);
  itkBooleanMacro(Recursive);

  /** Use additional series information such as ProtocolName
   *   and SeriesName to identify when a single SeriesUID contains
   *   multiple 3D volumes - as can occur with perfusion and DTI imaging
   */
  void SetUseSeriesDetails(bool useSeriesDetails);

  /** Returns true if using additional series information such as ProtocolName
   *   and SeriesName to identify when a single SeriesUID contains
   *   multiple 3D volumes - as can occur with perfusion and DTI imaging
   */
  bool GetUseSeriesDetails(void)
  {
    return m_UseSeriesDetails;
  }

  /** Add more restriction on the selection of a Series. This follow the same
   * approach as SetUseSeriesDetails, but allow a user to add even more DICOM
   * tags to take into account for subrefining a set of DICOM files into multiple
   * series. Format for tag is "group|element" of a DICOM tag.
   * \warning User need to set SetUseSeriesDetails(true)
   */
  void AddSeriesRestriction(const std::string & /* tag */)
  {
    // m_SerieHelper->AddRestriction(tag);
  }

  /** Parse any sequences in the DICOM file. Defaults to false
   *  to skip sequences. This makes loading DICOM files faster when
   *  sequences are not needed.
   */
  itkSetMacro(LoadSequences, bool);
  itkGetConstMacro(LoadSequences, bool);
  itkBooleanMacro(LoadSequences);

  /** Parse any private tags in the DICOM file. Defaults to false
   * to skip private tags. This makes loading DICOM files faster when
   * private tags are not needed.
   */
  itkSetMacro(LoadPrivateTags, bool);
  itkGetConstMacro(LoadPrivateTags, bool);
  itkBooleanMacro(LoadPrivateTags);
protected:
  DCMTKSeriesFileNames();
  ~DCMTKSeriesFileNames();
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DCMTKSeriesFileNames);

  /** internal method for reading out filenames and UID lists */
  void GetDicomData(const std::string &series, bool saveFileNames);
  /** Contains the input directory where the DICOM serie is found */
  std::string m_InputDirectory;

  /** Contains the output directory where the DICOM serie should be written */
  std::string m_OutputDirectory;

  /** Internal structure to keep the list of input/output filenames */
  FileNamesContainerType m_InputFileNames;
  FileNamesContainerType m_OutputFileNames;

  /** Internal structure to order serie from one directory */

  /** Internal structure to keep the list of series UIDs */
  SeriesUIDContainerType m_SeriesUIDs;

  bool m_UseSeriesDetails;
  bool m_Recursive;
  bool m_LoadSequences;
  bool m_LoadPrivateTags;
};
} //namespace ITK

#endif // itkDCMTKSeriesFileNames_h
