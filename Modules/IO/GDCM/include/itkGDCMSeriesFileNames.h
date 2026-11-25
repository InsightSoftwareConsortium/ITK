/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkGDCMSeriesFileNames_h
#define itkGDCMSeriesFileNames_h

#include "itkProcessObject.h"
#include "itkObjectFactory.h"
#include "itkMacro.h"
#include <map>
#include <utility>
#include <vector>
#include "ITKIOGDCMExport.h"

namespace itk
{
/**
 * \class GDCMSeriesFileNames
 * \brief Generate a sequence of filenames from a DICOM series.
 *
 * This class generates a sequence of files whose filenames point to
 * a DICOM file. The ordering is based on the following strategy:
 * Read all images in the directory (assuming there is only one study/series)
 *
 *   1. Extract Image Orientation & Image Position from DICOM images, and then
 *      calculate the ordering based on the 3D coordinate of the slice.
 *   2. If the geometric ordering fails (duplicate positions, inconsistent
 *      orientation), an exception is thrown by default; see
 *      FailOnAmbiguousOrdering. When FailOnAmbiguousOrdering is false, the
 *      ordering falls back to 'Instance Number' when unique, else to
 *      lexicographic filename order (and in either case, DidUseAmbiguousOrdering
 *      is set to true).
 *
 *  If multiple volumes are being grouped as a single series for your
 *    DICOM objects, you may want to try calling SetUseSeriesDetails(true)
 *    prior to calling SetDirectory().
 *
 * \ingroup IOFilters
 *
 * \ingroup ITKIOGDCM
 */

/** \todo: remove these from the itk:: namespace */
using FilenamesContainer = std::vector<std::string>;
using SerieUIDContainer = std::vector<std::string>;

class ITKIOGDCM_EXPORT GDCMSeriesFileNames : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GDCMSeriesFileNames);

  /** Standard class type aliases. */
  using Self = GDCMSeriesFileNames;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;

  /** Type of the container that holds the file names in the series. */
  using FileNamesContainerType = FilenamesContainer;

  /** Type of the container that holds the UID's for the series. */
  using SeriesUIDContainerType = SerieUIDContainer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(GDCMSeriesFileNames);

  /* -------- Define the API for GDCMSeriesFileNames ----------- */

  /** Set the directory that contains the DICOM series. */
  void
  SetInputDirectory(const char * name);

  /** Set the directory that contains the DICOM series. */
  void
  SetInputDirectory(const std::string & name);

  /** Set the directory that contains the DICOM series. */
  void
  SetDirectory(const std::string & name)
  {
    SetInputDirectory(name);
  }

  /** Returns a vector containing the series' file names. The file
   * names are ordered by the strategy define in header.
   * No sorting is done based on UID */
  const FileNamesContainerType &
  GetInputFileNames();

  /** Set the directory where the output DICOM series should be written. */
  void
  SetOutputDirectory(const std::string & name)
  {
    m_OutputDirectory = name;
    this->Modified();
  }

  /** Returns a vector containing the series' file names. The file
   * names are ordered in the same exact order as the input one.
   * This could be dangerous if the writing has changed 3rd position
   * or some other DICOM tag in the header
   */
  const FileNamesContainerType &
  GetOutputFileNames();

  /** Returns a vector containing the series' file names. The file
   * names are ordered by the strategy define in header.
   * All DICOM files have the same exact UID equal to the one user's
   * specified.  An extended UID may be returned/used if
   * SetUseSeriesDetails(true) has been called.
   */
  const FileNamesContainerType &
  GetFileNames(const std::string serie);

  /** Returns a vector containing all the UIDs found when parsing the
   * directory specified via SetDirectory. If no directory is specified
   * return an empty vector.  An extended UID may be returned/used if
   * SetUseSeriesDetails(true) has been called.
   */
  const SeriesUIDContainerType &
  GetSeriesUIDs();

  /** Recursively parse the input directory.
   * Must be set before the call to SetInputDirectory(). */
  /** @ITKStartGrouping */
  itkSetMacro(Recursive, bool);
  itkGetConstMacro(Recursive, bool);
  itkBooleanMacro(Recursive);
  /** @ITKEndGrouping */

  /** Use additional series information such as ProtocolName
   *   and SeriesName to identify when a single SeriesUID contains
   *   multiple 3D volumes - as can occur with perfusion and DTI imaging.
   *   Off by default: series are identified by their SeriesInstanceUID
   *   (0020,000e) alone, per the DICOM information model.
   */
  void
  SetUseSeriesDetails(bool useSeriesDetails);

  /** Returns true if using additional series information such as ProtocolName
   *   and SeriesName to identify when a single SeriesUID contains
   *   multiple 3D volumes - as can occur with perfusion and DTI imaging
   */
  bool
  GetUseSeriesDetails() const
  {
    return m_UseSeriesDetails;
  }

  /** Add more restriction on the selection of a Series. This follows the same
   * approach as SetUseSeriesDetails, but allows a user to add even more DICOM
   * tags to take into account for subrefining a set of DICOM files into multiple
   * series. The tag format is "group|element" of a DICOM tag. Call order
   * relative to SetUseSeriesDetails does not matter.
   * \warning UseSeriesDetails needs to be set to true.
   */
  void
  AddSeriesRestriction(const std::string & tag);

  /** When true, throw an exception when a series cannot be ordered geometrically by
   * gdcm::IPPSorter (duplicate ImagePositionPatient, inconsistent
   * orientation). When false, fall back to the legacy SerieHelper
   * heuristics: Instance Number when unique, else lexicographic filename
   * order. The fallback is not DICOM-standards conforming and its output
   * should not be trusted; it exists only for backward compatibility. */
  /** @ITKStartGrouping */
  itkSetMacro(FailOnAmbiguousOrdering, bool);
  itkGetConstMacro(FailOnAmbiguousOrdering, bool);
  itkBooleanMacro(FailOnAmbiguousOrdering);
  /** @ITKEndGrouping */

  /** If ambiguous ordering is encountered, this is set to true. */
  /** @ITKStartGrouping */
  itkGetConstMacro(DidUseAmbiguousOrdering, bool);
  /** @ITKEndGrouping */

  /** No effect with the gdcm::Scanner backend (retained for source
   *  compatibility). Series enumeration reads only the grouping and
   *  ordering tags, so sequences are never parsed during the scan.
   */
  /** @ITKStartGrouping */
  itkSetMacro(LoadSequences, bool);
  itkGetConstMacro(LoadSequences, bool);
  itkBooleanMacro(LoadSequences);
  /** @ITKEndGrouping */

  /** No effect with the gdcm::Scanner backend (retained for source
   *  compatibility). Series enumeration reads only the grouping and
   *  ordering tags, so private tags are never parsed during the scan.
   */
  /** @ITKStartGrouping */
  itkSetMacro(LoadPrivateTags, bool);
  itkGetConstMacro(LoadPrivateTags, bool);
  itkBooleanMacro(LoadPrivateTags);
  /** @ITKEndGrouping */
protected:
  GDCMSeriesFileNames();
  ~GDCMSeriesFileNames() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Contains the input directory where the DICOM series is found */
  std::string m_InputDirectory = "";

  /** Contains the output directory where the DICOM series should be written */
  std::string m_OutputDirectory = "";

  /** Internal structure to keep the list of input/output filenames */
  FileNamesContainerType m_InputFileNames{};
  FileNamesContainerType m_OutputFileNames{};

  /** Parse the input directory into the per-series file-name map. */
  void
  BuildSeriesMap();

  /** Files of one series; ordered lazily on the first GetFileNames call. */
  struct SeriesEntry
  {
    FileNamesContainerType Files{};
    bool                   Ordered{ false };
  };

  /** Order one series geometrically, or apply the legacy fallback / throw
   * per FailOnAmbiguousOrdering. */
  void
  OrderSeries(SeriesEntry & entry);

  /** File names per distinct series identifier. */
  std::map<std::string, SeriesEntry> m_SeriesFiles{};

  /** Instance Number (0020,0013) per scanned file, for the legacy fallback. */
  std::map<std::string, std::string> m_InstanceNumbers{};

  /** User (group,element) tags from AddSeriesRestriction, appended to the
   * series identifier (after the default detail tags) when UseSeriesDetails
   * is enabled. */
  std::vector<std::pair<unsigned short, unsigned short>> m_UserRefineTags{};

  /** Modified time of the last directory parse; the cache is rebuilt only
   * when the object has been Modified() since. */
  TimeStamp m_CacheBuildTime{};

  /** Internal structure to keep the list of series UIDs */
  SeriesUIDContainerType m_SeriesUIDs{};

  bool m_UseSeriesDetails = false;
  bool m_FailOnAmbiguousOrdering = true;
  bool m_DidUseAmbiguousOrdering = false;
  bool m_Recursive = false;
  bool m_LoadSequences = false;
  bool m_LoadPrivateTags = false;
};
} // namespace itk

#endif // itkGDCMSeriesFileNames_h
