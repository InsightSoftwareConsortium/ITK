/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkArchetypeSeriesFileNames_h
#define itkArchetypeSeriesFileNames_h
#include "ITKIOImageBaseExport.h"


#include "itkObject.h"
#include "itkObjectFactory.h"
#include <vector>
#include <string>

namespace itk
{
/** \class ArchetypeSeriesFileNames
 * \brief Generate an ordered sequence of filenames.
 *
 * This class generates an ordered sequence of files based on an
 * archetypical filename.  From the archetypical filename, a set of
 * regular expressions is created to group filenames based on numeric
 * substrings.  There can be multiple numeric substrings in the
 * archetype.  When this occurs, ArchetypeSeriesFileNames can not
 * determine which numeric substring refers to the "image number" and
 * which numeric substring refers to the "series" or "study". By
 * default, the ArchetypeSeriesFileNames assumes the rightmost numeric
 * substring refers to the image number, and this is the group of
 * filenames returned by default.  However, the other groupings of
 * filenames can also be queried by passing in a group number to the
 * GetFileNames() method. Groups are numbered by the numeric
 * substrings from right to left in the archetype.
 *
 * For example, if a directory contains the files
 *
 *         foo_5_1.png
 *         foo_5_2.png
 *         foo_5_3.png
 *         foo_6_1.png
 *         foo_6_2.png
 *         foo_6_3.png
 *
 * and specifying an archetypical file foo_5_1.png, the filename list
 * will contain
 *
 *         foo_5_1.png
 *         foo_5_2.png
 *         foo_5_3.png
 *
 *  \ingroup IOFilters
 *
 * \ingroup ITKIOImageBase
 */

class ITKIOImageBase_EXPORT ArchetypeSeriesFileNames : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ArchetypeSeriesFileNames);

  /** Standard class type aliases. */
  using Self = ArchetypeSeriesFileNames;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ArchetypeSeriesFileNames, Object);

  /* -------- Define the API for ArchetypeSeriesFileNames ----------- */

  // The archetypical filename from which to generate the regular
  // expressions
  void
  SetArchetype(const std::string & archetype);

  itkGetStringMacro(Archetype);

  using VectorSizeType = size_t;

  /** Get the number of groupings that match the Archetype */
  VectorSizeType
  GetNumberOfGroupings();

  /** Helper types for managing the groups of filenames and their sizes */
  using IntVectorType = std::vector<int>;
  using StringVectorType = std::vector<std::string>;

  /** Returns a vector containing the series' file names. The file
   * names are ordered by Index. Defaults to returning the filenames
   * to the rightmost grouping. */
  const StringVectorType &
  GetFileNames(VectorSizeType group = 0);

protected:
  ArchetypeSeriesFileNames();
  ~ArchetypeSeriesFileNames() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Method that actually does the archetype matching/grouping */
  void
  Scan();

private:
  /** A string for formatting the names of files in the series. */
  std::string m_Archetype;

  std::vector<StringVectorType> m_Groupings;
  StringVectorType              m_FileNames; // ivar for returning by
                                             // reference

  TimeStamp m_ArchetypeMTime;
  TimeStamp m_ScanTime;
};
} // namespace itk

#endif // itkArchetypeSeriesFileNames_h
