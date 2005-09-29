/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGDCMSeriesFileNames.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGDCMSeriesFileNames_h
#define __itkGDCMSeriesFileNames_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkExceptionObject.h"
#include <vector>
#include "gdcm/src/gdcmSerieHelper.h"

namespace itk
{

/** \class GDCMSeriesFileNames
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
 */
typedef std::vector<std::string> FilenamesContainer;
typedef std::vector<std::string> SerieUIDContainer;
class ITK_EXPORT GDCMSeriesFileNames : public Object
{
public:
  /** Standard class typedefs. */
  typedef GDCMSeriesFileNames     Self;
  typedef Object                  Superclass;
  typedef SmartPointer<Self>      Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GDCMSeriesFileNames, Object);

  /* -------- Define the API for GDCMSeriesFileNames ----------- */

  /** Set the directory that contains the DICOM series. */
  void SetInputDirectory (const char * name)
    {
    if( !name )
      {
      itkExceptionMacro(<<"SetInputDirectory() received a NULL string");
      }
    std::string fname = name;
    this->SetInputDirectory( fname );
    }

  /** Set the directory that contains the DICOM series. */
  void SetInputDirectory (std::string const &name)
    {
    m_InputDirectory = name;
    this->Modified();
    }

  /** Set the directory that contains the DICOM series. */
  void SetDirectory (std::string const &name)
    {
    m_Directory = name;
    m_SerieHelper->SetUseSeriesDetails( m_UseSeriesDetails );
    m_SerieHelper->SetDirectory( name ); //as a side effect it also execute
    this->Modified();
    }

  /** Returns a vector containing the series' file names. The file
   * names are ordered by the strategy define in header. */
  const FilenamesContainer &GetInputFileNames () ;

  /** Set the directory where the output DICOM serie should be written. */
  void SetOutputDirectory (std::string const &name)
    {
    m_OutputDirectory = name;
    this->Modified();
    }

  /** Returns a vector containing the series' file names. The file
   * names are ordered in the same extact order as the input one. 
   * This could be dangerous if the writting has change 3d position
   * or some other DICOM tag in the header
   */
  const FilenamesContainer &GetOutputFileNames () ;

  /** Returns a vector containing the series' file names. The file
   * names are ordered by the strategy define in header. 
   * All DICOM files have the same exact UID equal to the one user's 
   * specified.  An extended UID may be returned/used if 
   * SetUseSeriesDetails(true) has been called.
   */
  const FilenamesContainer &GetFileNames(const std::string serie);

  /** Returns a vector containing all the UIDs found when parsing the
   * direcory specified via SetDirectory. If no direcory is specified 
   * return an empty vector.  An extended UID may be returned/used if 
   * SetUseSeriesDetails(true) has been called.
   */
  const SerieUIDContainer &GetSeriesUIDs();

  /** Use additional series information such as ProtocolName
   *   and SeriesName to identify when a single SeriesUID contains
   *   multiple 3D volumes - as can occur with perfusion and DTI imaging
   */
  void SetUseSeriesDetails( bool useSeriesDetails)
    {
    m_UseSeriesDetails = useSeriesDetails;
    m_SerieHelper->SetUseSeriesDetails( m_UseSeriesDetails );
    }

  /** Returns true if using additional series information such as ProtocolName
   *   and SeriesName to identify when a single SeriesUID contains
   *   multiple 3D volumes - as can occur with perfusion and DTI imaging
   */
  bool GetUseSeriesDetails( void )
    {
    return m_UseSeriesDetails;
    }

  /** Returns a pointer to the SeriesHelper class.  This access allows
   *   the files as gdcm dicom objects in a series to be queried for
   *   dicom tag values prior to reading the series.   Such querying is
   *   useful to determine which series should be read - e.g., to determine
   *   which is the T2 scan, etc.
   */
  gdcm::SerieHelper * GetSeriesHelper( void )
    {
    return m_SerieHelper;
    }

protected:
  GDCMSeriesFileNames();
  ~GDCMSeriesFileNames();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  GDCMSeriesFileNames(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  /** Contains the input directory where the DICOM serie is found */
  std::string m_InputDirectory;

  /** Contains the output directory where the DICOM serie should be written */
  std::string m_OutputDirectory;

  /** Contains the directory where the DICOM serie is found */
  std::string m_Directory;

  /** Internal structure to keep the list of input/output filenames */
  FilenamesContainer  m_InputFileNames;
  FilenamesContainer  m_OutputFileNames;

  /** Internal structure to order serie from one directory */
  gdcm::SerieHelper *m_SerieHelper;

  /** Internal structure to keep the list of input/output filenames */
  FilenamesContainer  m_FileNames;

  /** Internal structure to keep the list of series UIDs */
  SerieUIDContainer m_SeriesUIDs;

  bool              m_UseSeriesDetails;
};

} //namespace ITK

#endif // __itkGDCMSeriesFileNames_h
