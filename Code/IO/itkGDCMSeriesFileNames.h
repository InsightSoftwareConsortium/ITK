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

namespace itk
{

/** \class GDCMSeriesFileNames
 * \brief Generate a sequence of filenames from a DICOM series.
 *
 * This class generate a sequence of files whose filenames points to 
 * a DICOM file. The oredring is based on the following strategy:
 * Read all images in the directory (assuming there is only one study/serie)
 *
 *   1. Extract Image Orientation & Image Position from DICOM images, and then
 *      calculate the ordering based on the 3D coordinate of the slice
 *   2. If for some reason this information is not found or failed, another 
 *      strategy is used: the ordering is based on 'Image Number'
 *   3. If this strategy also failed, then the filenames are ordered by 
 *      lexicographical order.
 *
 * \ingroup IOFilters
 *
 */
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

  /** Returns a vector containing the series' file names. The file
   * names are ordered by the strategy define in header. */
  const std::vector<std::string> &GetInputFileNames () ;

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
  const std::vector<std::string> &GetOutputFileNames () ;

protected:
  GDCMSeriesFileNames() {};
  ~GDCMSeriesFileNames() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  GDCMSeriesFileNames(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  /** Contains the input directory where the DICOM serie is found */
  std::string m_InputDirectory;

  /** Contains the output directory where the DICOM serie should be written */
  std::string m_OutputDirectory;

  /** Internal structure to keep the list of input/output filenames */
  std::vector<std::string>  m_InputFileNames;
  std::vector<std::string>  m_OutputFileNames;
};

} //namespace ITK

#endif // __itkGDCMSeriesFileNames_h
