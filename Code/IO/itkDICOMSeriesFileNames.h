/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDICOMSeriesFileNames.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDICOMSeriesFileNames_h
#define __itkDICOMSeriesFileNames_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkExceptionObject.h"
#include <vector>

#include "DICOMParser.h"
#include "DICOMAppHelper.h"

namespace itk
{

/** \class DICOMSeriesFileNames
 * \brief Generate an ordered sequence of filenames.
 *
 * This class generates an ordered sequence of filenames based on the
 * DICOM tags in the files. Currently, the files are sorted by image
 * number. Other sorting orders may be provided in the future.
 * When the GetFileNames method is applied, the driectory specified
 * with the SetDirectory() method processes each file in the
 * directory. Only valid DICOM files are accepted. There may be
 * multiple DICOM series withing the directory. Currently, the
 * filenames from the first series are returned.
 *
 * \ingroup IOFilters
 *
 */

class ITK_EXPORT DICOMSeriesFileNames : public Object
{
public:
  /** Standard class typedefs. */
  typedef DICOMSeriesFileNames    Self;
  typedef Object                  Superclass;
  typedef SmartPointer<Self>      Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DICOMSeriesFileNames, Object);

  /* -------- Define the API for DICOMSeriesFileNames ----------- */

  /** The directory containing the DICOM files. */
  itkSetStringMacro(Directory);
  itkGetStringMacro(Directory);

  /** Returns a vector containing the series' file names. The file
    * names are sorted by image number. */
  const std::vector<std::string> &GetFileNames ();

  /** Set the filename sorting order to sorting images based on the
   * DICOM field of slice number, the DICOM field of slice location,
   * or the position of the image computed using the
   * ImagePositionPatient and ImageOrientationPatient DICOM fields. */
  typedef enum {SortByImageNumber, SortBySliceLocation, SortByImagePositionPatient} FileNameSortingOrderType;
  itkSetMacro(FileNameSortingOrder, FileNameSortingOrderType);
  itkGetMacro(FileNameSortingOrder, FileNameSortingOrderType);
  void SetFileNameSortingOrderToSortByImageNumber()
    { this->SetFileNameSortingOrder(SortByImageNumber); }
  void SetFileNameSortingOrderToSortBySliceLocation()
    { this->SetFileNameSortingOrder(SortBySliceLocation); }
  void SetFileNameSortingOrderToSortByImagePositionPatient()
    { this->SetFileNameSortingOrder(SortByImagePositionPatient); }


protected:
  DICOMSeriesFileNames();
  ~DICOMSeriesFileNames() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  DICOMSeriesFileNames(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  int CanReadFile(const char* fname);

  DICOMParser m_Parser;
  DICOMAppHelper m_AppHelper;

  std::string m_Directory;
  std::vector<std::string>  m_FileNames;

  FileNameSortingOrderType m_FileNameSortingOrder;
};

} //namespace ITK

#endif // __itkDICOMSeriesFileNames_h
