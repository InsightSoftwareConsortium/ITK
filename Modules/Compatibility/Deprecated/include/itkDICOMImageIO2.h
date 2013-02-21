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
#ifndef __itkDICOMImageIO2_h
#define __itkDICOMImageIO2_h
#if !defined( ITK_LEGACY_REMOVE )

#include <fstream>
#include "itkImageIOBase.h"

#include "DICOMParser.h"
#include "DICOMAppHelper.h"

namespace itk
{
/**
 * \class DICOMImageIO2
 *  \brief Read DICOMImage file format.
 * \deprecated
 * \ingroup ITKDeprecated
 */
class ITK_EXPORT DICOMImageIO2:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef DICOMImageIO2        Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DICOMImageIO2, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *);

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();

  /** Get the type of the pixel.  */
  // virtual const std::type_info& GetPixelType() const;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer);

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of uint8_t would have a
   * component size of 1 byte. */
  // virtual unsigned int GetComponentSize() const;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *) { return false; }

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation() {}

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *) {}

  /** Get methods to query patient information and scanner information */
  void GetPatientName(char *name);

  void GetPatientID(char *id);

  void GetPatientSex(char *sex);

  void GetPatientAge(char *age);

  void GetStudyID(char *id);

  void GetPatientDOB(char *dob);

  void GetStudyDescription(char *desc);

  void GetBodyPart(char *part);

  void GetNumberOfSeriesInStudy(char *series);

  void GetNumberOfStudyRelatedSeries(char *series);

  void GetStudyDate(char *date);

  void GetModality(char *modality);

  void GetManufacturer(char *manu);

  void GetInstitution(char *ins);

  void GetModel(char *model);

protected:
  DICOMImageIO2();
  virtual ~DICOMImageIO2();
  void PrintSelf(std::ostream & os, Indent indent) const;

  itkdicomparser::DICOMParser    *m_Parser;
  itkdicomparser::DICOMAppHelper *m_AppHelper;

  void ReadDataCallback(doublebyte group,
                        doublebyte element,
                        itkdicomparser::DICOMParser::VRTypes type,
                        uint8_t *val,
                        quadbyte len);

  uint8_t *m_ImageDataBuffer;

private:
  DICOMImageIO2(const Self &);  //purposely not implemented
  void operator=(const Self &); //purposely not implemented
};
} // end namespace itk

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif // __itkDICOMImageIO2_h
