/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGDCMImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGDCMImageIO_h
#define __itkGDCMImageIO_h

#include "itkImageIOBase.h"
#include "gdcm/src/gdcmHeader.h"

namespace itk
{

/** \brief ImageIO class for reading and writing DICOM v3 and ACR/NEMA images
 *  This class is only an adaptor to the gdcm library (currently gdcm 0.6.x is used):
 *  
 *  http://creatis-www.insa-lyon.fr/Public/Gdcm/
 *
 *  CREATIS INSA - Lyon 2003-2004
 *    http://www.creatis.insa-lyon.fr
 *
 *  \warning There are several restrictions to this current writer:
 *           1. Basically you always need a DICOM as input to write a proper DICOM image file
 *              (As of 12/10/2004 this restriction is solved in GDCM CVS repository)
 *           2. Eventhough during the writing process you pass in a DICOM file as input
 *              The output file may not contains ALL DICOM field from the input file.
 *              In particular: - The SeQuence DICOM field (SQ).
 *                             - The Binary DICOM field (a non human readable string)
 *                             - Fields from Private Dictionary with unresolved Name (= unknown at runtime)
 *
 *  \ingroup IOFilters
 *
 */
class ITK_EXPORT GDCMImageIO : public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef GDCMImageIO Self;
  typedef ImageIOBase Superclass;
  typedef SmartPointer<Self>  Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GDCMImageIO, Superclass);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char*);
  
  /** Set the spacing and dimesion information for the current filename. */
  virtual void ReadImageInformation();
  
  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char*);

  /** Writes the spacing and dimentions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation();
  
  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void* buffer);
  
  /** Macro to access Rescale Slope and Rescale Intercept. Which are
   * needed to rescale properly image when needed. User then need to 
   * Always check those value when access value from the DICOM header */
  itkGetMacro(RescaleSlope, double);
  itkGetMacro(RescaleIntercept, double);

protected:
  GDCMImageIO();
  ~GDCMImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;

  bool OpenGDCMFileForReading(std::ifstream& os, const char* filename);
  bool OpenGDCMFileForWriting(std::ofstream& os, const char* filename);
  void InternalReadImageInformation(std::ifstream& file);

  double m_RescaleSlope;
  double m_RescaleIntercept;
#if GDCM_MAJOR_VERSION == 0 && GDCM_MINOR_VERSION <= 5
  ::gdcmHeader *m_GdcmHeader;
#endif

private:
  GDCMImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#endif // __itkGDCMImageIO_h
