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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
/**
 * \file   itkGE4ImageIO.h
 *         Much of the code for this file reader/writer was taken from
 *         the University of Iowa Imaging Group library with the
 *         permission of the authors, Milan Sonka, Joseph Reinhardt,
 *         Ryan Long, Hans Johnson, Gary Christensen, and others.
 *         The specification for this file format is taken from the
 *         web site http://analyzedirect.com/support/10.0Documents/Analyze_Resource_01.pdf
 * \author Kent Williams
 *         The University of Iowa 2003
 * \brief This file was written as a modification to the itkMetaImageIO
 *        as a new method for reading in files from the GE4 scanner.
 */

#ifndef itkGE4ImageIO_h
#define itkGE4ImageIO_h
#include "ITKIOGEExport.h"

#include "itkIPLCommonImageIO.h"

namespace itk
{
/** \class GE4ImageIO
 *
 * \author Hans J. Johnson
 * \brief Class that defines how to read GE4 file format.
 *
 * \ingroup IOFilters
 * \ingroup ITKIOGE
 */
class ITKIOGE_EXPORT GE4ImageIO:public IPLCommonImageIO
{
public:
  /** Standard class typedefs. */
  typedef GE4ImageIO           Self;
  typedef IPLCommonImageIO     Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GE4ImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
   * \author Hans J Johnson
   * \param FileNameToRead The name of the file to test for reading.
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can read the file specified.
   */
  virtual bool CanReadFile(const char *FileNameToRead) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  // Implemented in superclass
  //      virtual void ReadImageInformation();

  /** Get the type of the pixel.  */
  // Implemented in superclass
  // virtual const std::type_info& GetPixelType() const;

  /** Reads the data from disk into the memory buffer provided. */
  // Implemented in superclass
  //      virtual void Read(void* buffer);

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a
   * component size of 1 byte. */
  // Implemented in superclass
  // virtual unsigned int GetComponentSize() const;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /* * Determine if the file can be written with this ImageIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \author Hans J. Johnson
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can write the file specified.
  */
  // Implemented in superclass
  // virtual bool CanWriteFile(const char * FileNameToWrite);

  /* * Set the spacing and dimension information for the set filename. */
  // Implemented in superclass
  // virtual void WriteImageInformation();

  /* * Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  // Implemented in superclass
  // virtual void Write(const void* buffer);

protected:
  GE4ImageIO();
  ~GE4ImageIO() ITK_OVERRIDE;
  // Implemented in Superclass
  // void PrintSelf(std::ostream& os, Indent indent) const;
  virtual GEImageHeader * ReadHeader(const char *FileNameToRead) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GE4ImageIO);

  float MvtSunf(int numb);

};
} // end namespace itk

#endif // itkGE4ImageIO_h
