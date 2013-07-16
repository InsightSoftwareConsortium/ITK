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
#ifndef __itkGiplImageIO_h
#define __itkGiplImageIO_h


#include <fstream>
#include "itkImageIOBase.h"
#include <cstdio>

namespace itk
{
class GiplImageIOInternals;

/** \class GiplImageIO
 *
 *  \brief Read GiplImage file format.
 *
 *  \ingroup IOFilters
 *
 * \ingroup ITKIOGIPL
 */
class GiplImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef GiplImageIO          Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GiplImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *);

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *);

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer);

  GiplImageIO();
  ~GiplImageIO();
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  GiplImageIO(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  void SwapBytesIfNecessary(void *buffer, SizeValueType numberOfPixels);

  bool CheckExtension(const char *);

  std::ifstream m_Ifstream;
  std::ofstream m_Ofstream;
  bool          m_IsCompressed;

  GiplImageIOInternals *m_Internal;
};
} // end namespace itk

#endif // __itkGiplImageIO_h
