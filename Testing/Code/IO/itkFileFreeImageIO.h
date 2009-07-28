/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileFreeImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFileFreeImageIO_h
#define __itkFileFreeImageIO_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkImageIOBase.h"
#include <vector>
#include <string>

namespace itk
{
/** \class FileFreeImageIO
 * \brief ImageIO object for reading images from memory
 *
 * The "filename" specified will look like a URI:
 *     fileFree:
 *
 */
class ITK_EXPORT FileFreeImageIO : public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef FileFreeImageIO            Self;
  typedef ImageIOBase                Superclass;
  typedef SmartPointer<Self>         Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileFreeImageIO, ImageIOBase);

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char*);
  
  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();
  
  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char*);

  /** Writes the header of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void* buffer);

protected:
  FileFreeImageIO();
  ~FileFreeImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  FileFreeImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  void SplitString (const std::string &text,
                    const std::string &separators,
                    std::vector<std::string> &words);
};

} // end namespace itk
#endif // __itkFileFreeImageIO_h

