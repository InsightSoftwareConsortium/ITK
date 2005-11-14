/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioRadImageIO.h
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
#ifndef __itkBioRadImageIO_h
#define __itkBioRadImageIO_h

#include "itkImageIOBase.h"
#include <fstream>

namespace itk
{

/** \class BioRadImageIO
 *
 *  \brief ImageIO class for reading Bio-Rad images.
 *  Bio-Rad file format are used by confocal micropscopes like MRC 1024, MRC 600
 *  http://www.bio-rad.com/
 *
 * The reader/writer was based on a scanned copy of the MRC-600 documentation
 * http://forums.ni.com/attachments/ni/200/7567/1/file%20format.pdf
 *
 * \ingroup IOFilters
 *
 */
class ITK_EXPORT BioRadImageIO : public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef BioRadImageIO            Self;
  typedef ImageIOBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BioRadImageIO, Superclass);

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
  virtual void WriteImageInformation() {};
  
  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void* buffer);

protected:
  BioRadImageIO();
  ~BioRadImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;

  bool OpenBioRadFileForReading(std::ifstream& os, const char* filename);
  bool OpenBioRadFileForWriting(std::ofstream& os, const char* filename);
  void InternalReadImageInformation(std::ifstream& file);

private:
  BioRadImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#endif // __itkBioRadImageIO_h
