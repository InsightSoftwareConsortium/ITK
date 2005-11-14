/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLSMImageIO.h
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
#ifndef __itkLSMImageIO_h
#define __itkLSMImageIO_h

#include "itkTIFFImageIO.h"
#include <fstream>

namespace itk
{

/** \class LSMImageIO
 *
 *  \brief ImageIO class for reading LSM (Zeiss) images
 * LSM is a line of confocal laser scanning microscopes produced by the Zeiss company
 * LSM files are essentially extensions of the TIFF multiple image stack file format.
 *
 * \ingroup IOFilters
 *
 */
class ITK_EXPORT LSMImageIO : public TIFFImageIO
{
public:
  /** Standard class typedefs. */
  typedef LSMImageIO            Self;
  typedef ImageIOBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LSMImageIO, Superclass);

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
  LSMImageIO();
  ~LSMImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  LSMImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  void FillZeissStruct(char *z);
};

} // end namespace itk

#endif // __itkLSMImageIO_h
