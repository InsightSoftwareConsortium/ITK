/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDICOMImageIO2.h
  Language:  C++
  Date:      $Date$
  Version:   $1.0$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDICOMImageIO2_h
#define __itkDICOMImageIO2_h

#include <fstream>
#include "itkImageIOBase.h"

#include "DICOMParser.h"
#include "DICOMAppHelper.h"

namespace itk
{

/** \brief Read DICOMImage file format. */
class ITK_EXPORT DICOMImageIO2 : public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef DICOMImageIO2            Self;
  typedef ImageIOBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
 
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DICOMImageIO2, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char*) ;
  

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();

  /** Get the type of the pixel.  */
  // virtual const std::type_info& GetPixelType() const;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer);

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a 
   * component size of 1 byte. */
  // virtual unsigned int GetComponentSize() const;
  
  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char*) {return false;}

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation() {}
  
  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void*) {}


protected:
  DICOMImageIO2();
  ~DICOMImageIO2();
  void PrintSelf(std::ostream& os, Indent indent) const;

  DICOMParser Parser;
  DICOMAppHelper AppHelper; 

  void ReadDataCallback(doublebyte group,
                        doublebyte element,
                        DICOMParser::VRTypes type,
                        unsigned char* val,
                        quadbyte len);

  unsigned char* ImageDataBuffer;

private:
  DICOMImageIO2(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif // __itkDICOMImageIO2_h
