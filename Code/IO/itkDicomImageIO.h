/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDicomImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $1.0$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDicomImageIO_h
#define __itkDicomImageIO_h

#include <fstream>
#include "itkImageIOBase.h"

namespace itk
{

/** \brief Read DicomImage file format. */
class ITK_EXPORT DicomImageIO : public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef DicomImageIO            Self;
  typedef ImageIOBase  Superclass;
  typedef SmartPointer<Self>  Pointer;

  //this structure is used to creat a table of tags
  typedef struct Bal
  {
    unsigned char Subtag1 [2]; 
    unsigned char Subtag2 [2];
    int count;
  } Tag;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DicomImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char*) ;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();

  /** Get the type of the pixel.  */
  virtual const std::type_info& GetPixelType() const;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer);

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a 
   * component size of 1 byte. */
  virtual unsigned int GetComponentSize() const;
  
  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char*);

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation();
  
  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void* buffer);


protected:
  DicomImageIO();
  ~DicomImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  DicomImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  void SwapBytesIfNecessary(void* buffer, unsigned long numberOfPixels);
 
  /** This function builds a list of tags required to read data in a
   * dicom file*/
  bool CheckTagTable(std::ifstream & inputStream,
                     std::list <Tag> &TableOfTags) const;

  /** This function puts the cursor of the stream on the first byte
   *  after the last balise at the end of the header */
  bool GoToTheEndOfHeader(std::ifstream & inputStream,
                          long int& i,Tag & tagcurrent) const;

  /** This function puts the cursor of the stream on the first byte
   * after the given balise */
  bool GoToTag(std::ifstream & inputStream, int balise1, int balise2,
               long int & i, long int & max, Tag & tagcurrent) const;

  /** return true if tag([0][1]) = tagvalue1 and tag([2][3]) = tagvalue2*/ 
  bool IfEqual(unsigned char * tag, int tagvalue1, int tagvalue2) const;
  
  // Position after ReadImageInformation.
  size_t m_InputPosition;
};

} // end namespace itk

#endif // __itkDicomImageIO_h
