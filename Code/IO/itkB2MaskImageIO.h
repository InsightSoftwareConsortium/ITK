/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkB2MaskImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/**
 * \file   Much of the code for this file reader/writer was taken from 
 *         the University of Iowa Psychiatry departments
 *         Imaging Group with permission of the author Vincent Magnotta
 * \author Hans J. Johnson
 *         The University of Iowa 2002
 * \brief This file was written as a modification to the itkMetaImageIO
 *        as a new method for reading in files from the B2Mask specification.
 *        B2Masks are specified as a single file.  The first part of the file
 *        begins with an "IPL_HEADER_BEGIN" followed by ascii text describing the
 *        dimensions, and other important information about the data.  The header
 *        information concludes with  the "IPL_HEADER_END\n" identifier, and is immediatly followed by
 *        the binary portion of the file.  The binary portion contains a binary octreeHdr
 *        consisting of 6 unsigned integers, followed by sequentially defined unsigned short integers
 *        representing (WHITE=1, BLACK=0, GRAY=2) colors.  The GRAY color indicates that this is a branching node.
 */

/**
  * \todo
  *  \par Everything still needs to be done.
 */

#ifndef __itkB2MaskImageIO_h
#define __itkB2MaskImageIO_h

#include <fstream>
#include "itkImageIOBase.h"
#include "itkB2IPLHeaderInfo.h"
#include "itkOctree.h"

namespace itk
{
/**
 * \author Hans J. Johnson
 * \brief Class that defines how to read B2Mask file format. 
 * */
class ITK_EXPORT B2MaskImageIO : public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef B2MaskImageIO            Self;
  typedef ImageIOBase  Superclass;
  typedef SmartPointer<Self>  Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(B2MaskImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
   * \author Hans J Johnson
   * \param FileNameToRead The name of the file to test for reading.
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can read the file specified. 
   */
  virtual bool CanReadFile(const char* FileNameToRead) ;

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

  /** Determine if the file can be written with this ImageIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \author Hans J. Johnson
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can write the file specified. 
   */
  virtual bool CanWriteFile(const char * FileNameToWrite);

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void* buffer);


protected:
  B2MaskImageIO();
  ~B2MaskImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;
private:
  B2MaskImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  itk::OctreeNodeBranch *  readOctree (
        std::ifstream & octreestream,
        const ImageIOBase::ByteOrder machineByteOrder,
        const ImageIOBase::ByteOrder fileByteOrder);
  ImageIOBase::ByteOrder m_MachineByteOrder;

  itk::B2IPLHeaderInfo m_IPLHeaderInfo;
  OctreeBase *m_Octree;
};

} // end namespace itk

#endif // __itkB2MaskImageIO_h
