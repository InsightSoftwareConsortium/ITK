/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkMetaImageIO_h
#define __itkMetaImageIO_h

#include <fstream>
#include "itkImageIOBase.h"

namespace itk
{

/** \brief Read MetaImage file format. */
class ITK_EXPORT MetaImageIO : public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef MetaImageIO            Self;
  typedef ImageIOBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char*) ;

  /** Set the spacing and dimention information for the set filename. */
  virtual void ReadImageInformation();
  
  /** Get the type of the pixel.  */
  virtual const std::type_info& GetPixelType() const;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer);

  /** Get the image origin. */
  virtual const double* GetOrigin() const;

  /** Get the image spacing. */
  virtual const double* GetSpacing() const;

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a 
   * component size of 1 byte. */
  virtual unsigned int GetComponentSize() const;

  /** Enums used to specify byte order; whether Big Endian or Little Endian. */
  typedef  enum {BigEndian,LittleEndian} ByteOrder;
  
  /** These methods indicate the byte ordering of the file you are trying
   * to read in. These methods will then either swap or not swap
   * the bytes depending on the byte ordering of the machine it is
   * being run on. For example, reading in a BigEndian file on a
   * BigEndian machine will result in no swapping. Trying to read
   * the same file on a LittleEndian machine will result in swapping.
   * Note: most UNIX machines are BigEndian while PC's
   * and VAX's are LittleEndian. So if the file you are reading
   * in was generated on a VAX or PC, SetImageByteOrderToLittleEndian 
   * otherwise SetImageByteOrderToBigEndian.  */
  itkSetMacro(ImageByteOrder,ByteOrder);
  itkGetConstMacro(ImageByteOrder,ByteOrder);
    
  /** Specify the byet ordering of the file. */
  void SetImageByteOrderToBigEndian()
    { this->SetImageByteOrder(BigEndian); }
  void SetImageByteOrderToLittleEndian()
    { this->SetImageByteOrder(LittleEndian); }
  
  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char*)
    { return false; }

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(void* buffer)
    { return; }

protected:
  MetaImageIO();
  ~MetaImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  MetaImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  void SwapBytesIfNecessary(void* buffer, unsigned long numberOfPixels);
  bool GetSeparatorCharacter(std::ifstream & ifs) const;

  ComponentType m_MetaPixelType;
  double m_Spacing[10];
  double m_Origin[10];
  ByteOrder      m_ImageByteOrder;
  std::ifstream   m_Ifstream;
  
};

} // end namespace itk

#endif // __itkMetaImageIO_h
