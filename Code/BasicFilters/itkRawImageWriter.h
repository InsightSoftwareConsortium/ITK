/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageWriter.h
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
#ifndef __itkRawImageWriter_h
#define __itkRawImageWriter_h

#include "itkImageWriter.h"
#include <vector>

namespace itk
{

/** \class RawImageWriter
 * \brief Write an image (n-dimensional) in raw format.
 *
 * RawImageWriter writes n-dimensional images in raw file format. 
 * The raw format is just a sequence of numbers representing pixel
 * (i.e., image) values in the order of image definition (i.e., row, 
 * then slice, then volume, etc.) You can specify
 * binary or ASCII output types, as well as the byte order (little
 * endian or big endian).
 *
 * \ingroup IOFilters
 */
template <class TInputImage>
class ITK_EXPORT RawImageWriter : public ImageWriter<TInputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RawImageWriter       Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageWriter<TInputImage>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RawImageWriter,ImageWriter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Enums used to specify the byte order.
   */
  typedef  enum {BigEndian,LittleEndian} ByteOrder;
  
  /** 
   * Set the ITK file type. The default is BigEndian.
   */
  itkSetMacro(ByteOrder,ByteOrder);
  
  /** 
   * Get the byte order.
   */
  itkGetMacro(ByteOrder,ByteOrder);
                 
  /** 
   * Specify the byte order as big endian.
   */
  void SetByteOrderToBigEndian() 
    {this->SetByteOrder(RawImageWriter::BigEndian);}

  /** 
   * Specify the byte order as little endian.
   */
  void SetByteOrderToLittleEndian() 
    {this->SetByteOrder(RawImageWriter::LittleEndian);}

protected:
  RawImageWriter();
  ~RawImageWriter() {}
  RawImageWriter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  void WriteData();
  
private:
  ByteOrder m_ByteOrder;

};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRawImageWriter.txx"
#endif

#endif
