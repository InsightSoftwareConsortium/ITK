/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageWriter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * \ingroup IOFilters Deprecated
 */
template <class TInputImage>
class ITK_EXPORT RawImageWriter : public ImageWriter<TInputImage>
{
public:
  /** Standard class typedefs. */
  typedef RawImageWriter       Self;
  typedef ImageWriter<TInputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RawImageWriter,ImageWriter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Enums used to specify the byte order. */
  typedef  enum {BigEndian,LittleEndian} ByteOrder;
  
  /** Set the ITK file type. The default is BigEndian. */
  itkSetMacro(ByteOrder,ByteOrder);
  
  /** Get the byte order. */
  itkGetMacro(ByteOrder,ByteOrder);
                 
  /** Specify the byte order as big endian. */
  void SetByteOrderToBigEndian() 
  {this->SetByteOrder(RawImageWriter::BigEndian);}

  /** Specify the byte order as little endian. */
  void SetByteOrderToLittleEndian() 
  {this->SetByteOrder(RawImageWriter::LittleEndian);}

protected:
  RawImageWriter();
  ~RawImageWriter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  void WriteData();
  
private:
  RawImageWriter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  ByteOrder m_ByteOrder;

};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRawImageWriter.txx"
#endif

#endif
