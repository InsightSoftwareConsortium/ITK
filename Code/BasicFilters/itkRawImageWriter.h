/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageWriter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
  void PrintSelf(std::ostream& os, Indent indent);

  void WriteData();
  
private:
  ByteOrder m_ByteOrder;

};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRawImageWriter.txx"
#endif

#endif
