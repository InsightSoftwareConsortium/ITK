/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileIteratorBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFileIteratorBase_h
#define __itkFileIteratorBase_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{

/** \class FileIteratorBase
 * \brief Base class defines an API for classes that generate an ordered 
 * sequence of filenames.
 * 
 * FileIteratorBase is a base class for those classes that are used by the
 * ImageSeriesReader and ImageSeriesWriter to read or write a sequence of 
 * files. FileIteratorBase defines an API that generates an ordered
 * sequence of filenames that are in turn used by subclasses of 
 * ImageIOBase (and similar classes) to read and write data.
 *
 * To use FileIteratorBase you must specify whether the instance is in
 * read or write mode. (This affects the generation of filename. In read
 * mode, files are assumed to exist. In write mode, filesnames are 
 * generated and probably do not initially exist.)
 * 
 *  \ingroup IOFilters
 *
 */
class ITK_EXPORT FileIteratorBase : public Object
{
public:
  /** Standard class typedefs. */
  typedef FileIteratorBase    Self;
  typedef Object              Superclass;
  typedef SmartPointer<Self>  Pointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileIteratorBase, Object);

  /** The format string used to generate the series. Different subclasses
   * require different characteristics of this string. For example, the
   * subclass NumericSeriesFileIterator requires a "%d" or some integral
   * format specified to be embedded in the string. */
  itkSetStringMacro(SeriesFormat);
  itkGetStringMacro(SeriesFormat);

  /** Set the modal behavior of the class: whether it is to be
   * used to write a sequence of files, or read a sequence of
   * existing files. Normally this is set by the ImageSeriesReader
   * or ImageSeriesWriter. */
  itkSetMacro(WriteMode,bool);
  itkGetMacro(WriteMode,bool);
  itkBooleanMacro(WriteMode);

  /** Return the first file in the sequence. If the empty string "" 
   * is returned, then there are no files in the sequence. */
  virtual const std::string& Begin() = 0;

  /** Return the next file in the sequence. If the empty string "" 
   * is returned, then there are no additional files in the sequence. */
  virtual const std::string& operator++() = 0;

  /** Return the previous file in the sequence. If the empty string "" 
   * is returned, then there are no previous files in the sequence. */
  virtual const std::string& operator--() = 0;

  /** Return the current filename in the sequence. If the empty string "" 
   * is returned, then there are no current file in the sequence. */
  virtual const std::string& operator*() const
  {return m_CurrentFileName;}

protected:
  FileIteratorBase();
  ~FileIteratorBase();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Set the iterator into either read or write mode. */
  bool m_WriteMode;

  /** A string for formatting the names of files in the series. */
  std::string m_SeriesFormat;

  /** The current filename in the sequence. */
  std::string m_CurrentFileName;

private:
  FileIteratorBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} //namespace ITK

#endif // __itkFileIteratorBase_h
