/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericSeriesFileIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericSeriesFileIterator_h
#define __itkNumericSeriesFileIterator_h

#include "itkFileIteratorBase.h"

namespace itk
{

/** \class NumericSeriesFileIterator
 * \brief Generate an ordered sequence of filenames.
 *
 * This class is used to generate an ordered sequence of files
 * whose filenames contain a single unique, non-negative, integral
 * value (e.g. test.1.png, test2.png, foo.3, etc.). This class can
 * be used in to write files in which case a sprintf-style series
 * format and a starting index must be specified; or to read files,
 * in which case you can either provide the series format, or the
 * class will attempt to deduce the format.
 *
 * \ingroup IOFilters
 *
 */
class ITK_EXPORT NumericSeriesFileIterator : public FileIteratorBase
{
public:
  /** Standard class typedefs. */
  typedef NumericSeriesFileIterator    Self;
  typedef FileIteratorBase             Superclass;
  typedef SmartPointer<Self>           Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NumericSeriesFileIterator, FileIteratorBase);

  /* -------- Satisfy the API of FileIteratorBase ------------- */

  /** Return the first file in the sequence. If the empty string "" 
   * is returned, then there are no files in the sequence. */
  virtual const std::string& Begin();

  /** Return the next file in the sequence. If the empty string "" 
   * is returned, then there are no additional files in the sequence. */
  virtual const std::string& operator++();

  /** Return the previous file in the sequence. If the empty string "" 
   * is returned, then there are no previous files in the sequence. */
  virtual const std::string& operator--();

  /* -------- Define the API for NumericSeriesFileIterator ----------- */

  /** Use this method to set the starting index of the numeric series.
   * The method is used only when generating file sequences in write mode
   * (see documentation for superclass). The default value is 1. */
  itkSetMacro(StartIndex,unsigned long);
  itkGetMacro(StartIndex,unsigned long);

protected:
  NumericSeriesFileIterator();
  ~NumericSeriesFileIterator();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** The starting index for writing a sequence of files. */
  unsigned long m_StartIndex;

  /** The current index in the series of files. */
  unsigned long m_CurrentIndex;

private:
  NumericSeriesFileIterator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} //namespace ITK

#endif // __itkNumericSeriesFileIterator_h
