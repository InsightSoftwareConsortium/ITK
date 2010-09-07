/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelObjectLine.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelObjectLine_h
#define __itkLabelObjectLine_h

#include "itkIndex.h"
#include "itkIndent.h"

namespace itk
{
/** \class LabelObjectLine
 * LabelObjectLine is the line object used in the LabelObject class
 * to store the line which are part of the object.
 * A line is formed of and index and a length in the dimension 0.
 * It is used in a run-length encoding
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \ingroup LabeledImageObject
 */
template< unsigned int VImageDimension >
class LabelObjectLine
{
public:
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);

  typedef Index< VImageDimension > IndexType;
  typedef unsigned long            LengthType;

  LabelObjectLine() {}
  virtual ~LabelObjectLine() {}
  LabelObjectLine(const IndexType & idx, const LengthType & length);

  /**
   * Set/Get Index
   */
  void SetIndex(const IndexType & idx);

  const IndexType & GetIndex() const;

  /**
   * SetGet Length
   */
  void SetLength(const LengthType length);

  const LengthType & GetLength() const;

  /**
   *  Check for index
   */
  bool HasIndex(const IndexType idx) const;

  bool IsNextIndex(const IndexType & idx) const;

  /** Cause the object to print itself out. */
  void Print(std::ostream & os, Indent indent = 0) const;

protected:
  /** Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes.  */
  virtual void PrintSelf(std::ostream & os, Indent indent) const;

  virtual void PrintHeader(std::ostream & os, Indent indent) const;

  virtual void PrintTrailer(std::ostream & os, Indent indent) const;

private:
  IndexType  m_Index;
  LengthType m_Length;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelObjectLine.txx"
#endif

#endif
