/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularExpressionSeriesFileNames.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegularExpressionSeriesFileNames_h
#define __itkRegularExpressionSeriesFileNames_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkExceptionObject.h"
#include <vector>

namespace itk
{

/** \class RegularExpressionSeriesFileNames
 * \brief Generate an ordered sequence of filenames that match a
 * regular expression.
 *
 * This class generates an ordered sequence of files whose filenames
 * match a regular expression. The file names are sorted using a sub
 * expression match selected by SubMatch. Regular expressions are a
 * powerful,  compact mechanism for parsing strings.
 * Expressions consist of the following metacharacters:
 *
 *  ^        Matches at beginning of a line
 *
 *  $        Matches at end of a line
 *
 * .         Matches any single character
 *
 * [ ]       Matches any character(s) inside the brackets
 *
 * [^ ]      Matches any character(s) not inside the brackets
 *
 *  -        Matches any character in range on either side of a dash
 *
 *  *        Matches preceding pattern zero or more times
 *
 *  +        Matches preceding pattern one or more times
 *
 *  ?        Matches preceding pattern zero or once only
 *
 * ()        Saves a matched expression and uses it in a  later match
 *
 * Note that more than one of these metacharacters can be  used
 * in  a  single  regular expression in order to create complex
 * search patterns. For example, the pattern [^ab1-9]  says  to
 * match  any  character  sequence that does not begin with the
 * characters "ab"  followed  by  numbers  in  the  series  one
 * through nine.
 * 
 * \ingroup IOFilters
 *
 */
class ITK_EXPORT RegularExpressionSeriesFileNames : public Object
{
public:
  /** Standard class typedefs. */
  typedef RegularExpressionSeriesFileNames    Self;
  typedef Object                  Superclass;
  typedef SmartPointer<Self>      Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegularExpressionSeriesFileNames, Object);

  /* -------- Define the API for RegularExpressionSeriesFileNames ---------- */
  /** The directory containing the files. */
  itkSetStringMacro(Directory);
  itkGetStringMacro(Directory);

  /** The RegularExpression. Refer to the description for valid expressions */
  itkSetStringMacro(RegularExpression);
  itkGetStringMacro(RegularExpression);

  /** The index of the submatch that will be used to sort the
   * matches. */
  itkSetMacro(SubMatch, unsigned int);
  itkGetMacro(SubMatch, unsigned int);

  /** NumericSortOn changes the sort of the submatch field to a
   * numeric sort. NumericSortOff is the default, and sorts the
   * submatch alphabetically. */
  itkSetMacro(NumericSort,bool);
  itkGetMacro(NumericSort,bool);
  itkBooleanMacro(NumericSort);

  /** Return the SubMatch of the specified index. */
  std::string GetSubMatch (unsigned int);

  /** Returns a vector containing the series' file names. The file
    * names are sorted by the sub expression selected by the SubMatch id. */
  const std::vector<std::string> &GetFileNames ();

protected:
  RegularExpressionSeriesFileNames() :
    m_Directory("."),
    m_SubMatch(1),
    m_NumericSort(false),
    m_RegularExpression(".*\\.([0-9]+)")
    {};
  ~RegularExpressionSeriesFileNames() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  RegularExpressionSeriesFileNames(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  std::string m_Directory;
  unsigned int m_SubMatch;
  bool m_NumericSort;
  std::string m_RegularExpression;

  std::vector<std::string>  m_FileNames;
};

} //namespace ITK

#endif // __itkRegularExpressionSeriesFileNames_h
