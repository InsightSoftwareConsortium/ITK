/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArchetypeSeriesFileNames.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkArchetypeSeriesFileNames_h
#define __itkArchetypeSeriesFileNames_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObject.h"
#include "itkObjectFactory.h"
#include <vector>
#include <string>



namespace itk
{
/** \class ArchetypeSeriesFileNames
 * \brief Generate an ordered sequence of filenames.
 *
 * This class generates an ordered sequence of files based on an
 * archetypical filename.  From the archetypical filename, a set of
 * regular expressions is created to group filenames based on numeric
 * substrings.  There can be multiple numeric substrings in the
 * archetype.  When this occurs, ArchetypeSeriesFileNames can not
 * determine which numeric substring refers to the "image number" and
 * which numeric substring refers to the "series" or "study". By
 * default, the ArchetypeSeriesFileNames assumes the rightmost numeric
 * substring refers to the image number, and this is the group of
 * filenames returned by default.  However, the other groupings of
 * filenames can also be queried by passing in a group number to the
 * GetFileNames() method. Groups are numbered by the numeric
 * substrings from right to left in the archetype.
 *
 * For example, if a directory contains the files
 *
 *         foo_5_1.png 
 *         foo_5_2.png 
 *         foo_5_3.png 
 *         foo_6_1.png 
 *         foo_6_2.png 
 *         foo_6_3.png 
 *
 * and specifying an archetypical file foo_5_1.png, the filename list
 * will contain
 *
 *         foo_5_1.png 
 *         foo_5_2.png
 *         foo_5_3.png
 *
 *  \ingroup IOFilters
 *
 */


class ITK_EXPORT ArchetypeSeriesFileNames : public Object
{
public:
  /** Standard class typedefs. */
  typedef ArchetypeSeriesFileNames    Self;
  typedef Object                  Superclass;
  typedef SmartPointer<Self>      Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ArchetypeSeriesFileNames, Object);

  /* -------- Define the API for ArchetypeSeriesFileNames ----------- */

  // The archetypical filename from which to generate the regular
  // expressions
  void SetArchetype(const std::string &archetype);
  itkGetStringMacro(Archetype);

  typedef  size_t VectorSizeType;

  /** Get the number of groupings that match the Archetype */
  VectorSizeType GetNumberOfGroupings();

  /** Returns a vector containing the series' file names. The file
    * names are ordered by Index. Defaults to returning the filenames
    * to the rightmost grouping. */
  const std::vector<std::string> &GetFileNames ( VectorSizeType group = 0);


protected:
  ArchetypeSeriesFileNames();
  ~ArchetypeSeriesFileNames() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Method that actually does the archetype matching/grouping */
  void Scan();
  
private:
  ArchetypeSeriesFileNames(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  /** A string for formatting the names of files in the series. */
  std::string m_Archetype;

  std::vector<std::vector<std::string> > m_Groupings;
  std::vector<std::string> m_FileNames;   // ivar for returning by reference

  TimeStamp  m_ArchetypeMTime;
  TimeStamp  m_ScanTime;
  
};

} //namespace ITK

#endif // __itkArchetypeSeriesFileNames_h
