/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPathToPathFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkPathToPathFilter_h
#define _itkPathToPathFilter_h

#include "itkPathSource.h"
#include "itkPath.h"

namespace itk
{
  
/** \class PathToPathFilter
 * \brief Base class for filters that take a path as input and produce a path as output.
 *
 * PathToPathFilter is the base class for all process objects that output
 * path data and require path data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 *
 * \ingroup PathFilters
 */

template <class TInputPath, class TOutputPath>
class ITK_EXPORT PathToPathFilter : public PathSource<TOutputPath>
{
public:
  /** Standard class typedefs. */
  typedef PathToPathFilter  Self;
  typedef PathSource<TOutputPath>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(PathToPathFilter,PathSource);

  /** Some convenient typedefs. */
  typedef TInputPath InputPathType;
  typedef typename InputPathType::Pointer InputPathPointer;
  typedef typename InputPathType::ConstPointer InputPathConstPointer;
  
  /** Set/Get the path input of this process object.  */
  virtual void SetInput( const InputPathType *path);
  virtual void SetInput( unsigned int, const TInputPath * path);
  const InputPathType * GetInput(void);
  const InputPathType * GetInput(unsigned int idx);

protected:
  PathToPathFilter();
  ~PathToPathFilter() {}
  
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** What is the input requested region that is required to produce the output
   * requested region?  Up till and including now, the base assumption is that
   * the largest possible region will be requested of the input.  If this method
   * is overridden, the new method should call its superclass' implementation as
   * its first step.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();


private:
  PathToPathFilter(const Self&); //purposely not implemented
  void operator=(const Self&);   //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPathToPathFilter.txx"
#endif

#endif
