/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomVariateGeneratorBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRandomVariateGeneratorBase_h
#define __itkRandomVariateGeneratorBase_h

#include "itkObject.h"

namespace itk
{
namespace Statistics
{
/** \class RandomVariateGeneratorBase
 * \brief this class defines common interfaces for random variate generators
 *
 * \ingroup Statistics
 */
class ITK_EXPORT RandomVariateGeneratorBase:public Object
{
public:
  /** Standard class typedefs. */
  typedef RandomVariateGeneratorBase Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RandomVariateGeneratorBase, Object);

  /** get a variate using FastNorm function */
  virtual double GetVariate() = 0;

protected:
  RandomVariateGeneratorBase() {}
  virtual ~RandomVariateGeneratorBase() {}
private:
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#endif
