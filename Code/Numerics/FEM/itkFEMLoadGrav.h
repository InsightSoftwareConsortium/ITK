/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadGrav.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMLoadGrav_h
#define __itkFEMLoadGrav_h

#include "itkFEMLoadElementBase.h"
#include "vnl/vnl_vector.h"

namespace itk {
namespace fem {




/**
 * \class LoadGrav
 * \brief Abstract gravity load class.
 *
 * This load is integrated over a whole element. The load vector is returned in a 
 * Fg member function defined in a derived class. The Fg function accepts a vector 
 * specifying a point in global coordinate system and returns a load vector
 * defined at the point. Derived LoadClasses must define this function.
 */
class LoadGrav : public LoadElement
{
FEM_ABSTRACT_CLASS(LoadGrav,LoadElement)
public:

  virtual vnl_vector<Float> Fg(vnl_vector<Float>) = 0;

};




/**
 * \class LoadGravConst
 * \brief Constant gravity load class.
 *
 * This is a special case of LoadGrav. The load vector is the same on
 * every point in space.
 */
class LoadGravConst : public LoadGrav
{
FEM_CLASS(LoadGravConst,LoadGrav)
public:
  vnl_vector<Float> Fg_value;
  virtual vnl_vector<Float> Fg(vnl_vector<Float>) {
    return Fg_value;
  };

  /**
   * Read an object from input stream.
   */
  virtual void Read( std::istream& f, void* info );

  /**
   * Write an object to the output stream
   */
  virtual void Write( std::ostream& f ) const;

};

FEM_CLASS_INIT(LoadGravConst)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadGrav_h
