/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadGrav.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 * This load is integrated over a whole element. The load vector is returned in a 
 * Fg member function defined in a derived class. The Fg function accepts a vector 
 * specifying a point in global coordinate system and returns a load vector
 * defined at the point. Derived LoadClasses must define this function.
 */
class LoadGrav : public LoadElement
{
FEM_CLASS_SP(LoadGrav,LoadElement)
public:

  virtual vnl_vector<Float> Fg(vnl_vector<Float>) = 0;

};




/**
 * \class LoadGravConst
 * \brief Constant gravity load class.
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
  virtual void Write( std::ostream& f, int ofid ) const;

};

FEM_CLASS_INIT(LoadGravConst)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadGrav_h
