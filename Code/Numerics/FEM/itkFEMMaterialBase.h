/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMMaterialBase.h
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

#ifndef __itkFEMMaterialBase_h
#define __itkFEMMaterialBase_h

#include "itkFEMLightObject.h"
#include "itkFEMPArray.h"
#include <iostream>

namespace itk {
namespace fem {




/**
 * \brief Base class for storing all the implicit material and other properties
          required to fully define the element class.
 *
 * When specifying materials for particular element, you should use MaterialStandard
 * class or derive your own class (using Material or MaterialStandard as a base class)
 * if your Element requires special properties or constants
 */
class Material : public FEMLightObject
{
FEM_CLASS_SP(Material,FEMLightObject)
public:
  /**
   * Array class that holds special pointers to objects of all Material classes
   */
  typedef FEMPArray<Self> ArrayType;

  /**
   * Material base class doesn't define anything.
   * Everything usefull is stored in derived clases. This class
   * is here just to group all material classes together and access
   * them via this base class.
   */

};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMMaterialBase_h
