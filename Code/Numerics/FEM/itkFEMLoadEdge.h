/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadEdge.h
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

#ifndef __itkFEMLoadEdge_h
#define __itkFEMLoadEdge_h

#include "itkFEMLoadElementBase.h"
#include "vnl/vnl_matrix.h"

namespace itk {
namespace fem {




/**
 * \class LoadEdge
 * \brief A generic load that can be applied to an edge of the element.
 *
 * Can also be used to apply natural (Neumann) boundary condition on the
 * edge of the element. In this case m_Edge defines the edge or surfance
 * of the element on which the BC exists and matrix m_Force holds the actual
 * prescribed values of the BC.
 */
class LoadEdge : public LoadElement
{
FEM_CLASS(LoadEdge)
public:
  /** 
   * Read a Load object from input stream.
   * We need arrays of elements and nodes to do that.
   */
  virtual void Read( std::istream& f, void* info );

  /**
   * Write a Load object to the output stream
   */
  virtual void Write( std::ostream& f, int ofid ) const;

public:
  /**
   * Local number of the edge (face) of the element on which the load acts.
   * Check the corresponding element class for more info on edge numbering.
   */
  int m_Edge;

  /**
   * An edge force matrix. This matrix specifies nodal forces on all
   * nodes within the edge or face on which the load acts. Each nodal
   * force is decomposed into several components (check the documentation 
   * inside the element class). In case of 2D elements this components
   * are normal (1st component) and tangential (2nd component) force
   * acting on the edge of the element. A positive normal load acts in
   * a direction INTO the element. A positive tangential load acts in
   * an ANTICLOCKWISE direction with respect to the loaded elemenet.
   * Each nodal force is stored in a column of the matrix. The number
   * of columns in the Force matrix must correspond to the number of
   * nodes that define the edge (face...). The force is interpolated
   * over the entire edge (face) by using the shape functions of the
   * element. Again check the documentation of the element class to which
   * the force is applied.
   */
  vnl_matrix<Float> m_Force;

};

FEM_CLASS_INIT(LoadEdge)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadEdge_h
