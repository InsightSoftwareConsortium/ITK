/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEM.cxx
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

#include "itkFEM.h"

namespace itk {
namespace fem {




/**
 * \brief Function that uses all OFID_... global variables.
 *
 * Defining this function eliminates compile warnings about OFID_...
 * being defined but never used on some systems (Sun). Other than
 * that, this function does nothing else.
 * 
 * FIXME: There is probably a better way to get rid of the compile warnings.
 */
void OFIDs(void)
{
  OFID_NodeXY;
  OFID_NodeXYZ;
  OFID_NodeXYrotZ;
  OFID_Node2DIsotropic;

  OFID_MaterialStandard;

  OFID_Bar2D;
  OFID_Beam2D;
  OFID_TriC02D;
  OFID_QuadC02D;
  OFID_LoadElement;
  OFID_LoadGravConst;
  OFID_C1IsoCurve2D;
  OFID_HexahedronC03D;
  OFID_TetrahedronC03D;

  OFID_LoadNode;
  OFID_LoadBCMFC;
  OFID_LoadPoint;
  OFID_LoadEdge;
}




}} // end namespace itk::fem
