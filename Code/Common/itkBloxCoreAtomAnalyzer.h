/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxCoreAtomAnalyzer.h
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
#ifndef __itkBloxCoreAtomAnalyzer_h
#define __itkBloxCoreAtomAnalyzer_h

#include "vnl/vnl_vector_fixed.h"
#include "vnl/algo/vnl_generalized_eigensystem.h"
#include "itkPoint.h"
#include "itkCovariantVector.h"

#include "itkBloxCoreAtomPixel.h"
#include "itkBloxBoundaryPointItem.h"

namespace itk
{

/**
 * \class BloxCoreAtomAnalyzer
 * \brief
 *
 * \ingroup ImageFeatureExtraction
 * */

template <unsigned int NDimensions>
class BloxCoreAtomAnalyzer
{
public:
  
  /**
   * The type of core atom item we process
   * */
  typedef BloxCoreAtomItem<NDimensions> TCoreAtomItemType;

  /**
   * The type of boundary point item we process
   * */
  typedef BloxBoundaryPointItem<NDimensions> TBPItemType;

  /**
   * The type used to store the position of the BoundaryPointItem
   * */
  typedef Point<double, NDimensions> TPositionType;
  
  /**
   * The type of vector used to store the gradient of the BoundaryPointItem
   * */
  typedef CovariantVector<double, NDimensions> TGradientType;

  /**
   * VNL type used in eigenanalysis
   * */
  typedef vnl_vector_fixed<double, NDimensions> TVectorType;

  /**
   * Analyze all of the core atoms in the pixel
   * Returns TRUE if blox contains core atoms and analysis is valid,
   * other wise returns false
   * */
  bool Analyze(BloxCoreAtomPixel<NDimensions>* bloxPixel);

  BloxCoreAtomAnalyzer();
  ~BloxCoreAtomAnalyzer();

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxCoreAtomAnalyzer.txx"
#endif

#endif
