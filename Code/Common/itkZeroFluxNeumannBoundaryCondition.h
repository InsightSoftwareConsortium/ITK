/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroFluxNeumannBoundaryCondition.h
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
#ifndef __itkZeroFluxNeumannBoundaryCondition_h
#define __itkZeroFluxNeumannBoundaryCondition_h
#include "itkNeighborhood.h"

namespace itk
{

/** \class ZeroFluxNeumannBoundaryCondition
 * \brief
 * A function object that determines a neighborhood of values at an
 * image boundary according to a Neumann boundary condition where first,
 * upwind derivatives on the boundary are zero.  This is a useful condition 
 * in solving some classes of differential equations. 
 *
 * For example, invoking this function object on a 7x5 iterator that masks
 * a region at an image corner (iterator is centered on the 2):
 *
 *               * * * * * * * 
 *               * * * * * * *
 *               * * 1 2 3 4 5  (where * denotes pixels that lie 
 *               * * 3 3 5 5 6          outside of the image boundary)
 *               * * 4 4 6 7 8
 *
 * returns the following neighborhood of values:
 *
 *               1 1 1 2 3 4 5
 *               1 1 1 2 3 4 5
 *               1 1 1 2 3 4 5
 *               3 3 3 3 5 5 6   (note the corner values)
 *               4 4 4 4 6 7 8
 *
 *
 * The input to this function object is a neighborhood iterator.  This boundary
 * condition object is designed to be given as a template argument to a
 * SmartNeighborhoodIterator or any of the SmartNeighborhoodIterator
 * subclasses.  It can also be used to override a default boundary condition
 * type in a SmartNeighborhoodIterator or any of the SmartNeighborhoodIterator
 * subclasses.
 * 
 * \ingroup DataRepresentation
 * \ingroup ImageObjects
 *
 */

template<class TImage>
class  ZeroFluxNeumannBoundaryCondition
  : public ImageBoundaryCondition<TImage>
{
public:
  /**
   * Self & superclass typedefs
   */ 
  typedef ZeroFluxNeumannBoundaryCondition Self;
  typedef ImageBoundaryCondition<TImage> Superclass;

  /**
   * Extract information from the image type
   */
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::PixelPointerType PixelPointerType;
  enum { ImageDimension = Superclass::ImageDimension };
  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::OffsetType OffsetType;

  typedef typename Superclass::NeighborhoodType NeighborhoodType;

  /**
   * Default constructor.
   */
  ZeroFluxNeumannBoundaryCondition() {}

  /**
   * Computes and returns a neighborhood of appropriate values from
   * neighborhood iterator data..
   */
  virtual PixelType operator()(const OffsetType& point_index,
                               const OffsetType& boundary_offset,
                               const NeighborhoodType *data) const; 
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkZeroFluxNeumannBoundaryCondition.txx"
#endif

#endif
