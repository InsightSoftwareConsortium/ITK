/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstantBoundaryCondition.h
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
#ifndef __itkConstantBoundaryCondition_h
#define __itkConstantBoundaryCondition_h
#include "itkNeighborhood.h"
#include "itkNumericTraits.h"
#include "itkPixelTraits.h"
#include "itkImageBoundaryCondition.h"

namespace itk
{

/** \class ConstantBoundaryCondition
 * \brief This boundary condition returns a constant value for out-of-bounds
 * image pixels.
 * 
 * For example, invoking this function object with a constant value of zero
 * (the default) on each out-of-bounds element of a 7x5 iterator that masks a
 * region at an image corner 
 * (iterator is centered on the 2): 
 *
 *               * * * * * * * 
 *               * * * * * * *
 *               * * 1 2 3 4 5  (where * denotes pixels that lie 
 *               * * 3 3 5 5 6          outside of the image boundary)
 *               * * 4 4 6 7 8
 *
 * would produce the following neighborhood of values:
 *
 *               0 0 0 0 0 0 0
 *               0 0 0 0 0 0 0
 *               0 0 1 2 3 4 5
 *               0 0 3 3 5 5 6
 *               0 0 4 4 6 7 8
 *
 *
 * 
 * \sa ImageBoundaryCondition
 */
template<class TImage,
         class TNeighborhoodType = Neighborhood<ITK_TYPENAME TImage::PixelType*,
                                                TImage::ImageDimension > >
class ConstantBoundaryCondition
  : public ImageBoundaryCondition<TImage, TNeighborhoodType>
{
public:
  /**
   * Self & superclass typedefs
   */ 
  typedef ConstantBoundaryCondition Self;
  typedef ImageBoundaryCondition<TImage, TNeighborhoodType> Superclass;

  /**
   * Extract information from the image type
   */
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::PixelPointerType PixelPointerType;
  enum { ImageDimension = Superclass::ImageDimension };
  typedef typename ScalarTraits<PixelType>::ValueType PixelScalarValueType;
  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::OffsetType OffsetType;
  
  /**
   * Default constructor.
   */
  ConstantBoundaryCondition()
  { m_Constant = NumericTraits<PixelScalarValueType>::Zero; }

  /**
   * Computes and returns appropriate out-of-bounds values from
   * neighborhood iterator data.
   */
  virtual PixelType operator()(const OffsetType& point_index,
                               const OffsetType& boundary_offset,
                               const TNeighborhoodType *data) const
  { return m_Constant; }

  void SetConstant(const PixelType &c)
  {  m_Constant = c; }

  const PixelType &GetConstant() const
  {  return m_Constant;  }
  
private:
  PixelType m_Constant;
};

} // end namespace itk

#endif
