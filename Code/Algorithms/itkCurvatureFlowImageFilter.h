/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvatureFlowImageFilter.h
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
#ifndef _itkCurvatureFlowImageFilter_h
#define _itkCurvatureFlowImageFilter_h

#include "itkLevelSetImageFilter.h"

namespace itk
{

/** \class CurvatureFlowImageFilter
  * \brief Denoise an image using curvature driven flow.
  *
  * CurvatureFlowImageFilter implements a curvature driven image denoising algorithm.
  * Iso-brightness contours in the input image are viewed as a level set.
  * The level set is then evolved using a curvature-based speed function.
  *
  * The advantage of this approach is that sharp boundaries are preserved
  * with smoothing occuring only within a region.
  *
  * Note that unlike level set segmenetation algorithms,
  * the image to be denoised is already the level set and can be set
  * directly as the input using the SetInput() method.
  *
  * Narrowbanding is not supported in this class.
  * 
  * Reference:
  * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
  * Cambridge Press, Chapter 16, Second edition, 1999.
  *
  * Possible improvements:
  * - At each iteration, the algorithm is highly parallelizable.
  * Future implementation should take advantage of this.
  *
  * \sa LevelSetImageFilter
  *
  * \ingroup ImageEnhancement 
  * \ingroup LevelSetSegmentation 
  *
  */
template <class TLevelSet> 
class ITK_EXPORT CurvatureFlowImageFilter : 
  public LevelSetImageFilter<TLevelSet>
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef CurvatureFlowImageFilter Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef LevelSetImageFilter<TLevelSet> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(CurvatureFlowImageFilter, LevelSetImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Typedef support for level set related types.
   */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::PixelType  PixelType;

  /**
   * Index typedef support
   */
  typedef Index<LevelSetType::SetDimension> IndexType;

  /**
   * Set the debugging mode
   */
  itkSetMacro( DebugOn, bool );

protected:
  CurvatureFlowImageFilter();
  ~CurvatureFlowImageFilter(){};
  CurvatureFlowImageFilter(const Self&){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void Initialize();
  void GenerateData();

private:
  bool m_DebugOn;

};


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCurvatureFlowImageFilter.txx"
#endif

#endif
