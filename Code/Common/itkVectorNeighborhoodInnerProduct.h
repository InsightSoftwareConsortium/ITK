/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorNeighborhoodInnerProduct.h
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
#ifndef __itkVectorNeighborhoodInnerProduct_h
#define __itkVectorNeighborhoodInnerProduct_h

#include "itkNeighborhoodIterator.h"
#include "itkSmartNeighborhoodIterator.h"
#include "itkVector.h"
#include "itkNeighborhood.h"
#include "itkConstSliceIterator.h"
#include "itkImageBoundaryCondition.h"

namespace itk {
  
/**
 * \class VectorNeighborhoodInnerProduct
 *
 * This is an explicit implementation of what should really be a partial
 * template specialization of NeighborhoodInnerProduct for itkVector.
 * 
 * This class defines the inner product operation between an itk::Neighborhood
 * and and itk::NeighborhoodOperator.  The operator() method is overloaded
 * to support various types of neighborhoods as well as inner products with
 * slices of neighborhoods.
 *
 * \ingroup Operators
 * 
 *\todo DOCUMENT!
 */
template<class TImage>
class VectorNeighborhoodInnerProduct
{
public:
  typedef typename TImage::PixelType PixelType; // Pixel type is a now a vector
  typedef typename PixelType::ValueType ScalarValueType;
  enum {VectorDimension = PixelType::VectorDimension };
  enum {ImageDimension = TImage::ImageDimension };

  /**
   *
   */
  PixelType operator()(const ConstNeighborhoodIterator<TImage> &it,
                       const Neighborhood<ScalarValueType, ImageDimension>
                       &op) const;
  /**
   *
   */
  PixelType operator()(const std::slice &s,
                       const ConstNeighborhoodIterator<TImage> &it,
                       const Neighborhood<ScalarValueType, ImageDimension>
                       &op) const;
};

template<class TImage>
class SmartVectorNeighborhoodInnerProduct
{
public:
  typedef typename TImage::PixelType PixelType;
  typedef typename PixelType::ValueType ScalarValueType;
  enum {VectorDimension = PixelType::VectorDimension };
  enum {ImageDimension = TImage::ImageDimension};

  /**
   *
   */
  PixelType operator()(const std::slice &s,
                      /*                      const ImageBoundaryCondition<TImage> *,*/
                       const ConstSmartNeighborhoodIterator<TImage> &it,
                       const Neighborhood<ScalarValueType, ImageDimension>
                       &op) const;
  /**
   * 
   */
  PixelType operator()(const ConstSmartNeighborhoodIterator<TImage> &it,
                       const Neighborhood<ScalarValueType, ImageDimension>
                       &op) const
    {
      return this->operator()(std::slice(0, it.Size(), 1), it, op);
    }
};
  
  
  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorNeighborhoodInnerProduct.txx"
#endif

#endif
