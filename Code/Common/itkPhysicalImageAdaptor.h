/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkPhysicalImageAdaptor.h
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
#ifndef __itkPhysicalImageAdaptor_h
#define __itkPhysicalImageAdaptor_h

#include "itkImageAdaptor.h"
#include "itkPhysicalImage.h"

namespace itk
{
  
/**
 * \class PhysicalImageAdaptor
 * \brief Give access to partial aspects of voxels from an Physical Image
 *
 * PhysicalImageAdaptors are templated over the ImageType and over a functor
 * that will specify what part of the pixel can be accessed
 *
 * The basic aspects of this class are the types it defines.  
 *
 * Physical Image adaptors can be used as intermediate classes that allow
 * to send an image to a filter, specifying what part of the
 * image pixels will the filter act on.
 *
 * The TAccessor class should implement the Get and Set methods
 * as static methods. These two will specify how data can be put
 * and get from parts of each pixel. It should define the types
 * ExternalType and InternalType too.
 *
 * \sa Image
 * */

template <class TImage, class TAccessor >
class ITK_EXPORT PhysicalImageAdaptor : public ImageAdaptor<TImage, TAccessor>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef PhysicalImageAdaptor  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageAdaptor<TImage, TAccessor> Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Index typedef support. An index is used to access pixel values.
   */
  enum { ImageDimension = TImage::ImageDimension };
  typedef Index<ImageDimension>  IndexType;

  /** 
   * Size typedef support. A size is used to define region bounds.
   */
  typedef Size<ImageDimension>  SizeType;

  /** 
   * Region typedef support. A region is used to specify a subset of an image.
   */
  typedef ImageRegion<ImageDimension>  RegionType;

  /** 
   * Typedef for associated AffineTransform
   *
   * This is used specifically as the type of the index-to-physical and
   * physical-to-index transforms associated with the origin and spacing
   * for the image, and more generally as any affine transformation of
   * the image.
   */
  typedef AffineTransform<double, ImageDimension> AffineTransformType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(PhysicalImageAdaptor, ImageAdaptor);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  


  /** 
   * Set the spacing (size of a pixel) of the image.
   */
  virtual void SetSpacing( const double values[TImage::ImageDimension] );
  virtual void SetSpacing( const float values[TImage::ImageDimension] );

  /** 
   * Get the spacing (size of a pixel) of the image. The
   * spacing is the geometric distance between image samples.
   * The value returned is a pointer to a double array.
   * \sa SetSpacing()
   */
  virtual const double* GetSpacing() const;
 
  /** 
   * Get the origin of the image. The origin is the geometric
   * coordinates of the image origin.  The value returned is
   * a pointer to a double array.
   * \sa SetOrigin()
   */
  virtual const double * GetOrigin() const;

  /** 
   * Set the origin of the image.
   */
  virtual void SetOrigin( const double values[TImage::ImageDimension] );
  virtual void SetOrigin( const float values[TImage::ImageDimension] );


  /**
   * Set Internal Image
   */
  virtual void SetImage( TImage * );

  
protected:
  PhysicalImageAdaptor();
  virtual ~PhysicalImageAdaptor();
  PhysicalImageAdaptor(const Self&);
  void operator=(const Self&);
  void PrintSelf(std::ostream& os, Indent indent) const;

  
private:
  
  typename TImage::Pointer   m_Image;

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPhysicalImageAdaptor.txx"
#endif
  
  

#endif

