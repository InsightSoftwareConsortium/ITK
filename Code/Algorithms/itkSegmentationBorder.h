/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationBorder.h
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
#ifndef _itkSegmentationBorder_h
#define _itkSegmentationBorder_h

#include "itkObject.h"
namespace itk
{

/** \class SegmentationBorder
 * \brief Base class for SegmentationBorder object
 * 
 * itkSegmentationBorder is the base class for the SegmentationBorder objects. It provides
 * the basic function definitons that are inherent to a SegmentationBorder objects.
 * This object stores information relevant to the borders that are used
 * in the region growing class. This class allows access to the parameter
 * that defines the length of the border associated with this object.
 * In order to use this object in an application, the object must be
 * created within the application and then used to initialize/store/use
 * the various parameters accessible through the public methods. For usage
 * also see itkRegionGrowImageFiltering class.
 *
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT SegmentationBorder : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef SegmentationBorder   Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef Object Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(SegmentationBorder,Object);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Type definition for the input image.
   */
  typedef typename TInputImage::Pointer   InputImageType;

  /**
   * Type definition for the input image pixel type.
   */
  typedef typename TInputImage::PixelType InputImagePixelType;

  /**
   * Set the length of a border object.
   */
  itkSetMacro(BorderLength, unsigned int);

  /**
   * Get the length of a border object.
   */
  itkGetMacro(BorderLength, unsigned int);

  /**
   * Define a virtual SegmentationBorder function. This function allows access to
   * specific instantiations of other border representations. This is the
   * function should be overloaded in any derived classes for the user to
   * access the various methods supported by the method.
   */
  virtual void ApplySegmentationBorder(){};

  /**
   * Constructor
   */
  SegmentationBorder();

  /**
   * Destructor
   */
  ~SegmentationBorder();

  /**
   * Copy constructor
   */
  SegmentationBorder(const Self&) {}

  /**
   * Assignment operator
   */
  void operator=(const Self&) {}

protected:
  /**
   * Print self identity
   */      
  void PrintSelf(std::ostream& os, Indent indent);

private:
  unsigned int m_BorderLength;

}; // class SegmentationBorder


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSegmentationBorder.txx"
#endif



#endif
