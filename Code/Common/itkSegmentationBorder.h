/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationBorder.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSegmentationBorder_h
#define _itkSegmentationBorder_h

#include "itkObject.h"
#include "itkObjectFactory.h"
namespace itk
{

/** \class SegmentationBorder
 * \brief Base class for SegmentationBorder object
 * 
 * itkSegmentationBorder is the base class for the SegmentationBorder
 * objects. It provides the basic function definitons that are inherent to a
 * SegmentationBorder objects.  This object stores information relevant to
 * the borders that are used in the region growing class. This class allows
 * access to the parameter that defines the length of the border associated
 * with this object.  In order to use this object in an application, the
 * object must be created within the application and then used to
 * initialize/store/use the various parameters accessible through the public
 * methods. For usage also see itkRegionGrowImageFiltering class.
 * This object can be used where a border of a region needs to be defined.
 * Chlid classes can be derived from this class for tailoring them towards 
 * specific algorithmic needs.
 * 
 * \ingroup RegionGrowingSegmentation 
 */
class ITKCommon_EXPORT SegmentationBorder : public Object
{
public:
  /** Standard class typedefs. */
  typedef SegmentationBorder   Self;
  typedef Object Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SegmentationBorder,Object);

  /** Type definition for the input image. */
//  typedef typename TInputImage::Pointer   InputImageType;

  /** Type definition for the input image pixel type. */
//  typedef typename TInputImage::PixelType InputImagePixelType;

  /** Set the length of a border object. */
  itkSetMacro(BorderLength, unsigned int);

  /** Get the length of a border object. */
  itkGetMacro(BorderLength, unsigned int);

  /** Define a virtual SegmentationBorder function. This function allows
   * access to specific instantiations of other border representations. This
   * is the function should be overloaded in any derived classes for the user
   * to access the various methods supported by the method. */
  virtual void ApplySegmentationBorder(){};

protected:
  SegmentationBorder();
  ~SegmentationBorder();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  SegmentationBorder(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  unsigned int m_BorderLength;

}; // class SegmentationBorder


} // namespace itk



#endif
