/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBorder.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkRGBorder_h
#define _itkRGBorder_h

#include "itkObject.h"
namespace itk
{

/** \class RGBorder
 * \brief Base class for RGBorder object
 * 
 * itkRGBorder is the base class for the RGBorder objects. It provides
 * the basic function definitons that are inherent to a RGBorder objects.
 * This object stores information relevant to the borders that are used
 * in the region growing class. This class allows access to the parameter
 * that defines the length of the border associated with this object.
 * In order to use this object in an application, the object must be
 * created within the application and then used to initialize/store/use
 * the various parameters accessible through the public methods. For usage
 * also see itkRegionGrowing class.
 *
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT RGBorder : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RGBorder   Self;

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
  itkTypeMacro(RGBorder,Object);

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
   * Define a virtual RGBorder function. This function allows access to
   * specific instantiations of other border representations. This is the
   * function should be overloaded in any derived classes for the user to
   * access the various methods supported by the method.
   */
  virtual void ApplyRGBorder(){};

  /**
   * Constructor
   */
  RGBorder();

  /**
   * Destructor
   */
  ~RGBorder();

  /**
   * Copy constructor
   */
  RGBorder(const Self&) {}

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

}; // class RGBorder


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRGBorder.txx"
#endif



#endif
