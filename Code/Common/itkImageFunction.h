/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkImageFunction_h
#define _itkImageFunction_h

#include "itkObject.h"
#include "itkIndex.h"
#include "vnl/vnl_math.h"

namespace itk
{

/** 
 * \class ImageFunction
 * \brief Evaluates a function of an image at specified index.
 *
 * ImageFunction is a baseclass for all objects that evaluates
 * a function of an image at index. This class is templated over 
 * the input image type and the type of the function output.
 *
 * The input image is set via method SetInputImage().
 * The Evaluate() method evaluates the function at an index or
 * at a non-integer position.
 *
 */
template <
class TInputImage, 
class TOutput 
>
class ITK_EXPORT ImageFunction : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * InputImageType typedef support.
   */
  typedef TInputImage InputImageType;

  /**
   * OutputType typedef support.
   */
  typedef TOutput OutputType;

  /**
   * Dimension underlying input image.
   */
  enum { ImageDimension = InputImageType::ImageDimension };

  /**
   * Index typedef support.
   */
  typedef Index<ImageDimension> IndexType;

  /**
   * InputPixel typedef support
   */
  typedef typename InputImageType::PixelType PixelType;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self); 
  
  /** 
   * Set the input image.
   */
  virtual void SetInputImage( InputImageType * ptr )
    { m_Image = ptr; }

  /**
   * Get the input image.
   */
  typename InputImageType::Pointer GetInputImage()
    { return m_Image; }

  /**
   * Evaluate the function at specified index
   */
  virtual TOutput Evaluate( const IndexType& index ) = 0;

  /** 
   * Evaluate the function at a non-integer position
   */
  virtual TOutput Evaluate( double coord[] )
    {
      IndexType index;
      for( int j = 0; j < ImageDimension; j++ )
        {
          index[j] = vnl_math_rnd( coord[j] );
        }
      return ( this->Evaluate( index ) );
    };

protected:

  ImageFunction() 
    { m_Image = NULL; }

  ImageFunction( const Self& ){};

  ~ImageFunction(){};

  void operator=(const Self&) {};

  void PrintSelf(std::ostream& os, Indent indent)
    { 
      this->Superclass::PrintSelf( os, indent );
      os << indent << "ImageFunction" << std::endl;
    }

  // made protected so subclass can access
  typename InputImageType::Pointer        m_Image;

};

} // namespace itk


#endif
