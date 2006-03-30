/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolylineMaskImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPolylineMaskImageFilter_h
#define _itkPolylineMaskImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
  
/** \class PolylineMaskImageFilter
 * \brief Implements image masking operation constrained by a polyline on a plane
 * perpendicular to certain viewing direction
 *
 * This class is parameterized over the types of the input image, polyline, vector
 * and output image.  
 * 
 * \ingroup ImageToImageFilter
 */
  template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
class ITK_EXPORT PolylineMaskImageFilter:public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef PolylineMaskImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( PolylineMaskImageFilter, ImageToImageFilter );

  /** Number of dimensions. */
  itkStaticConstMacro(NDimensions, unsigned int,
                      TInputImage::ImageDimension);

  itkStaticConstMacro(InputDimension, unsigned int, 3);

  /** Some convenient typedefs for input image */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::ConstPointer InputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType; 
  typedef typename InputImageType::PixelType InputImagePixelType; 
  typedef Point<double,3>            PointType;
  typedef Point<double,2> ProjPlanePointType;      

  /** Standard matrix type for this class. */
  typedef Matrix<double, itkGetStaticConstMacro(InputDimension), itkGetStaticConstMacro(InputDimension)> MatrixType;

  /** typedef for the vector type */
  typedef TVector    VectorType;

  /* typedef for the polyline type */
  typedef TPolyline    PolylineType;
  
  /* typedef for the output image */
  typedef TOutputImage OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType OutputImagePixelType;

  /** Read in image and polyline inputs */
  void SetInput1( const InputImageType * image);
  void SetInput2( const PolylineType * polyline);

  /** Set/Get view vector */
  itkSetMacro(ViewVector,VectorType);
  itkGetConstMacro(ViewVector,VectorType);  

  /** Set/Get in up vector */
  itkSetMacro(UpVector,VectorType);
  itkGetConstMacro(UpVector,VectorType);  

  /** Set/Get camera center point */
  itkSetMacro(CameraCenterPoint,PointType);
  itkGetConstMacro(CameraCenterPoint,PointType);  

  /** Set/Get focal distance */
  itkSetMacro(FocalDistance,double);
  itkGetConstMacro(FocalDistance,double);  

  /** Set/Get focal point center point */
  itkSetMacro(FocalPoint,ProjPlanePointType);
  itkGetConstMacro(FocalPoint,ProjPlanePointType);  
    
  /* Roatation matrix generation matrix */
  void GenerateRotationMatrix();

  /* 3D Point transforming and projecting function */
  ProjPlanePointType TransformProjectPoint(PointType inputPoint);
  /* Generate Data */
  void GenerateData(void);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(VectorHasNumericTraitsCheck,
    (Concept::HasNumericTraits<typename VectorType::ValueType>));
  /** End concept checking */
#endif

protected:
  PolylineMaskImageFilter();
  virtual ~PolylineMaskImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  PolylineMaskImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /* viewing direction and up vector */
  VectorType m_ViewVector;
  VectorType m_UpVector;

  /** Matrix representation of the rotation. */
  MatrixType          m_RotationMatrix;   

  /**  Camera Center point */
  PointType          m_CameraCenterPoint;

  /** Camera focal points on the projection plane */
  ProjPlanePointType m_FocalPoint;
  
  /* focal distance of the camera */
  double m_FocalDistance;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolylineMaskImageFilter.txx"
#endif

#endif
