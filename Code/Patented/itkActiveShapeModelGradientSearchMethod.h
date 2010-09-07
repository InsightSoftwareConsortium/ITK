/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkActiveShapeModelGradientSearchMethod.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkActiveShapeModelGradientSearchMethod_h
#define __itkActiveShapeModelGradientSearchMethod_h

#include <time.h>
#include <math.h>

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_math.h"

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkNumericTraits.h"

#include "itkImageRegionConstIterator.h"

#include "itkGradientMagnitudeImageFilter.h"

#include "itkVector.h"
#include "itkListSample.h"
#include "itkExceptionObject.h"

#include "itkConceptChecking.h"

#include <vector>
#include <list>

namespace itk
{
/** \class ActiveShapeModelGradientSearchMethod
 * \brief Base class for ActiveShapeModelGradientSearchMethod object
 *
 * itkActiveShapeModelGradientSearchMethod performs a search along normal direction for
 * the strongest edge on each landmark position.
 *
 * \par REFERENCES
 * \par
 * [1] Cootes T., Taylor C., et. al, Active Shape Models - Their training and application,
 *     Comput. Vis. Image Understanding, vol. 61, pp. 38-59, 1995.
 *
 *
 *
 *\ingroup Operators */

template< class TImage >
class ITK_EXPORT ActiveShapeModelGradientSearchMethod:public Object
{
public:

  /** Standard class typedefs. */
  typedef ActiveShapeModelGradientSearchMethod< TImage > Self;
  typedef Object                                         Superclass;
  typedef SmartPointer< Self >                           Pointer;
  typedef SmartPointer< const Self >                     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ActiveShapeModelGradientSearchMethod, Object);

  /** Image typedef support   */
  typedef TImage             InputImageType;
  typedef Image< double, 2 > Image2DType;

  /** Type for the region of the input image. */
  typedef typename InputImageType::RegionType RegionType;

  /** Type for the index of the input image. */
  typedef typename RegionType::IndexType IndexType;

  /** Type for the index of the input image. */
  typedef typename InputImageType::PixelType PixelType;

  /** Type for the size of the input image. */
  typedef typename RegionType::SizeType SizeType;

  /** Pointer Type for input image. */
  typedef typename InputImageType::ConstPointer InputImagePointer;

  /** Standard filter type within this class. */
  typedef GradientMagnitudeImageFilter< InputImageType, Image2DType > GradientFilterType;

  /** Standard vector type within this class. */
  typedef Vector< unsigned int, 2 >   MeasurementVectorUIntType;
  typedef Vector< double, 2 >         MeasurementVectorDoubleType;
  typedef std::vector< unsigned int > VectorType;
  typedef vnl_vector< double >        VectorOfDoubleType;
  /** Standard matrix type within this class. */
  typedef vnl_matrix< double > MatrixOfDoubleType;

  /** Standard list type within this class. */
  typedef Statistics::ListSample< MeasurementVectorUIntType > SampleType;

  /** Standard iterator type within this class. */
  typedef ImageRegionConstIterator< Image2DType  > ConstIteratorType;

  /** Set the input image. */
  virtual void SetImage(const InputImageType *image)
  {
    if ( m_Image != image )
      {
      m_Image = image;
      this->Modified();
      m_Valid = false;
      }
  }

  /** Set/Get the lenght of profile */
  itkSetMacro(LenghtOfProfile, unsigned int);
  itkGetMacro(LenghtOfProfile, unsigned int);

  /** Set/Get the number ofinteration */
  itkSetMacro(NumberOfIteration, unsigned int);
  itkGetMacro(NumberOfIteration, unsigned int);

  /** Set/Get the initial shape */
  itkSetMacro(MeanShape, VectorOfDoubleType);
  itkGetMacro(MeanShape, VectorOfDoubleType);

  /** Set/Get the eigen values */
  itkSetMacro(EigenValues, VectorOfDoubleType);
  itkGetMacro(EigenValues, VectorOfDoubleType);

  /** Set/Get the eigen vectors */
  itkSetMacro(EigenVectors, MatrixOfDoubleType);
  itkGetMacro(EigenVectors, MatrixOfDoubleType);

  /** Compute a new shape.
   * This method computes a new shape from a mean, eigenvectors and eigenvalues
   * given as a parameter and stores it in the object.  The values of these
   * parameters can then be retrieved by using
   * other methods of this object. */
  void GenerateData();

  /** Return the mean Shape of the model (x1 y1 x2 y2 ... xn yn),
   * where n = n. of landmarks */
  VectorOfDoubleType  GetNewShape();

protected:
  ActiveShapeModelGradientSearchMethod();
  ~ActiveShapeModelGradientSearchMethod() {}

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  ActiveShapeModelGradientSearchMethod(const Self &); //purposely not
                                                      // implemented
  void operator=(const Self &);                       //purposely not
                                                      // implemented

  bool               m_Valid;                         // Have parameters been
                                                      // computed yet?
  VectorOfDoubleType m_MeanShape;
  MatrixOfDoubleType m_EigenVectors;
  VectorOfDoubleType m_EigenValues;
  unsigned int       m_LenghtOfProfile;
  unsigned int       m_NumberOfIteration;
  VectorOfDoubleType m_DiffVector;
  VectorOfDoubleType m_Db;
  VectorOfDoubleType m_Blimit;
  VectorOfDoubleType m_NewShape;
  InputImagePointer  m_Image;
}; // class ActiveShapeModelGradientSearchMethod
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkActiveShapeModelGradientSearchMethod.txx"
#endif

#endif
