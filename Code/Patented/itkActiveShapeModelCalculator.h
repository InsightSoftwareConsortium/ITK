/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkActiveShapeModelCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkActiveShapeModelCalculator_h
#define __itkActiveShapeModelCalculator_h

#include <time.h>
#include <math.h>

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_math.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include <vnl/algo/vnl_generalized_eigensystem.h>
#include <vnl/algo/vnl_symmetric_eigensystem.h>

#include "itkImageSliceConstIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkBinaryThinningImageFilter.h"
#include "itkBinaryPruningImageFilter.h"
#include "itkPointSet.h"
#include "itkVector.h"
#include "itkListSample.h"
#include "itkExceptionObject.h"

#include "itkConceptChecking.h"

#include <vector>
#include <list>

namespace itk
{
/** \class ActiveShapeModelCalculator
 * \brief Base class for ActiveShapeModelCalculator object
 *
 * itkActiveShapeModelCalculator performs a principal component analysis
 * (PCA) on a set of binary tranining images. The input is a binary volume that contains the shapes.
 * The number of largest principal components which explain 98% of shape variance is automatically computed.
 * The ITK pipeline mechanism sets up the storage for input images. The outputs are the mean shape,
 * eigenvectors and eigenvalues. The user specifies the tolerance value (in pixel unit) for automatic landmark
 * identification.
 * The algorithm uses the VNL library to perform the eigen analysis.
 *
 * The Update() function enables the calculation of the various models, creates
 * the membership function objects and populates them.
 *
 * \par REFERENCES
 * \par
 * [1] Cootes T., Taylor C., et. al, Active Shape Models - Their training and application,
 *     Comput. Vis. Image Understanding, vol. 61, pp. 38-59, 1995.
 *
 * [2] D. H. Douglas and T. K. Peucker, Algorithms for the reduction of the number of points required
 *     to represent a digitized line or its caricature, Canadian Cartographer 10(2), pp. 112--122, 1973.

 *
 * \ingroup Operators */

template< class TImage >
class ITK_EXPORT ActiveShapeModelCalculator:public Object
{
public:
  /** Standard class typedefs. */
  typedef ActiveShapeModelCalculator< TImage > Self;
  typedef Object                               Superclass;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ActiveShapeModelCalculator, Object);

  /** Extract the dimension of the image.
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);*/

  /** Standard pixel type within this class. */
  typedef float         PixelType;
  typedef unsigned char Pixel8bitsType;

  /** Standard vector type within this class. */
  typedef Vector< double, 2 >         Vector2DType;
  typedef Vector< double, 3 >         Vector3DType;
  typedef Vector< unsigned int, 2 >   MeasurementVectorType;
  typedef std::vector< unsigned int > VectorType;
  typedef vnl_vector< double >        VectorOfDoubleType;

  /** Standard matrix type within this class. */

  typedef vnl_matrix< double > MatrixOfDoubleType;
  typedef vnl_matrix< int >    MatrixOfIntegerType;

  /** Standard image type within this class. */
  typedef TImage                     Image3DType;
  typedef Image< PixelType, 2 >      Image2DType;
  typedef Image< Pixel8bitsType, 2 > Image2D8bitsType;
  typedef Image< Pixel8bitsType, 3 > Image3D8bitsType;

  /** Standard index type within this class. */
  typedef typename Image2D8bitsType::IndexType IndexType;
  typedef typename Image3DType::IndexType      Index3DType;

  /** Standard image type pointer within this class. */
  typedef typename Image2DType::Pointer      Image2DPointer;
  typedef typename Image3DType::ConstPointer Image3DConstPointer;

  /** Standard PointSet type within this class. */
  typedef PointSet< unsigned short, 2 >    PointSetType;
  typedef typename PointSetType::PointType PointType;

  /** Standard container type within this class. */
  typedef typename PointSetType::PointsContainer    PointsContainer;
  typedef typename PointSetType::PointDataContainer PointDataContainer;

  /** Standard list type within this class. */
  typedef std::list< IndexType >                          List2DType;
  typedef Statistics::ListSample< MeasurementVectorType > SampleType;

  /** Standard iterator type within this class. */
  typedef ImageLinearIteratorWithIndex< Image2DType >     LinearIteratorType;
  typedef ImageSliceConstIteratorWithIndex< Image3DType > SliceIteratorType;
  typedef ImageRegionConstIterator< Image3D8bitsType  >   ConstIteratorType;
  typedef ImageRegionIterator< Image2D8bitsType >         IteratorType;
  typedef NeighborhoodIterator< Image2D8bitsType >        NeighborIteratorType;
  typedef typename PointsContainer::Iterator              PointsIterator;

  /** Standard filter type within this class. */
  typedef GradientMagnitudeImageFilter< Image3DType, Image3DType >        GradientFilterType;
  typedef BinaryThresholdImageFilter< Image3DType, Image3DType >          BinaryFilterType;
  typedef BinaryThresholdImageFilter< Image2DType, Image2D8bitsType >     BinaryFilterType1;
  typedef BinaryThresholdImageFilter< Image3DType, Image3D8bitsType >     BinaryFilterType2;
  typedef DanielssonDistanceMapImageFilter< Image3DType, Image3DType >    DistanceMapFilterType;
  typedef BinaryThinningImageFilter< Image2D8bitsType, Image2D8bitsType > ThinFilterType;
  typedef BinaryPruningImageFilter< Image2D8bitsType, Image2D8bitsType >  PruneFilterType;

  /** Set the input binary image (values in {0,255}) . */
  virtual void SetImage(const Image3DType *image)
  {
    if ( m_Image != image )
      {
      m_Image = image;
      this->Modified();
      m_Valid = false;
      }
  }

  /** Set/Get the lower threshold for the binary filter applied on the gradient images.
      Changing this value from the default is not recommended or necessary but could be used to
      get thinner contours at the risk of creating an unstable solution. */
  void SetLowerThresholdGradient(const double & lt)
  { m_LowerThreshold = lt; }
  const double & GetLowerThresholdGradient() const
  { return m_LowerThreshold; }

  /** Set/Get the distance threshold for the binary filter applied on the mean of the
      distance images. Changing this value from the default is not recommended or necessary but
      could be used to get thicker contours at the risk of creating an unstable solution. */
  void SetUpperThresholdMeanDistance(const double & ut1)
  { m_UpperThreshold1 = ut1; }
  const double & GetUpperThresholdMeanDistance() const
  { return m_UpperThreshold1; }

  /** Set/Get the distance threshold for the binary filter applied on the distance images
      Changing this value from the default is not recommended or necessary but could be used to
      get thicker contours at the risk of creating an unstable solution. */
  void SetUpperThresholdDistance(const double & ut2)
  { m_UpperThreshold2 = ut2; }
  const double & GetUpperThresholdDistance() const
  { return m_UpperThreshold2; }

  /** Set/Get the tolerance threshold (in pixels unit) for the automatic
    landmarks. */
  void SetTolerance(const double & t)
  { m_Tolerance = t; }
  const double & GetTolerance() const
  { return m_Tolerance; }

  /** Set/Get the number of iteration for Pruning filter */
  void SetPruneIteration(const unsigned int & t)
  { m_PruneIteration = t; }
  const unsigned int & GetPruneIteration() const
  { return m_PruneIteration; }

  /** Get the number of training images. */
  const unsigned int & GetNumberOfTrainingImages() const
  { return m_NumberOfTrainingImages; }

  /** Compute mean, eigenvectors and eigenvalues of a new or modified training set.
   * This method computes the mean, eigenvectors and eigenvalues of the training set
   * given as a parameter and stores them in the object.  The values of these
   * parameters can then be retrieved by using
   * other methods of this object. */
  void GenerateData();

  /** Return the mean Shape of the model (x1 y1 x2 y2 ... xn yn), where n = n.
    of landmarks*/
  VectorOfDoubleType  GetMeanShape();

  /** Return the largest t eigenvalues [1 x t]*/
  VectorOfDoubleType  GetEigenvalues();

  /** Return the matrix where each column corresponds to a eigenvector [2n x
    t]*/
  MatrixOfDoubleType  GetEigenvector();

protected:

  ActiveShapeModelCalculator():m_NumberOfTrainingImages(0)
  {
    m_LowerThreshold      = 10.0; // default value
    m_UpperThreshold1     = 1.0;  // default value
    m_UpperThreshold2     = 1.0;  // default value
    m_Tolerance           = 2.0;  // default value
    m_PruneIteration      = 3;    // default value
    m_NumberOfTrainingImages = 0;
    m_EigenVectors.set_size(0, 0);
    m_EigenValues.set_size(0);
    m_Means.set_size(0);
    m_Valid = false;
    m_Image = NULL;
  }

  ~ActiveShapeModelCalculator() {}

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "LowerThreshold: " << m_LowerThreshold << std::endl;
    os << indent << "UpperThreshold1: " << m_UpperThreshold1 << std::endl;
    os << indent << "UpperThreshold2: " << m_UpperThreshold2 << std::endl;
    os << indent << "Tolerance: " << m_Tolerance << std::endl;
    os << indent << "Number of training images: " << m_NumberOfTrainingImages << std::endl;
    os << indent << "Number of iteration for Prunig filter: " << m_PruneIteration << std::endl;
    itkDebugMacro(<< "                                    ");
    itkDebugMacro(<< "Results of the shape model");
    itkDebugMacro(<< "====================================");

    itkDebugMacro(<< "The mean shape: ");

    itkDebugMacro(<< m_Means);

    itkDebugMacro(<< " ");
    itkDebugMacro(<< "==================   ");

    itkDebugMacro(<< "The eigen values: ");

    itkDebugMacro(<< m_EigenValues);

    itkDebugMacro(<< " ");
    itkDebugMacro(<< "==================   ");

    itkDebugMacro(<< "The eigen vectors: ");

    for ( unsigned int i = 0; i < m_Means.size(); i++ )
      {
      itkDebugMacro( << m_EigenVectors.get_row(i) );
      }

    itkDebugMacro(<< " ");
    itkDebugMacro(<< "+++++++++++++++++++++++++");
  }  // end PrintSelf

private:
  ActiveShapeModelCalculator(const Self &); //purposely not implemented
  void operator=(const Self &);             //purposely not implemented

  bool                m_Valid;                        // Have parameters been
                                                      // computed yet?
  float               m_LowerThreshold;
  float               m_UpperThreshold1;
  float               m_UpperThreshold2;
  float               m_Tolerance;
  VectorOfDoubleType  m_Means;
  MatrixOfDoubleType  m_EigenVectors;
  VectorOfDoubleType  m_EigenValues;
  unsigned int        m_NumberOfTrainingImages;
  unsigned int        m_PruneIteration;
  Image3DConstPointer m_Image;
}; // class ActiveShapeModelCalculator
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkActiveShapeModelCalculator.txx"
#endif

#endif
