/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTriangleMeshToBinaryImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTriangleMeshToBinaryImageFilter_h
#define __itkTriangleMeshToBinaryImageFilter_h

#include "itkImageSource.h"

#include <itkMesh.h>
#include <itkLineCell.h>
#include <itkPolygonCell.h>
#include <itkVertexCell.h>
#include <itkMapContainer.h>
#include <itkVectorContainer.h>
#include <itkVector.h>
#include <itkPoint.h>
#include <itkArray.h>
#include "itkAutomaticTopologyMeshSource.h"
#include "itkPointSet.h"

#include <vector>

namespace itk
{

  class Point1D  
    {
    public:
      double x;
      int   sign;
     
      Point1D(){};
      Point1D(const double p, const int s)
        {
        x = p;
        sign = s;
        };
      Point1D(const Point1D &point)
        {
        x = point.x;
        sign = point.sign;
        };
      const double getX() const
        {
        return x;
        };
      const int  getSign() const
        {
        return sign;
        };     
  
    };
/** \class TriangleMeshToBinaryImageFilter
 *
 * \brief 3D Rasterization algorithm Courtesy of Dr David Gobbi of Atamai Inc.

 * \author Leila Baghdadi, MICe, Hospital for Sick Childern, Toronto, Canada,
 */
template <class TInputMesh, class TOutputImage>
class ITK_EXPORT TriangleMeshToBinaryImageFilter : public ImageSource<TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef TriangleMeshToBinaryImageFilter  Self;
  typedef ImageSource<TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  typedef typename TOutputImage::IndexType  IndexType;
  typedef typename TOutputImage::SizeType   SizeType;
  typedef TOutputImage  OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::ValueType  ValueType;
  typedef typename OutputImageType::SpacingType SpacingType;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(TriangleMeshToBinaryImageFilter,ImageSource);

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Some convenient typedefs. */
  typedef TInputMesh                             InputMeshType;
  typedef typename InputMeshType::Pointer        InputMeshPointer;
  typedef typename InputMeshType::PointType                       InputPointType;
  typedef typename InputMeshType::PixelType                       InputPixelType;
  typedef typename InputMeshType::MeshTraits::CellTraits          InputCellTraitsType;
  typedef typename InputMeshType::CellType                        CellType;
  typedef typename InputMeshType::CellsContainerPointer           CellsContainerPointer;
  typedef typename InputMeshType::CellsContainerIterator          CellsContainerIterator;

  typedef typename InputMeshType::PointsContainer                 InputPointsContainer;
  typedef typename InputPointsContainer::Pointer                  InputPointsContainerPointer;
  typedef typename InputPointsContainer::Iterator                 InputPointsContainerIterator;
  
  typedef itk::PointSet < double , 3 > PointSetType;
  
  typedef PointSetType::PointsContainer      PointsContainer;
  

  typedef itk::Point < double, 3> PointType;
  typedef itk::Point < double, 2> Point2DType;
  
  typedef itk::Array<double> DoubleArrayType;

  typedef std::vector< Point1D >  Point1DVector;
  typedef std::vector< std::vector< Point1D> > Point1DArray;

  typedef std::vector< Point2DType >       Point2DVector;
  typedef std::vector< std::vector< Point2DType > >       Point2DArray;

  typedef std::vector< PointType >       PointVector;
  typedef std::vector< std::vector< PointType > >       PointArray;

  typedef std::vector< int > StencilIndexVector;
  /** Spacing (size of a pixel) of the output image. The
   * spacing is the geometric distance between image samples.
   * It is stored internally as double, but may be set from
   * float. \sa GetSpacing() */
  itkSetMacro(Spacing, SpacingType);
  virtual void SetSpacing( const double spacing[3] );
  virtual void SetSpacing( const float spacing[3] );
  itkGetConstReferenceMacro(Spacing, SpacingType);

  /** Set/Get the value for pixels inside the spatial object. 
  * By default, this filter will return an image
  * If this "inside" value is changed to a non-null value,
  * the output produced by this filter will be a mask with inside/outside values 
  * specified by the user. */
  itkSetMacro(InsideValue, ValueType);
  itkGetMacro(InsideValue, ValueType);

  /** Set/Get the value for pixels outside the spatial object.
  * By default, this filter will return an image
  * If this "outside" value is changed to a non-null value,
  * the output produced by this filter will be a mask with inside/outside values
  * specified by the user. */
  itkSetMacro(OutsideValue, ValueType);
  itkGetMacro(OutsideValue, ValueType);

  /** The origin of the output image. The origin is the geometric
   * coordinates of the index (0,0,...,0).  It is stored internally
   * as double but may be set from float.
   * \sa GetOrigin() */
  itkSetMacro(Origin, PointType);
  virtual void SetOrigin( const double origin[3] );
  virtual void SetOrigin( const float origin[3] );
  itkGetConstReferenceMacro(Origin, PointType);
  
  /** Set/Get Index */
  itkSetMacro(Index,IndexType);
  itkGetMacro(Index,IndexType);

  /** Set/Get Size */
  itkSetMacro(Size,SizeType);
  itkGetMacro(Size,SizeType);

  /** Set the mesh input of this process object.  */
  void SetInput(InputMeshType *input);

  /** Get the mesh input of this process object.  */
  InputMeshType * GetInput(void);
  InputMeshType * GetInput(unsigned int idx);

  /* Set the tolerance for doing spatial searches of the polydata. */
  itkSetMacro(Tolerance, double);
  itkGetMacro(Tolerance, double);
    
protected:
  TriangleMeshToBinaryImageFilter();
  ~TriangleMeshToBinaryImageFilter();

  virtual void GenerateOutputInformation(){}; // do nothing
  virtual void GenerateData();

  virtual void RasterizeTriangles();
  static int PolygonToImageRaster( PointVector coords, Point1DArray & zymatrix, int extent[6]);
  
  
  IndexType    m_Index;
  SizeType     m_Size;
  SpacingType  m_Spacing;
  PointType    m_Origin; //start value
  double       m_Tolerance;
  ValueType    m_InsideValue;
  ValueType    m_OutsideValue;
  StencilIndexVector  StencilIndex;

  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  TriangleMeshToBinaryImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  static bool ComparePoints2D(Point2DType a, Point2DType b);
  static bool ComparePoints1D(Point1D a, Point1D b);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTriangleMeshToBinaryImageFilter.txx"
#endif

#endif
