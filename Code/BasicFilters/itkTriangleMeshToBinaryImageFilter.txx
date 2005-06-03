/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkTriangleMeshToBinaryImageFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkTriangleMeshToBinaryImageFilter_txx
#define _itkTriangleMeshToBinaryImageFilter_txx

#include "itkTriangleMeshToBinaryImageFilter.h"
#include <itkImageRegionIteratorWithIndex.h>


namespace itk
{
/** Constructor */
template <class TInputMesh, class TOutputImage>
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::TriangleMeshToBinaryImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
 
  m_Size.Fill(0);
  m_Index.Fill(0);
  
  for (unsigned int i = 0; i < 3; i++)
    {
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0;
    }

  m_InsideValue = 1;
  m_OutsideValue = 0;
  
  m_Tolerance = 1e-5;
  
}

/** Destructor */
template <class TInputMesh, class TOutputImage>
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::~TriangleMeshToBinaryImageFilter()
{
}
  

/** Set the Input Mesh */
template <class TInputMesh, class TOutputImage>
void 
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::SetInput(TInputMesh *input)
{
  
  this->ProcessObject::SetNthInput(0, input);
}

/** Get the input Mesh */
template <class TInputMesh, class TOutputImage>
typename TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>::InputMeshType *
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::GetInput(void) 
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TInputMesh * >
    (this->ProcessObject::GetInput(0) );

}
  
/** Get the input Mesh */
template <class TInputMesh, class TOutputImage>
typename TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>::InputMeshType *
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::GetInput(unsigned int idx)
{
  return static_cast< TInputMesh * >
    (this->ProcessObject::GetInput(idx));

}


//----------------------------------------------------------------------------
template <class TInputMesh, class TOutputImage>
void 
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::SetSpacing(const SpacingType& spacing )
{
  unsigned int i; 
  for (i=0; i<TOutputImage::ImageDimension; i++)
    {
    if ( (double)spacing[i] != m_Spacing[i] )
      {
      break;
      }
    } 
  if ( i < TOutputImage::ImageDimension ) 
    { 
    for (i=0; i<TOutputImage::ImageDimension; i++)
      {
      m_Spacing[i] = spacing[i];
      }
    this->Modified();
    }
}

//----------------------------------------------------------------------------
template <class TInputMesh, class TOutputImage>
void 
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::SetSpacing(const double spacing[3] )
{
  unsigned int i; 
  for (i=0; i<3; i++)
    {
    if ( spacing[i] != m_Spacing[i] )
      {
      break;
      }
    } 
  if ( i < 3 ) 
    { 
    for (i=0; i<3; i++)
      {
      m_Spacing[i] = spacing[i];
      }
    }
}

template <class TInputMesh, class TOutputImage>
void 
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::SetSpacing(const float spacing[3] )
{
  unsigned int i; 
  for (i=0; i<3; i++)
    {
    if ( (double)spacing[i] != m_Spacing[i] )
      {
      break;
      }
    } 
  if ( i < 3 ) 
    { 
    for (i=0; i<3; i++)
      {
      m_Spacing[i] = spacing[i];
      }
    }
}

template <class TInputMesh, class TOutputImage>
const double * 
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::GetSpacing() const
{
  return m_Spacing;
}


//----------------------------------------------------------------------------
template <class TInputMesh, class TOutputImage>
void 
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::SetOrigin(const PointType& origin)
{
  unsigned int i; 
  for (i=0; i<TOutputImage::ImageDimension; i++)
    {
    if ( (double)origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < TOutputImage::ImageDimension ) 
    { 
    for (i=0; i<TOutputImage::ImageDimension; i++)
      {
      m_Origin[i] = origin[i];
      }
    this->Modified();
    }
}

//----------------------------------------------------------------------------
template <class TInputMesh, class TOutputImage>
void 
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::SetOrigin(const double origin[3] )
{
  unsigned int i; 
  for (i=0; i<3; i++)
    {
    if ( origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < 3) 
    { 
    for (i=0; i<3; i++)
      {
      m_Origin[i] = origin[i];
      }
    }
}

template <class TInputMesh, class TOutputImage>
void 
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::SetOrigin(const float origin[3] )
{
  unsigned int i; 
  for (i=0; i<3; i++)
    {
    if ( (double)origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < 3 ) 
    { 
    for (i=0; i<3; i++)
      {
      m_Origin[i] = origin[i];
      }
    }
}

template <class TInputMesh, class TOutputImage>
const double * 
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::GetOrigin() const
{
  return m_Origin;
}

// used by an STL sort
template <class TInputMesh, class TOutputImage>
bool
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::ComparePoints2D(Point2DType a, Point2DType b)
{
  // sort xy points by ascending y value, then x
  if (a[1] == b[1])
    {
    return (a[0] < b[0]);
    }
  else
    {
    return (a[1] < b[1]);
    }
}

// used by an STL sort
template <class TInputMesh, class TOutputImage>
bool
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::ComparePoints1D(Point1D a, Point1D b)
{
  return (a.x < b.x);
}
//----------------------------------------------------------------------------

/** Update */
template <class TInputMesh, class TOutputImage>
void
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::GenerateData(void)
{
  unsigned int i;
  itkDebugMacro(<< "TriangleMeshToBinaryImageFilter::Update() called");

  RasterizeTriangles();
  
  // Get the input and output pointers 
  OutputImagePointer   OutputImage = this->GetOutput();

  if (m_Size[0] == 0 ||  m_Size[1] == 0 ||  m_Size[2] == 0)
    {
    itkExceptionMacro(<< "Must Set Image Size");  
    }

  typename OutputImageType::RegionType region;

  region.SetSize ( m_Size );
  region.SetIndex( m_Index );

  OutputImage->SetLargestPossibleRegion( region);     // 
  OutputImage->SetBufferedRegion( region );           // set the region 
  OutputImage->SetRequestedRegion( region );          //                                                                       
  OutputImage->SetSpacing(m_Spacing);         // set spacing
  OutputImage->SetOrigin(m_Origin);   //   and origin

  OutputImage->Allocate();   // allocate the image                            
  
  typedef itk::ImageRegionIteratorWithIndex<OutputImageType> myIteratorType;

  myIteratorType it(OutputImage,region);

  
  int DataIndex=0;
  int StencilId=0;
  it.GoToBegin();
  
  int n=StencilIndex.size();
  int StencilMin = StencilIndex[0];
  int StencilMax = StencilIndex[n-1];
  
  while(!it.IsAtEnd())
    {
    if (DataIndex >=  StencilMin && DataIndex <=  StencilMax )
      {
      if (DataIndex == StencilIndex[StencilId])
        {
        it.Set(m_InsideValue);
        StencilId++;
        }
      else
        {
        it.Set(m_OutsideValue);
        }
      }
    else
      {
      it.Set(m_OutsideValue);
      }
    DataIndex++;
    ++it;
    }
  itkDebugMacro(<< "TriangleMeshToBinaryImageFilter::Update() finished");

} // end update function  

//----------------------------------------------------------------------------
/** convert a single polygon/triangle to raster format */
template<class TInputMesh, class TOutputImage>
int
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>
::PolygonToImageRaster(PointVector coords, Point1DArray & zymatrix, int  extent[6])
{
  // convert the polgon into a rasterizable form by finding its
  // intersection with each z plane, and store the (x,y) coords
  // of each intersection in a vector called "matrix"
  int zSize = extent[5]-extent[4]+1;
  int zInc = extent[3]-extent[2]+1;
  Point2DArray matrix(zSize);
  
  // each iteration of the following loop examines one edge of the
  // polygon, where the endpoints of the edge are p1 and p2
  int n = (int) (coords.size());
  PointType p0 = coords[0];
  PointType p1 = coords[n-1];
  double area = 0.0;

  for (int i=0; i < n; i++)
    {
    PointType p2 = coords[i];
    // calculate the area (actually double the area) of the polygon's
    // projection into the zy plane via cross product, one triangle
    // at a time
    double v1y = p1[1]-p0[1];
    double v1z = p1[2]-p0[2];
    double v2y = p2[1]-p0[1];
    double v2z = p2[2]-p0[2];
    area += (v1y*v2z - v2y*v1z);
     
    // skip any line segments that are perfectly horizontal
    if (p1[2] == p2[2])
      {
      p1 = coords[i];
      continue;
      }
     
    // sort the endpoints, this improves robustness
    if (p1[2] > p2[2])
      {
      std::swap(p1,p2);
      }

    int zmin = (int)(ceil(p1[2]));
    int zmax = (int)(ceil(p2[2]));

    if (zmin > extent[5] || zmax < extent[4])
      {
      continue;
      }

    // cap to the volume extents
    if (zmin < extent[4])
      {
      zmin = extent[4];
      }
    if (zmax > extent[5])
      {
      zmax = extent[5]+1;
      }
    double temp = 1.0/(p2[2] - p1[2]);
    for (int z = zmin; z < zmax; z++)
      {
      double r = (p2[2] - (double)(z))*temp;
      double f = 1.0 - r;
      Point2DType XY;
      XY[0] = r*p1[0] + f*p2[0];
      XY[1] = r*p1[1] + f*p2[1];
      matrix[z-extent[4]].push_back(XY);
      }
     
    p1 = coords[i];
     
    } //end of for loop

  // area is not really needed, we just need the sign
  int sign;
  if (area < 0.0)
    {
    sign = -1;
    }
  else if (area > 0.0)
    {
    sign = 1;
    }
  else
    {
    return 0;
    }
  
  // rasterize the polygon and store the x coord for each (y,z)
  // point that we rasterize, kind of like using a depth buffer
  // except that 'x' is our depth value and we can store multiple
  // 'x' values per (y,z) value.
  
  for (int z = extent[4]; z <= extent[5]; z++)
    {
      
    Point2DVector &xylist = matrix[z-extent[4]];
      
    if (xylist.empty())
      {
      continue;
      }
      
    // sort by ascending y, then x
    std::sort(xylist.begin(), xylist.end(), ComparePoints2D);
    
    int n = (int)(xylist.size())/2;
    for (int k = 0; k < n; k++)
      {  
      Point2DType  &p1 = xylist[2*k];
      double X1 = p1[0];
      double Y1 = p1[1];
      Point2DType  &p2 = xylist[2*k+1];
      double X2 = p2[0];
      double Y2 = p2[1];
      
      if (Y2 == Y1)
        {
        continue;
        }  
      double temp = 1.0/(Y2 - Y1);
      int ymin = (int)(ceil(Y1));
      int ymax = (int)(ceil(Y2)); 
      for (int y = ymin; y < ymax; y++)
        {
        double r = (Y2 - y)*temp;
        double f = 1.0 - r;
        double X = r*X1 + f*X2;
        if (extent[2] <= y && y <= extent[3])
          {
          int zyidx = (z-extent[4])*zInc+(y-extent[2]);   
          zymatrix[zyidx].push_back( Point1D(X,sign) );
          }
        }
      }
    }
 
  return sign;
      
}

/** raterize : Courtesy of Dr D Gobbi of Atamai Inc.*/
template <class TInputMesh, class TOutputImage>
void
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::RasterizeTriangles()
{
//try to print my small mesh
  
  InputMeshPointer input = this->GetInput(0);
  
  InputPointsContainerPointer      myPoints = input->GetPoints();
  InputPointsContainerIterator     points = myPoints->Begin();
 
  int extent[6];

  // create a similar extent like vtk
  extent[0] = m_Index[0];
  extent[1] = m_Size[0] - 1;
  extent[2] = m_Index[1];
  extent[3] = m_Size[1] - 1;
  extent[4] = m_Index[2];
  extent[5] = m_Size[2] - 1;
  
  const double *spacing = m_Spacing;
  const double *origin  = m_Origin;

  // Only divide once
  double invspacing[3];
  invspacing[0] = 1.0/spacing[0];
  invspacing[1] = 1.0/spacing[1];
  invspacing[2] = 1.0/spacing[2];

  // need to translate points and create new points
  PointsContainer::Pointer NewPoints = PointsContainer::New();
  PointSetType::Pointer NewPointSet = PointSetType::New();
  PointSetType::PointType newpoint;

  InputPointType point;

  unsigned int pointId = 0;
  while( points != myPoints->End() ) 
    {
   
    point = points.Value();
    
    newpoint[0] = (point[0]-origin[0]) * invspacing[0];
    newpoint[1] = (point[1]-origin[1]) * invspacing[1];
    newpoint[2] = (point[2]-origin[2]) * invspacing[2];

    NewPoints->InsertElement(pointId++, newpoint);
    
    points++;
    }
  NewPointSet->SetPoints(NewPoints);

  // the stencil is kept in 'zymatrix' that provides 
  // the x extents for each (y,z) coordinate for which a ray 
  // parallel to the x axis intersects the polydata
  int zInc = extent[3]-extent[2]+1;
  int zSize = extent[5]-extent[4]+1;
  Point1DArray zymatrix(zInc*zSize);
  PointVector coords;
  
  CellsContainerPointer cells = input->GetCells();
  CellsContainerIterator cellIt = cells->Begin();
  
  while ( cellIt != cells->End() )
    {
     
    CellType *nextCell = cellIt->Value();
    typename CellType::PointIdIterator pointIt = nextCell->PointIdsBegin() ;
    PointType  p;
    
    switch (nextCell->GetType())
      {
      case CellType::TRIANGLE_CELL:
      case CellType::POLYGON_CELL:
      {
      pointId = 0;
      coords.clear();
      while (pointIt != nextCell->PointIdsEnd() ) 
        {
        NewPointSet->GetPoint(*pointIt++, &newpoint);
        p[0] = newpoint[0];
        p[1] = newpoint[1];
        p[2] = newpoint[2];
        coords.push_back(p);
        }
      this->PolygonToImageRaster(coords, zymatrix, extent);
      }
      break;
      default:
        itkExceptionMacro(<< "Need Triangle or Polygon cells ONLY");
      }
    cellIt++;
    }
 
      
  // we only want to generate a max of 1 warning per execute
  int alreadywarned = 1;
  
  //create the equivalent of vtkStencilData from our zymatrix
  for (int z = extent[4]; z <= extent[5]; z++)
    {
    for (int y = extent[2]; y <= extent[3]; y++)
      {
      int zyidx = (z-extent[4])*zInc+(y-extent[2]);
      Point1DVector xlist = zymatrix[zyidx];
  
      if (xlist.empty())
        {
        continue;
        }
      if (xlist.size() > 1)
        {
        std::sort(xlist.begin(), xlist.end(), ComparePoints1D);
        }
      //get the first entry
      double lastx = xlist[0].x;
      int signproduct = 1;
  
      // if adjacent x values are within tolerance of each
      // other, check whether the number of 'exits' and
      // 'entrances' are equal (via signproduct) and if so,
      // ignore all x values, but if not, then count
      // them as a single intersection of the ray with the
      // surface

      std::vector<double> nlist;
      int m = xlist.size();
      for (int j = 1; j < m; j++)
        {
        Point1D p1D = xlist[j];
        double x = p1D.x;
        int sign = p1D.sign;

        //check absolute distance from lastx to x
        if (((x < lastx) ? (lastx - x) : (x - lastx)) > m_Tolerance)
          {
          if (signproduct > 0)
            {
            nlist.push_back(lastx);
            }
          signproduct = 1;
          }
        else
          {
          signproduct *= sign;
          }
        lastx = x;
        }
      if (signproduct > 0)
        {
        nlist.push_back(lastx);
        }
      // if xlist length is not divisible by two, then
      // the polydata isn't a closed surface
      if ((int)(nlist.size())%2 != 0 && !alreadywarned)
        {
        alreadywarned = 1;
        itkWarningMacro(<<"RequestInformation: PolyData does not form a closed surface");
        }
      // create the stencil extents
      int minx1 = extent[0]; // minimum allowable x1 value
      int n = (int)(nlist.size())/2;
      
      for (int i = 0; i < n; i++)
        {
        int x1 = (int)(ceil(nlist[2*i]));
        int x2 = (int)(floor(nlist[2*i+1]));
        
        if (x2 < extent[0] || x1 > (extent[1]))
          {
          continue;
          }
        x1 = (x1 > minx1) ? (x1) : (minx1); // max(x1,minx1)
        x2 = (x2 < extent[1]) ? (x2) : (extent[1]); //min(x2,extent[1])
 
        if (x2 >= x1)
          {
          for (int idX = x1; idX <= x2; idX++)
            {
            StencilIndex.push_back(idX + y * m_Size[0] + z * m_Size[0] * m_Size[1]);
            }
          }
        // next x1 value must be at least x2+1
        minx1 = x2+1;
        }   
      }
    }
}


template<class TInputMesh, class TOutputImage>
void 
TriangleMeshToBinaryImageFilter<TInputMesh,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Size : " << m_Size << std::endl;
  
  os << indent << "Inside Value : " << m_InsideValue << std::endl;
  os << indent << "Outside Value : " << m_OutsideValue << std::endl;
  os << indent << "Tolerance: " << m_Tolerance << std::endl;
}

} // end namespace itk

#endif
