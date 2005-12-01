/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshSourceGraftOutputTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <itkTransformMeshFilter.h>
#include <itkMesh.h>
#include <itkMeshSource.h>
#include <itkAffineTransform.h>
#include "itkMeshToMeshFilter.h"
#include "itkTransform.h"
#include "itkExceptionObject.h"

namespace itk
{
template <class TInputMesh, class TOutputMesh, class TTransform>
class MeshSourceGraftOutputFilter : 
    public MeshToMeshFilter<TInputMesh,TOutputMesh>
{
public:
  /** Standard class typedefs. */
  typedef MeshSourceGraftOutputFilter  Self;
  typedef MeshToMeshFilter<TInputMesh,TOutputMesh> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  typedef TInputMesh InputMeshType;
  typedef TOutputMesh OutputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;
  
  /** Type for representing coordinates. */
  typedef typename TInputMesh::CoordRepType  CoordRepType;

  /** Type of the transform. */
  typedef TTransform  TransformType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(MeshSourceGraftOutputFilter,MeshToMeshFilter);

  /** Set transform. */
  itkSetObjectMacro(Transform, TransformType); 

  /** Get transform. */
  itkGetObjectMacro(Transform,TransformType);

protected:
  MeshSourceGraftOutputFilter();
  ~MeshSourceGraftOutputFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Generate Requested Data */
  virtual void GenerateData( void );

  /** Transform to apply to all the mesh points. */
  typename TransformType::Pointer   m_Transform;

private:
  MeshSourceGraftOutputFilter(const MeshSourceGraftOutputFilter&); //purposely not implemented
  void operator=(const MeshSourceGraftOutputFilter&); //purposely not implemented
  
};

/**
 *
 */
template <class TInputMesh, class TOutputMesh, class TTransform>
MeshSourceGraftOutputFilter<TInputMesh,TOutputMesh,TTransform>
::MeshSourceGraftOutputFilter()
{
  m_Transform = TransformType::New();
}

/**
 *
 */
template <class TInputMesh, class TOutputMesh, class TTransform>
void 
MeshSourceGraftOutputFilter<TInputMesh,TOutputMesh,TTransform>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  if (m_Transform)
    {
    os << indent << "Transform: " << m_Transform << std::endl;
    }
};


/**
 * This method causes the filter to generate its output.
 */
template <class TInputMesh, class TOutputMesh, class TTransform>
void MeshSourceGraftOutputFilter<TInputMesh,TOutputMesh,TTransform>
::GenerateData(void) 
{
  typedef typename TInputMesh::PointsContainer  InputPointsContainer;
  typedef typename TOutputMesh::PointsContainer OutputPointsContainer;

  typedef typename TInputMesh::PointsContainerPointer  InputPointsContainerPointer;
  typedef typename TOutputMesh::PointsContainerPointer OutputPointsContainerPointer;

  InputMeshPointer    inputMesh      =  this->GetInput();
  OutputMeshPointer   outputMesh     =  this->GetOutput();
  
  if( !inputMesh )
    {
    itkExceptionMacro(<<"Missing Input Mesh");
    }

  if( !outputMesh )
    {
    itkExceptionMacro(<<"Missing Output Mesh");
    }

  // Declare the type for the filter
  typedef itk::TransformMeshFilter<
                                TInputMesh,
                                TOutputMesh,
                                TransformType  >       FilterType;
            

  // Create a Filter                                
  FilterType::Pointer filter = FilterType::New();

  // Connect the inputs
  filter->SetInput( inputMesh ); 
  filter->SetTransform( m_Transform ); 

  // Execute the filter
  filter->Update();
  std::cout << "Filter: " << filter;

  // Get the Smart Pointer to the Filter Output 
  GraftOutput( filter->GetOutput() );
/*
  outputMesh->SetBufferedRegion( outputMesh->GetRequestedRegion() );

  InputPointsContainerPointer  inPoints  = inputMesh->GetPoints();
  OutputPointsContainerPointer outPoints = outputMesh->GetPoints();

  outPoints->Reserve( inputMesh->GetNumberOfPoints() );
  outPoints->Squeeze();  // in case the previous mesh had 
                         // allocated a larger memory

  typename InputPointsContainer::ConstIterator  inputPoint  = inPoints->Begin();
  typename OutputPointsContainer::Iterator      outputPoint = outPoints->Begin();

  while( inputPoint != inPoints->End() ) 
    {
    outputPoint.Value() = 
      m_Transform->TransformPoint( inputPoint.Value() );

    ++inputPoint;
    ++outputPoint;
    }


  // Create duplicate references to the rest of data on the mesh

  outputMesh->SetPointData(  inputMesh->GetPointData() );
  
  outputMesh->SetCellLinks(  inputMesh->GetCellLinks() );
  
  outputMesh->SetCells(  inputMesh->GetCells() );
  outputMesh->SetCellData(  inputMesh->GetCellData() );
  
  
  unsigned int maxDimension = TInputMesh::MaxTopologicalDimension;

  for( unsigned int dim = 0; dim < maxDimension; dim++ )
    {
    outputMesh->SetBoundaryAssignments(  dim,
                                         inputMesh->GetBoundaryAssignments(dim) );
    }
*/
}

} // end namespace itk

int itkMeshSourceGraftOutputTest(int, char* [] ) 
{
  // Declare the mesh pixel type.
  // Those are the values associated 
  // with each mesh point. (not used on this filter test)
  typedef int PixelType;
  
  // Declare the types of the Mesh
  // By default it is a 3D mesh using itk::Point<float,3>
  // on the vertices, and an itk::VectorContainter
  // as containter for points
  typedef itk::Mesh<PixelType>  MeshType;

  // Declare the type for PointsContainer
  typedef MeshType::PointsContainer     PointsContainerType;

  // Declare the type for PointsContainerPointer
  typedef MeshType::PointsContainerPointer     
                                        PointsContainerPointer;
  // Declare the type for Points
  typedef MeshType::PointType           PointType;

  // Create an input Mesh
  MeshType::Pointer inputMesh  = MeshType::New();

  // Insert data on the Mesh
  PointsContainerPointer  points = inputMesh->GetPoints();

  // Fill a cube with points , just to get some data
  int n = 1;  // let's start with a few of them
  PointsContainerType::ElementIdentifier  count = 0; // count them

  for(int x= -n; x <= n; x++)
    {
    for(int y= -n; y <= n; y++)
      {
      for(int z= -n; z <= n; z++)
        {
        PointType p;
        p[0] = x;
        p[1] = y;
        p[2] = z;
        std::cout << "Inserting point # ";
        std::cout.width( 3); std::cout << count << "  = ";
        std::cout.width( 4); std::cout << p[0] << ", ";
        std::cout.width( 4); std::cout << p[1] << ", ";
        std::cout.width( 4); std::cout << p[2] << std::endl;
        points->InsertElement( count, p );
        count++;
        }
      }
    }
  
  std::cout << "Input Mesh has " << inputMesh->GetNumberOfPoints();
  std::cout << "   points " << std::endl;

  
  // Declare the transform type
  typedef itk::AffineTransform<float,3> TransformType;
  

  // Declare the type for the filter
  typedef itk::MeshSourceGraftOutputFilter<
                                MeshType,
                                MeshType,
                                TransformType  >       FilterType;
            

  // Create a Filter                                
  FilterType::Pointer filter = FilterType::New();
  
  // Create an  Transform 
  // (it doesn't use smart pointers)
  TransformType::Pointer   affineTransform = TransformType::New();
  affineTransform->Scale( 3.5 );
  TransformType::OffsetType::ValueType tInit[3] = {100,200,300};
  TransformType::OffsetType   translation = tInit;
  affineTransform->Translate( translation );

  // Connect the inputs
  filter->SetInput( inputMesh ); 
  filter->SetTransform( affineTransform ); 

  // Execute the filter
  filter->Update();
  std::cout << "Filter: " << filter;

  // Get the Smart Pointer to the Filter Output 
  MeshType::Pointer outputMesh = filter->GetOutput();

  std::cout << "Output Mesh has " << outputMesh->GetNumberOfPoints();
  std::cout << "   points " << std::endl;

  // Get the the point container
  MeshType::PointsContainerPointer  
                  transformedPoints = outputMesh->GetPoints();


  PointsContainerType::ConstIterator it = transformedPoints->Begin();
  while( it != transformedPoints->End() )
    {
    PointType p = it.Value();
    std::cout.width( 5 ); std::cout << p[0] << ", ";
    std::cout.width( 5 ); std::cout << p[1] << ", ";
    std::cout.width( 5 ); std::cout << p[2] << std::endl;
    ++it;
    }
  
  // All objects should be automatically destroyed at this point

  return 0;

}




