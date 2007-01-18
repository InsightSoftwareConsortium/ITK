// -------------------------------------------------------------------------
// itkQuadEdgeMeshToQuadEdgeMeshFilter.h
// $Revision: 1.1 $
// $Author: sylvain $
// $Name:  $
// $Date: 2007-01-18 19:22:19 $
// -------------------------------------------------------------------------
// This code is an implementation of the well known quad edge (QE) data
// structure in the ITK library. Although the original QE can handle non
// orientable 2-manifolds and its dual and its mirror, this implementation
// is specifically dedicated to handle orientable 2-manifolds along with
// their dual.
//
// Any comment, criticism and/or donation is welcome.
//
// Please contact any member of the team:
//
// - The frog master (Eric Boix)       eboix@ens-lyon.fr
// - The duck master (Alex Gouaillard) alexandre.gouaillard@sun.com
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------

#ifndef __ITKQUADEDGEMESH__MESHCOPY__H__
#define __ITKQUADEDGEMESH__MESHCOPY__H__

#include <itkMeshToMeshFilter.h>

namespace itkQE
{
    /**
     */
    template< typename TInputMesh, typename TOutputMesh >
        class MeshCopy
        : public itk::MeshToMeshFilter< TInputMesh, TOutputMesh >
        {
            public:
            /** Basic types. */
            typedef MeshCopy                                         Self;
            typedef itk::MeshToMeshFilter< TInputMesh, TOutputMesh > Superclass;
            typedef itk::SmartPointer< Self >                        Pointer;
            typedef itk::SmartPointer< const Self >                  ConstPointer;

            /** Input types. */
            typedef TInputMesh                              InputMeshType;
            typedef typename InputMeshType::Pointer         InputMeshPointer;
            typedef typename InputMeshType::ConstPointer    InputMeshConstPointer;
            typedef typename InputMeshType::CoordRepType    InputCoordRepType;
            typedef typename InputMeshType::PointType       InputPointType;
            typedef typename InputMeshType::PointIdentifier InputPointIdentifier;
            typedef typename InputMeshType::QEPrimal        InputQEPrimal;
            typedef typename InputMeshType::VectorType      InputVectorType;

            typedef typename InputMeshType::PointsContainerConstIterator
                InputPointsContainerConstIterator;
            typedef typename InputMeshType::CellsContainerConstIterator
                InputCellsContainerConstIterator;
            typedef typename InputMeshType::EdgeCellType    InputEdgeCellType;
            typedef typename InputMeshType::PolygonCellType InputPolygonCellType;
            typedef typename InputMeshType::PointIdList     InputPointIdList;

            typedef typename InputQEPrimal::IteratorGeom    InputQEIterator;

            /** Output types. */
            typedef TOutputMesh                               OutputMeshType;
            typedef typename OutputMeshType::Pointer          OutputMeshPointer;
            typedef typename OutputMeshType::ConstPointer     OutputMeshConstPointer;
            typedef typename OutputMeshType::CoordRepType     OutputCoordRepType;
            typedef typename OutputMeshType::PointType        OutputPointType;
            typedef typename OutputMeshType::PointIdentifier  OutputPointIdentifier;
            typedef typename OutputMeshType::QEPrimal         OutputQEPrimal;
            typedef typename OutputMeshType::VectorType       OutputVectorType;
            typedef typename OutputQEPrimal::IteratorGeom     OutputQEIterator;
            typedef typename OutputMeshType::PointsContainerIterator
                OutputPointsContainerIterator;

            public:
            itkNewMacro( Self );
            itkTypeMacro( MeshCopy, itk::MeshToMeshFilter );

            protected:
            MeshCopy( );
            virtual ~MeshCopy( ) { }

            virtual void GenerateData( );

            private:
            MeshCopy( const Self& ); // Not impl.
            void operator=( const Self& );  // Not impl.
        };

} // enamespace

#include "itkQEMeshCopy.txx"

#endif // __ITKQUADEDGEMESH__MESHCOPY__H__

// eof - itkQuadEdgeMeshToQuadEdgeMeshFilter.h
