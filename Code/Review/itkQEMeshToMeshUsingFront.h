// -------------------------------------------------------------------------
// itkQEMeshToMeshUsingFront.h
// $Revision: 1.1 $
// $Author: sylvain $
// $Name:  $
// $Date: 2007-01-09 00:58:17 $
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

#ifndef __ITKQUADEDGEMESH__MESHTOMESHUSINGFRONT__H__
#define __ITKQUADEDGEMESH__MESHTOMESHUSINGFRONT__H__

#include <list>
#include "itkQEMeshFrontPaths.h"
#include "itkQEMeshCopy.h"

namespace itkQE
{
/**
 */
template< class TInputMesh, class TOutputMesh, class TQE >
    class MeshToMeshUsingFront
        : public MeshFrontPaths< TOutputMesh, TQE >,
          public MeshCopy< TInputMesh, TOutputMesh >
    {
        public:
        // Standard types
        typedef MeshToMeshUsingFront                Self;
        typedef MeshCopy< TInputMesh, TOutputMesh > Superclass;
        typedef MeshFrontPaths< TOutputMesh, TQE >  Superclass2;
        typedef itk::SmartPointer< Self >           Pointer;
        typedef itk::SmartPointer< const Self >     ConstPointer;

        // Template parameters
        typedef TInputMesh  IMeshType;
        typedef TOutputMesh OMeshType;
        
        // Types in superclass
        typedef typename Superclass::OutputMeshPointer      MeshPointer;
        typedef typename Superclass::OutputMeshConstPointer MeshConstPointer;
        typedef typename Superclass::OutputPointIdentifier  PointIdentifier;
        typedef typename Superclass::OutputVectorType       VectorType;
        typedef typename Superclass::OutputCoordRepType     CoordRepType;
        typedef typename Superclass2::FrontType             FrontType;
        typedef typename Superclass2::FrontIterator         FrontIterator;
        typedef typename Superclass2::FrontConstIterator    FrontConstIterator;
        typedef typename Superclass2::FrontQEType FrontQEType;
        
        public:
        itkNewMacro( Self );
        itkTypeMacro( MeshToMeshUsingFront, itkProcessObject );

        protected:
        MeshToMeshUsingFront( )
           : Superclass2( ),
             Superclass( )
        { };
        virtual ~MeshToMeshUsingFront( ) { };

        virtual void InitFront( ) { }; 
        virtual void GenerateData( )
           {
              // copy input mesh into output mesh with respects to types
              this->Superclass::GenerateData();
              
              // run the front
              this->Superclass2::SetInput( this-> GetOutput( ) );
              this->InitFront( );
              this->Superclass2::GenerateData( );
           };

        const IMeshType* GetInput( )
        {
           if( this->GetNumberOfInputs( ) < 1 )
              return( 0 );
           return( static_cast< const IMeshType* >
            ( this->Superclass::GetInput( 0 ) ) );
        }

        virtual void SetInput( const IMeshType* input )
        { this->Superclass::SetInput( const_cast< IMeshType* >
                                      ( input ) ); };

        private:
        MeshToMeshUsingFront( const Self& );   // Not impl.
        void operator=( const Self& ); // Not impl.
   };

} // enamespace

//#include "itkQEMeshToMeshUsingFront.txx"

#endif // __ITKQUADEDGEMESH__MESHTOMESHUSINGFRONT__H__

// eof - itkQEMeshToMeshUsingFront.h
