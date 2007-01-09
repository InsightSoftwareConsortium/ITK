// -------------------------------------------------------------------------
// itkQEMeshMacro.h
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

namespace itkQE
{

//////////////////////////////////////////////////////////////////////////////
/** \def itkQEMeshForAllPointsMacro
 * \brief Iterate on all the itkQE::Points of a given itkQE::Mesh instance.
 *
 * @param MeshType      The type of the itkQE::MeshType
 * @param MeshInstance  The instance of the above MeshType we are considering
 * @param PointVariable The name of the variable the caller wants to use to
 *        designate the MeshType::PointType at current stage of iteration.
 *        The variable PointVariable is of type itkQE::Mesh::PointType.
 * @param PointIndex    The name of the variable the caller wants to use to
 *        designate the index within the MeshType::PointContainer container
 *        and corresponding to PointVariable at current stage of iteration.
 *        The variable PointIndex is of type itkQE::Mesh::PointIdentifier .
 * \warning Don't forget to close the opened block with the corresponding
 *          itkQE::itkQEMeshForAllPointsMacro macro.
 * \example itkQE::MeshExtractComponentFilter::GetOutput().
 */
#define itkQEMeshForAllPointsMacro( MeshType,                                \
                                    MeshInstance,                            \
                                    PointVariable,                           \
                                    PointIndex )                             \
{                                                                            \
   typedef typename MeshType::PointType       PointType;                     \
   typedef typename MeshType::PointIdentifier PointIdentifier;               \
   typedef typename MeshType::PointsContainer PointsContainer;               \
   typedef typename MeshType::PointsContainerIterator                        \
                                              PointsContainerIterator;       \
                                                                             \
   PointsContainer* points = (MeshInstance)->GetPoints( );                   \
   /* If no points container are present, do nothing */                      \
   if( ! points )                                                            \
   {                                                                         \
       itkWarningMacro( "No point container in itkQEMeshForAllPointsMacro" );\
   }                                                                         \
   else                                                                      \
   {                                                                         \
       PointsContainerIterator pointIterator = points->Begin();              \
       for( ; pointIterator != points->End( ); pointIterator++ )             \
       {                                                                     \
          PointType PointVariable = pointIterator.Value( );                  \
          PointIdentifier PointIndex = pointIterator.Index( );
      
/** \def itkQEMeshForAllPointsEndMacro
 * \brief Terminates a block of code started with the macro
 *        itkQE::itkQEMeshForAllPointsMacro
 * \warning Should only be used with the corresponding
 *          itkQE::itkQEMeshForAllPointsMacro
 */
#define itkQEMeshForAllPointsEndMacro                                        \
       } /* rof */                                                           \
   } /* if */                                                                \
}

//////////////////////////////////////////////////////////////////////////////
/** \def itkQEMeshForAllCellsMacro
 * \brief Iterate on all the MeshType::Cells of a given itkQE::Mesh instance.
 *
 * @param MeshType      The type of the itkQE::MeshType
 * @param MeshInstance  The instance of the above MeshType we are considering
 * @param cellIterator  The name of the variable the caller wants to use to
 *        designate the MeshType::CellContainerIterator at current stage of
 *        iteration.
 * \warning Don't forget to close the opened block with the corresponding
 *          itkQE::itkQEMeshForAllCellsEndMacro macro.
 * \example itkQE::itkQEMeshForAllPrimalEdgesMacro
 */
#define itkQEMeshForAllCellsMacro( MeshType,                                 \
                                   MeshInstance,                             \
                                   cellIterator )                            \
{                                                                            \
   typedef typename MeshType::CellsContainer CellsContainer;                 \
   typedef typename MeshType::CellsContainerIterator                         \
                              CellsContainerIterator;                        \
   /* If no cells are present, do nothing */                                 \
   if( ! MeshInstance->GetCells( ) )                                         \
   {                                                                         \
       itkWarningMacro( "No Cells container in itkQEMeshForAllCellsMacro" ); \
   }                                                                         \
   else                                                                      \
   {                                                                         \
     CellsContainerIterator cellIterator = MeshInstance->GetCells()->Begin();\
     for( ; cellIterator != MeshInstance->GetCells()->End(); cellIterator++ )\
     {                                                                       \
         /* Users code comes here: */

/** \def itkQEMeshForAllCellsEndMacro
 * \brief Terminates a block of code started with the macro
 *        itkQE::itkQEMeshForAllCellsMacro
 * \warning Should only be used with the corresponding
 *          itkQE::itkQEMeshForAllCellsMacro
 */
#define itkQEMeshForAllCellsEndMacro                                         \
     } /* rof */                                                             \
   } /* fi */                                                                \
}

//////////////////////////////////////////////////////////////////////////////
/** \def itkQEMeshForAllPrimalEdgesMacro
 * \brief Iterate on all the MeshType::QEPrimal* of a given itkQE::Mesh instance
 *
 * @param MeshType      The type of the itkQE::MeshType
 * @param MeshInstance  The instance of the above MeshType we are considering
 * @param EdgeVariable  The name of the variable the caller wants to use to
 *        designate the MeshType::QEPrimal* at current stage of iteration.
 * \warning Don't forget to close the opened block with the corresponding
 *          itkQE::itkQEMeshForAllPrimalEdgesMacro macro.
 */
#define itkQEMeshForAllPrimalEdgesMacro( MeshType,                           \
                                         MeshInstance,                       \
                                         EdgeVariable )                      \
{                                                                            \
   typedef typename MeshType::QEPrimal QEPrimal;                             \
                                                                             \
   itkQEMeshForAllCellsMacro( MeshType, MeshInstance, cellIterator )         \
   {                                                                         \
      if( QEPrimal* EdgeVariable =                                           \
                        dynamic_cast< QEPrimal* >( cellIterator.Value( ) ) ) \
      {                                                                      \
         /* Users code comes here: */

/** \def itkQEMeshForAllPrimalEdgesEndMacro
 * \brief Terminates a block of code started with the macro
 *        itkQE::itkQEMeshForAllPrimalEdgesMacro
 * \warning Should only be used with the corresponding
 *          itkQE::itkQEMeshForAllPrimalEdgesMacro
 */
#define itkQEMeshForAllPrimalEdgesEndMacro                                   \
      } /* fi */                                                             \
   }                                                                         \
   itkQEMeshForAllCellsEndMacro                                              \
}


} //ecapseman

// eof - itkQEMeshMacro.h
