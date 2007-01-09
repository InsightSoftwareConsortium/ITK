// -------------------------------------------------------------------------
// itkQECommon.h
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

/**
 * When a class doesn't heritate from itkObject the itkDebugMacro
 * isn't available because this->GetDebug() isn't defined. Provide
 * a simpler one.
 */
#include <itkMacro.h>

#undef ITKQEMESH_DEBUG_MACROS

#ifdef ITKQEMESH_DEBUG_MACROS
#define itkQEDebugMacro( x )                                            \
    {                                                                   \
        ::itk::OStringStream itkmsg;                                    \
        itkmsg << "Debug: In " __FILE__ ", line " << __LINE__ << "\n"   \
               << " (" << this << "): " x                               \
               << "\n\n";                                               \
        ::itk::OutputWindowDisplayDebugText( itkmsg.str( ).c_str( ) );  \
    }
#else // ITKQEMESH_DEBUG_MACROS
#define itkQEDebugMacro( x )
#endif // ITKQEMESH_DEBUG_MACROS

#ifdef ITKQEMESH_DEBUG_MACROS
#define itkQEWarningMacro( x )                                          \
    {                                                                   \
        ::itk::OStringStream itkmsg;                                    \
        itkmsg << "WARNING: In " __FILE__ ", line " << __LINE__ << "\n" \
               << " (" << this << "): " x                               \
               << "\n\n";                                               \
        ::itk::OutputWindowDisplayWarningText( itkmsg.str( ).c_str( ) ); \
    }
#else // ITKQEMESH_DEBUG_MACROS
#define itkQEWarningMacro( x )
#endif // ITKQEMESH_DEBUG_MACROS

// eof - itkQECommon.h
