/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolygonGroupOrientation.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPolygonGroupOrientation_h
#define _itkPolygonGroupOrientation_h
/**
 * \Enumerated PolygonGroupOrientation
 * \brief enumerates the possible spatial orientations
 */
namespace itk {
typedef enum {
  Axial = 0,
  Coronal = 1,
  Sagittal = 2,
  UserPlane = 3,
  Unknown = 4
} PolygonGroupOrientation;
}
#endif
