/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsVectorPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericTraitsVectorPixel.h"

namespace itk
{

const Vector<double , 2>  NumericTraits<Vector<double, 2> >::Zero = Vector<double , 2>( NumericTraits<double >::Zero );
const Vector<double , 2>  NumericTraits<Vector<double , 2> >::One = Vector<double , 2>( NumericTraits<double >::One );
 const Vector<double , 3>  NumericTraits<Vector<double, 3> >::Zero = Vector<double , 3>( NumericTraits<double>::Zero );
const Vector<double , 3>  NumericTraits<Vector<double , 3> >::One = Vector<double , 3>( NumericTraits<double>::One );
 const Vector<double , 4>  NumericTraits<Vector<double, 4> >::Zero = Vector<double , 4>( NumericTraits<double>::Zero );
const Vector<double , 4>  NumericTraits<Vector<double , 4> >::One = Vector<double , 4>( NumericTraits<double>::One );
  const Vector<double , 5>  NumericTraits<Vector<double, 5> >::Zero = Vector<double , 5>( NumericTraits<double>::Zero );
const Vector<double , 5>  NumericTraits<Vector<double , 5> >::One = Vector<double , 5>( NumericTraits<double>::One );
 
const Vector<float , 2>  NumericTraits<Vector<float, 2> >::Zero = Vector<float , 2>( NumericTraits<float >::Zero );
const Vector<float , 2>  NumericTraits<Vector<float , 2> >::One = Vector<float , 2>( NumericTraits<float >::One );
const Vector<float , 3>  NumericTraits<Vector<float, 3> >::Zero = Vector<float , 3>( NumericTraits<float >::Zero );
const Vector<float , 3>  NumericTraits<Vector<float , 3> >::One = Vector<float , 3>( NumericTraits<float >::One );
 

} // end namespace itk
