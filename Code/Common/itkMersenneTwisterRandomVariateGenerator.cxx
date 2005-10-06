/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMersenneTwisterRandomVariateGenerator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMersenneTwisterRandomVariateGenerator.h"

namespace itk {
  namespace Statistics {
    MersenneTwisterRandomVariateGenerator::Pointer MersenneTwisterRandomVariateGenerator::m_Instance = 0;

    /**
     * This just calls GetInstance
     */
    MersenneTwisterRandomVariateGenerator::Pointer 
    MersenneTwisterRandomVariateGenerator::New()
    { 
      return GetInstance();
    }

    /**
     * Return the single instance of the MersenneTwisterRandomVariateGenerator
     */
    MersenneTwisterRandomVariateGenerator::Pointer
    MersenneTwisterRandomVariateGenerator
    ::GetInstance()
    {
      if ( !MersenneTwisterRandomVariateGenerator::m_Instance )
        {
        // Try the factory first
        MersenneTwisterRandomVariateGenerator::m_Instance  = ObjectFactory<Self>::Create();
        // if the factory did not provide one, then create it here
        if( ! MersenneTwisterRandomVariateGenerator::m_Instance )
          {
          MersenneTwisterRandomVariateGenerator::m_Instance = new MersenneTwisterRandomVariateGenerator;
          // Remove extra reference from construction.
          MersenneTwisterRandomVariateGenerator::m_Instance->UnRegister();
          }
        }
      /**
       * return the instance
       */
      return MersenneTwisterRandomVariateGenerator::m_Instance;
    }
  }
}
