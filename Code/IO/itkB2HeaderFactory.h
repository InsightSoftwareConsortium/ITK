/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkB2HeaderFactory.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkB2HeaderFactory_h
#define __itkB2HeaderFactory_h

#include "itkB2HeaderBase.h"

namespace itk
{
    /** 
     * \class B2HeaderFactory
     * \brief Create instances of B2Header objects using an object factory.
     */
    //class ITK_EXPORT B2HeaderFactory : itk::Object
    class B2HeaderFactory
    {
      public:
        /** Standard class typedefs. */
        typedef B2HeaderFactory   Self;
        typedef Self *  Pointer;
        typedef const Self *  ConstPointer;

        B2HeaderFactory();
        virtual ~B2HeaderFactory();
        /** Class Methods used to interface with the registered factories */

        /** Convenient typedefs. */
        typedef itk::B2HeaderBase::Pointer B2HeaderBasePointer;

        /** Create the appropriate B2Header depending on the particulars of the file. */
        static B2HeaderBasePointer CreateB2HeaderReader(const std::string & TypeID);
        static B2HeaderBasePointer CreateB2HeaderWriter(const std::string & TypeID);

      protected:
        //virtual void PrintSelf(std::ostream& os, Indent indent) const;

      private:
        B2HeaderFactory(const Self&); //purposely not implemented
        void operator=(const Self&); //purposely not implemented
    };
} // end namespace itk
#endif
