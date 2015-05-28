//
//  Macro.h
//  itkDiffusion
//
//  Created by Jean-Marie Mirebeau on 07/03/2014.
//
//

#ifndef itkDiffusion_Macro_h
#define itkDiffusion_Macro_h

/**
 Getters and setters for functor types, inspired by UnaryFunctorImageFilter.
 No equality test performed for the setter.
 */

#define GetSetFunctorMacro(name, type) \
    virtual type &          Get##name()         {return this->m_##name;} \
    virtual const type &    Get##name() const   {return this->m_##name;} \
    virtual void Set##name(const type & _arg) {m_##name = _arg; this->Modified();}
#endif
