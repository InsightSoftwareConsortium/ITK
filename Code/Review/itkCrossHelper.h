#ifndef __itkCrossHelper_h
#define __itkCrossHelper_h

namespace itk
  {
  /** \class Cross
   * \brief Compute the cross product of two vectors of dimension 3,
   *        independently of the type of the values of vector's elements.
   */
  template< typename TVector >
  class CrossHelper
    {
    public:
      typedef TVector VectorType;
      typedef typename VectorType::ValueType ValueType;

      itkStaticConstMacro ( Dimension, unsigned int, VectorType::Dimension );

      /**
           * \param[in] iU
           * \param[in] iV
           * \return \f$ \boldsymbol{iU} \cdot \boldsymbol{iV} \f$
       */
      VectorType operator ( ) ( const VectorType& iU,
                                const VectorType& iV ) const
        {
          assert ( Dimension > 2 );

          VectorType oCross;

          // *** Arnaud : InputVDimension == 3
          oCross[0] = iU[1] * iV[2] - iV[1] * iU[2];
          oCross[1] = iV[0] * iU[2] - iU[0] * iV[2];
          oCross[2] = iU[0] * iV[1] - iV[0] * iU[1];

          for ( unsigned int dim = 3; dim < Dimension; dim++ )
            oCross[dim] = 0.;

          return oCross;
        }
    };
}

#endif
