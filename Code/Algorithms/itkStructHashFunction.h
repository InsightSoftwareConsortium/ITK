#ifndef ITKSTRUCTHASHFUNCTION_H
#define ITKSTRUCTHASHFUNCTION_H

namespace itk
{

/** /class StructHashFunction
 *
 *  /brief Generic hash function for an arbitrary struct (or class).
 *
 *  This regards the input key as a string of bytes, and applies a
 *  hash function similar to one that has been used in perl.  If the
 *  data in the input key has pointers to other classes, then the
 *  function will be hashing on the value of the pointer itself, not
 *  on the data it points to.  It is imagined that this function will
 *  be used more for structs (with fully exposed data) than for
 *  general classes.
 */
template< class TInput >
class StructHashFunction
{
public:

  /** Standard class typedefs. */
  typedef StructHashFunction Self;

  /** Input type */
  typedef TInput InputType;

  unsigned int operator()( const InputType& key ) const;

};

template< class TInput >
inline unsigned int
StructHashFunction< TInput >
::operator()( const InputType& key ) const
{
  int len = sizeof( InputType );
  const char* p = reinterpret_cast< const char* >( &key );
  int hash = 0;
  while( len-- )
    {
    hash = hash * 65 + *p++;
    }
  hash += (hash >> 5);
  return hash;
}

}

#endif  // ndef ITKSTRUCTHASHFUNCTION_H
