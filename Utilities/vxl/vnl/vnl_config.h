#ifndef vnl_config_h_
#define vnl_config_h_

// Edit this file to suit your taste, but don't commit the changes.

// set to 0 to disable *bounds* checks in the accessor methods (i.e.
// operator() and operator[]) of vnl_matrix<>, vnl_vector<> etc.
// this is not intended to also control *size* checks when doing
// matrix-vector arithmetic.
#define VNL_CONFIG_CHECK_BOUNDS   1

// set to 1 to enable certain old methods.
#define VNL_CONFIG_LEGACY_METHODS 0

// set to 0 if you don't need thread safe code.
#define VNL_CONFIG_THREAD_SAFE    1

#endif
