//-*- c++ -*-------------------------------------------------------------------
#ifndef netlib_h_
#define netlib_h_

extern "C" {
  int dsvdc_(double *x, const int& ldx, const int& m, const int& n,
	     double *sv,
	     double *errors,
	     double *u, const int& ldu,
	     double *v, const int& ldv,
	     double *work,
	     const int& job, int *info);

  int rsg_ (const int& nm, const int& n,
	    const double *a_matrix, const double *b_matrix,
	    double *eigenvalues,
	    const int& want_eigenvectors, const double *eigenvectors,
	    const double *workspace_1_size_n, const double *workspace_2_size_n,
	    int* output_error_code);

  int rs_(const int& nm, const int& n, const double *a_matrix,
	  double *eigenvalues,
	  const int& want_eigenvectors, const double *eigenvectors,
	  const double *workspace_1_size_n, const double *workspace_2_size_n,
	  int* output_error_code);

  int dqrdc_(double *x, const int& ldx, const int& n, const int& p, double* qraux, int *jpvt, double *work, const int& job);

};

#endif
