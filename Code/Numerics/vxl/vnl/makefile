# This needs to come before the use of $(optimize).
# Why not just leave it up here always so we don't
# have to keep moving it around?
#######################################
include ${IUEROOT}/config/top-params.mk
#######################################

USES := vxl
LIBRARY := vnl
VERSION := 1.0
DEFINES += -DBUILDING_VNL_DLL

ifeq (1,$(strip $(GNU_LIBSTDCXX_V3)))
# optimization breaks linux <stdlib.h>
optimize := 
endif

# template sources
MANPAGE_SOURCES += vnl_c_vector.txx
MANPAGE_SOURCES += vnl_vector.txx
MANPAGE_SOURCES += vnl_vector_ref.txx
MANPAGE_SOURCES += vnl_vector_fixed.txx
MANPAGE_SOURCES += vnl_file_vector.txx
MANPAGE_SOURCES += vnl_matrix.txx
MANPAGE_SOURCES += vnl_matrix_fixed.txx
MANPAGE_SOURCES += vnl_matrix_fixed_ref.txx
MANPAGE_SOURCES += vnl_diag_matrix.txx
MANPAGE_SOURCES += vnl_sparse_matrix.txx
MANPAGE_SOURCES += vnl_matrix_exp.txx
MANPAGE_SOURCES += vnl_file_matrix.txx

MANPAGE_SOURCES += vnl_fortran_copy.txx
MANPAGE_SOURCES += vnl_resize.txx
MANPAGE_SOURCES += vnl_determinant.txx
MANPAGE_SOURCES += vnl_quaternion.txx
MANPAGE_SOURCES += vnl_scalar_join_iterator.txx

#
SOURCES += vnl_alloc.cxx
SOURCES += vnl_math.cxx
SOURCES += vnl_copy.cxx
SOURCES += vnl_complex.cxx
SOURCES += vnl_error.cxx
SOURCES += vnl_test.cxx
SOURCES += vnl_matlab_print.cxx
SOURCES += vnl_matlab_print_format.cxx
SOURCES += vnl_matlab_print_scalar.cxx
SOURCES += vnl_matlab_print2.cxx
SOURCES += vnl_matlab_header.cxx
SOURCES += vnl_matlab_write.cxx
SOURCES += vnl_matlab_read.cxx
SOURCES += vnl_matlab_filewrite.cxx
SOURCES += vnl_matops.cxx
SOURCES += vnl_real_polynomial.cxx
SOURCES += vnl_real_npolynomial.cxx
SOURCES += vnl_binary.cxx

# ops
SOURCES += vnl_fastops.cxx
SOURCES += vnl_linear_operators_3.cxx
MANPAGE_SOURCES += vnl_complex_ops.txx

# traits
SOURCES += vnl_numeric_limits.cxx
SOURCES += vnl_numeric_traits.cxx
SOURCES += vnl_complex_traits.cxx

#
SOURCES += vnl_int_matrix.cxx     # ?
SOURCES += vnl_int_2.cxx
SOURCES += vnl_int_3.cxx
SOURCES += vnl_int_4.cxx
SOURCES += vnl_float_2.cxx
SOURCES += vnl_float_3.cxx
SOURCES += vnl_float_4.cxx

SOURCES += vnl_double_2.cxx
SOURCES += vnl_double_3.cxx
SOURCES += vnl_double_4.cxx
SOURCES += vnl_double_2x3.cxx
SOURCES += vnl_double_3x2.cxx

# optimization
SOURCES += vnl_cost_function.cxx
SOURCES += vnl_least_squares_function.cxx
SOURCES += vnl_least_squares_cost_function.cxx
SOURCES += vnl_nonlinear_minimizer.cxx

# linear systems
SOURCES += vnl_linear_system.cxx
SOURCES += vnl_sparse_matrix_linear_system.cxx

# special matrices
SOURCES += vnl_rotation_matrix.cxx
SOURCES += vnl_cross_product_matrix.cxx
SOURCES += vnl_identity_3x3.cxx

# stuff
SOURCES += vnl_trace.cxx
SOURCES += vnl_sample.cxx
SOURCES += vnl_unary_function.cxx
SOURCES += vnl_transpose.cxx

FLAGS_vnl_fastops.cxx = $(optimize)
FLAGS_Templates/vnl_c_vector+float-.cxx += $(optimize)
FLAGS_Templates/vnl_c_vector+double-.cxx += $(optimize)
FLAGS_Templates/vnl_c_vector+float_complex-.cxx += $(optimize)
FLAGS_Templates/vnl_c_vector+double_complex-.cxx += $(optimize)
FLAGS_Templates/vnl_sparse_matrix_instances.cxx += $(optimize)

# algo
SUBDIRS += algo

# tests
ifndef NOTEST
SUBDIRS += tests
endif

# examples
ifndef NOTEST
SUBDIRS += examples
endif

######################################
include ${IUEROOT}/config/top-rules.mk
######################################

debug=$(debug_for_optimize) # needed for using $(optimize) on some files

# on HP unix, this library has become too big, so it needs -fPIC
ifeq "HP-UX" "$(findstring HP-UX,$(OS))"
pic = $(PIC)
endif
