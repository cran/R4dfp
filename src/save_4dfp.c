/* Kevin P. Barry [ta0kira@users.berlios.de], BMCLAB, 23 Jun 2010 */

#include "R4dfp-object.h"

#include <sys/mman.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>


SEXP save_4dfp(SEXP oObject)
{
	R4dfp_update_file(oObject);

	R4dfp_object *header = (R4dfp_object*) R_ExternalPtrAddr(VECTOR_ELT(oObject, R4DFP_INTERNAL));
	if (!header)
	{
	error("empty image header");
	return oObject;
	}

	if (R4dfp_save(header) != 0)
	{
	error("unable to save 4dfp image");
	}

	return oObject;
}


SEXP write_voxels_4dfp(SEXP oObject, SEXP xX, SEXP yY, SEXP zZ, SEXP tT, SEXP vValue)
{
	R4dfp_object *header = (R4dfp_object*) R_ExternalPtrAddr(VECTOR_ELT(oObject, R4DFP_INTERNAL));
	if (!header)
	{
	error("empty image header");
	return oObject;
	}

	if ((header->image.mtype & PROT_READ) && !(header->image.mtype & PROT_WRITE))
	{
	error("image is mapped for direct reading but not writing");
	return oObject;
	}


	int row_x = 0, col_x = 0, use_t = 1;

	SEXP matrix_x;
	PROTECT(matrix_x = getAttrib(xX, R_DimSymbol));
	if (length(matrix_x) == 2)
	{
	row_x = INTEGER(matrix_x)[0];
	col_x = INTEGER(matrix_x)[1];
	switch (col_x)
	 {
	case 3:                     break;
	case 4:  use_t = 0;         break;
	default: row_x = col_x = 0; break;
	 }
	}


	PROTECT(xX = coerceVector(xX, INTSXP));
	if (!row_x) PROTECT(yY = coerceVector(yY, INTSXP));
	else        PROTECT(yY);
	if (!row_x) PROTECT(zZ = coerceVector(zZ, INTSXP));
	else        PROTECT(zZ);
	if (use_t)  PROTECT(tT = coerceVector(tT, INTSXP));
	else        PROTECT(tT);
	PROTECT(vValue = coerceVector(vValue, REALSXP));

	int length_x = length(xX);
	int length_y = length(yY);
	int length_z = length(zZ);
	int length_t = length(tT);
	int length_v = length(vValue);

	int *index_x = INTEGER(xX);
	int *index_y = row_x? NULL : INTEGER(yY);
	int *index_z = row_x? NULL : INTEGER(zZ);
	int *index_t = use_t? INTEGER(tT) : NULL;

	int X, Y, Z, t;


	if (!row_x)
	{
	for (X = 0; X < length_x; X++)
	if (index_x[X] >= header->header.matrix_size[0] || index_x[X] < 0)
	 {
	error("x index is out of bounds");
	UNPROTECT(6);
	return oObject;
	 }
	}

	else
	{
	for (X = 0; X < row_x; X++)
	for (Y = 0; Y < col_x; Y++)
	if (index_x[X + Y * row_x] >= header->header.matrix_size[Y] || index_x[X + Y * row_x] < 0)
	 {
	error("index is out of bounds");
	UNPROTECT(6);
	return oObject;
	 }
	}


	if (!row_x) for (Y = 0; Y < length_y; Y++)
	if (index_y[Y] >= header->header.matrix_size[1] || index_y[Y] < 0)
	{
	error("y index is out of bounds");
	UNPROTECT(6);
	return oObject;
	}


	if (!row_x) for (Z = 0; Z < length_z; Z++)
	if (index_z[Z] >= header->header.matrix_size[2] || index_z[Z] < 0)
	{
	error("z index is out of bounds");
	UNPROTECT(6);
	return oObject;
	}


	if (use_t) for (t = 0; t < length_t; t++)
	if (index_t[t] >= header->header.matrix_size[3] || index_t[t] < 0)
	{
	error("t index is out of bounds");
	UNPROTECT(6);
	return oObject;
	}


	SEXP new_vector;

	if (!row_x)
	PROTECT(new_vector = NEW_NUMERIC(length_x * length_y * length_z * length_t));
	else if (use_t)
	PROTECT(new_vector = NEW_NUMERIC(row_x * length_t));
	else
	PROTECT(new_vector = NEW_NUMERIC(row_x));


	if (!row_x)
	{
	for (X = 0; X < length_x; X++)
	for (Y = 0; Y < length_y; Y++)
	for (Z = 0; Z < length_z; Z++)
	for (t = 0; t < length_t; t++)
	if (length_v == 1)
	 {
	*R4dfp_get_voxel(header, index_x[X], index_y[Y], index_z[Z], index_t[t]) =
	  ( (float) header->image.swap?
	    R4dfp_swap_endian(NUMERIC_POINTER(vValue)[0]) :
	    NUMERIC_POINTER(vValue)[0] );
	 }
	else
	 {
	*R4dfp_get_voxel(header, index_x[X], index_y[Y], index_z[Z], index_t[t]) =
	  ( (float) header->image.swap?
	    R4dfp_swap_endian(NUMERIC_POINTER(vValue)[R4dfp_get_index(length_x, length_y, length_z, X, Y, Z, t)]) :
	    NUMERIC_POINTER(vValue)[R4dfp_get_index(length_x, length_y, length_z, X, Y, Z, t)] );
	 }
	}

	else if (use_t)
	{
	for (X = 0; X < row_x; X++)
	for (t = 0; t < length_t; t++)
	if (length_v == 1)
	 {
	*R4dfp_get_voxel(header,
	   index_x[X + 0 * row_x], index_x[X + 1 * row_x], index_x[X + 2 * row_x], index_t[t]) =
	  ( (float) header->image.swap?
	    R4dfp_swap_endian(NUMERIC_POINTER(vValue)[0]) :
	    NUMERIC_POINTER(vValue)[0] );
	 }
	else
	 {
	*R4dfp_get_voxel(header,
	   index_x[X + 0 * row_x], index_x[X + 1 * row_x], index_x[X + 2 * row_x], index_t[t]) =
	  ( (float) header->image.swap?
	    R4dfp_swap_endian(NUMERIC_POINTER(vValue)[X + t * row_x]) :
	    NUMERIC_POINTER(vValue)[X + t * row_x] );
	 }
	}

	else
	{
	for (X = 0; X < row_x; X++)
	if (length_v == 1)
	 {
	*R4dfp_get_voxel(header,
	   index_x[X + 0 * row_x], index_x[X + 1 * row_x], index_x[X + 2 * row_x], index_x[X + 3 * row_x]) =
	  ( (float) header->image.swap?
	    R4dfp_swap_endian(NUMERIC_POINTER(vValue)[0]) :
	    NUMERIC_POINTER(vValue)[0] );
	 }
	else
	 {
	*R4dfp_get_voxel(header,
	   index_x[X + 0 * row_x], index_x[X + 1 * row_x], index_x[X + 2 * row_x], index_x[X + 3 * row_x]) =
	  ( (float) header->image.swap?
	    R4dfp_swap_endian(NUMERIC_POINTER(vValue)[X]) :
	    NUMERIC_POINTER(vValue)[X] );
	 }
	}


	UNPROTECT(7);
	return oObject;
}
