/* Kevin P. Barry [ta0kira@users.berlios.de], BMCLAB, 23 Jun 2010 */

#include "R4dfp-object.h"

#include <sys/mman.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>


SEXP load_4dfp(SEXP fFile, SEXP rRead, SEXP wWrite)
{
	const char *file_name = CHAR(CHARACTER_POINTER(fFile)[0]);
	int direct_read  = INTEGER(rRead)[0];
	int direct_write = INTEGER(wWrite)[0];

	int map_mode = 0x00;
	if (direct_read)  map_mode |= PROT_READ;
	if (direct_write) map_mode |= PROT_WRITE;

	R4dfp_object *new_image = R4dfp_load(file_name, map_mode);
	if (!new_image) error("unable to load 4dfp image");

	return R4dfp_to_R(new_image);
}


SEXP blank_4dfp()
{
	R4dfp_object *new_image = calloc(1, sizeof(R4dfp_object));
	if (!new_image) error("unable to create 4dfp image");

	strcpy(new_image->header.number_format, "float");
	new_image->header.number_of_bytes_per_pixel = sizeof(float);
	new_image->header.orientation = 2;

	union {
	  unsigned char byte;
	  short         word;
	} test;

	test.word = 01;

	if (test.byte)
	strcpy(new_image->header.imagedata_byte_order, "littleendian");
	else
	strcpy(new_image->header.imagedata_byte_order, "bigendian");

	return R4dfp_to_R(new_image);
}


SEXP read_voxels_4dfp(SEXP oObject, SEXP xX, SEXP yY, SEXP zZ, SEXP tT)
{
	R4dfp_object *header = (R4dfp_object*) R_ExternalPtrAddr(VECTOR_ELT(oObject, R4DFP_INTERNAL));
	if (!header)
	{
	error("empty image header");
	return R_NilValue;
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

	int length_x = length(xX);
	int length_y = length(yY);
	int length_z = length(zZ);
	int length_t = length(tT);

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
	UNPROTECT(5);
	return R_NilValue;
	 }
	}

	else
	{
	for (X = 0; X < row_x; X++)
	for (Y = 0; Y < col_x; Y++)
	if (index_x[X + Y * row_x] >= header->header.matrix_size[Y] || index_x[X + Y * row_x] < 0)
	 {
	error("index is out of bounds");
	UNPROTECT(5);
	return R_NilValue;
	 }
	}


	if (!row_x) for (Y = 0; Y < length_y; Y++)
	if (index_y[Y] >= header->header.matrix_size[1] || index_y[Y] < 0)
	{
	error("y index is out of bounds");
	UNPROTECT(5);
	return R_NilValue;
	}


	if (!row_x) for (Z = 0; Z < length_z; Z++)
	if (index_z[Z] >= header->header.matrix_size[2] || index_z[Z] < 0)
	{
	error("z index is out of bounds");
	UNPROTECT(5);
	return R_NilValue;
	}


	if (use_t) for (t = 0; t < length_t; t++)
	if (index_t[t] >= header->header.matrix_size[3] || index_t[t] < 0)
	{
	error("t index is out of bounds");
	UNPROTECT(5);
	return R_NilValue;
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
	NUMERIC_POINTER(new_vector)[R4dfp_get_index(length_x, length_y, length_z, X, Y, Z, t)] =
	  ( (double) header->image.swap?
	    R4dfp_swap_endian(*R4dfp_get_voxel_const(header,
	      index_x[X], index_y[Y], index_z[Z], index_t[t])) :
	    *R4dfp_get_voxel_const(header,
	      index_x[X], index_y[Y], index_z[Z], index_t[t]) );
	}

	else if (use_t)
	{
	for (X = 0; X < row_x; X++)
	for (t = 0; t < length_t; t++)
	NUMERIC_POINTER(new_vector)[X + t * row_x] =
	  ( (double) header->image.swap?
	    R4dfp_swap_endian(*R4dfp_get_voxel_const(header,
	      index_x[X + 0 * row_x], index_x[X + 1 * row_x], index_x[X + 2 * row_x], index_t[t])) :
	    *R4dfp_get_voxel_const(header,
	      index_x[X + 0 * row_x], index_x[X + 1 * row_x], index_x[X + 2 * row_x], index_t[t]) );
	}

	else
	{
	for (X = 0; X < row_x; X++)
	NUMERIC_POINTER(new_vector)[X] =
	  ( (double) header->image.swap?
	    R4dfp_swap_endian(*R4dfp_get_voxel_const(header,
	      index_x[X + 0 * row_x], index_x[X + 1 * row_x], index_x[X + 2 * row_x], index_x[X + 3 * row_x])) :
	    *R4dfp_get_voxel_const(header,
	      index_x[X + 0 * row_x], index_x[X + 1 * row_x], index_x[X + 2 * row_x], index_x[X + 3 * row_x]) );
	}


	UNPROTECT(6);
	return new_vector;
}
