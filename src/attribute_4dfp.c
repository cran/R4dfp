#include "R4dfp-object.h"

#include <string.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>


SEXP attribute_change_4dfp(SEXP oObject, SEXP aAttr, SEXP vValue)
{
	R4dfp_object *header = (R4dfp_object*) R_ExternalPtrAddr(VECTOR_ELT(oObject, R4DFP_INTERNAL));
	if (!header)
	{
	error("empty image header");
	return oObject;
	}

	if (header->image.mtype)
	{
	error("can't edit attributes for a mapped image (try 'recycle.4dfp' first)");
	return oObject;
	}

	const char *attribute = CHAR(CHARACTER_POINTER(aAttr)[0]);
	int I;


	if      (strcmp(attribute, "file") == 0)
	{
	free(header->file);
	get_file_names(CHAR(CHARACTER_POINTER(vValue)[0]), &header->file, NULL);
	SET_VECTOR_ELT(oObject, R4DFP_FILE, mkString(header->file? header->file : ""));
	}


	else if (strcmp(attribute, "dims") == 0)
	{
	PROTECT(vValue = coerceVector(vValue, INTSXP));
	if (length(vValue) != 4)
	 {
	error("incorrect number of dimensions");
	UNPROTECT(1);
	return oObject;
	 }
	R4dfp_object temp_header = *header;
	memcpy(temp_header.header.matrix_size, INTEGER(vValue), 4 * sizeof(int));
	if (temp_header.header.matrix_size[0] < 1 ||
	    temp_header.header.matrix_size[1] < 1 ||
	    temp_header.header.matrix_size[2] < 1 ||
	    temp_header.header.matrix_size[3] < 1)
	 {
	error("invalid dimension length");
	UNPROTECT(1);
	return oObject;
	 }
	temp_header.image.size =
	  temp_header.header.matrix_size[0] *
	  temp_header.header.matrix_size[1] *
	  temp_header.header.matrix_size[2] *
	  temp_header.header.matrix_size[3] * sizeof(float);
	temp_header.image.array = malloc(temp_header.image.size);
	if (!temp_header.image.array)
	 {
	error("couldn't allocate image array");
	UNPROTECT(1);
	return oObject;
	 }
	free(header->image.array);
	*header = temp_header;
	UNPROTECT(1);
	INTEGER(VECTOR_ELT(oObject, R4DFP_DIM))[0] = header->header.matrix_size[0];
	INTEGER(VECTOR_ELT(oObject, R4DFP_DIM))[1] = header->header.matrix_size[1];
	INTEGER(VECTOR_ELT(oObject, R4DFP_DIM))[2] = header->header.matrix_size[2];
	INTEGER(VECTOR_ELT(oObject, R4DFP_DIM))[3] = header->header.matrix_size[3];
	}


	else if (strcmp(attribute, "scale") == 0)
	{
	PROTECT(vValue = coerceVector(vValue, REALSXP));
	if (length(vValue) != 3 && length(vValue) != 4)
	 {
	error("incorrect number of dimensions");
	UNPROTECT(1);
	return oObject;
	 }
	for (I = 0; I < length(vValue); I++)
	header->header.scaling_factor[I] = (float) REAL(vValue)[I];
	UNPROTECT(1);
	REAL(VECTOR_ELT(oObject, R4DFP_SCALE))[0] = header->header.scaling_factor[0];
	REAL(VECTOR_ELT(oObject, R4DFP_SCALE))[1] = header->header.scaling_factor[1];
	REAL(VECTOR_ELT(oObject, R4DFP_SCALE))[2] = header->header.scaling_factor[2];
	REAL(VECTOR_ELT(oObject, R4DFP_SCALE))[3] = header->header.scaling_factor[3];
	}


	else if (strcmp(attribute, "mmppix") == 0)
	{
	PROTECT(vValue = coerceVector(vValue, REALSXP));
	if (length(vValue) != 3)
	 {
	error("incorrect number of dimensions");
	UNPROTECT(1);
	return oObject;
	 }
	for (I = 0; I < length(vValue); I++)
	header->header.mmppix[I] = (float) REAL(vValue)[I];
	UNPROTECT(1);
	REAL(VECTOR_ELT(oObject, R4DFP_MMPPIX))[0] = header->header.mmppix[0];
	REAL(VECTOR_ELT(oObject, R4DFP_MMPPIX))[1] = header->header.mmppix[1];
	REAL(VECTOR_ELT(oObject, R4DFP_MMPPIX))[2] = header->header.mmppix[2];
	}


	else if (strcmp(attribute, "center") == 0)
	{
	PROTECT(vValue = coerceVector(vValue, REALSXP));
	if (length(vValue) != 3)
	 {
	error("incorrect number of dimensions");
	UNPROTECT(1);
	return oObject;
	 }
	for (I = 0; I < length(vValue); I++)
	header->header.center[I] = (float) REAL(vValue)[I];
	UNPROTECT(1);
	REAL(VECTOR_ELT(oObject, R4DFP_CENTER))[0] = header->header.center[0];
	REAL(VECTOR_ELT(oObject, R4DFP_CENTER))[1] = header->header.center[1];
	REAL(VECTOR_ELT(oObject, R4DFP_CENTER))[2] = header->header.center[2];
	}


	else
	{
	error("invalid 4dfp attribute");
	return oObject;
	}


	return oObject;
}
