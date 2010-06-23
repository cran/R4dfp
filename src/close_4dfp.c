/* Kevin P. Barry [ta0kira@users.berlios.de], BMCLAB, 23 Jun 2010 */

#include "R4dfp-object.h"

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>


SEXP close_4dfp(SEXP oObject, SEXP sSave)
{
	int save_first = LOGICAL(sSave)[0];

	R4dfp_object *header = (R4dfp_object*) R_ExternalPtrAddr(VECTOR_ELT(oObject, R4DFP_INTERNAL));
	if (!header)
	{
	error("empty image header");
	return oObject;
	}

	SEXP save_4dfp(SEXP);
	if (save_first && !header->image.mtype) save_4dfp(oObject);

	R4dfp_free(header);
	R4dfp_clear(oObject);
	return oObject;
}
