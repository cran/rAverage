#include <stdio.h>
#include <stdlib.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "rAverage.h"

void R_init_rAverage(DllInfo* info) {
	R_registerRoutines(info, NULL, NULL, NULL, NULL);
	R_useDynamicSymbols(info, TRUE);
}
