int binomial(int n, int k);
/*
 * Calcola il coefficiente binomiale n su k.
 * Attenzione: n deve essere maggiore o uguale a k,
 * altrimenti la funzione dara' errore (per questioni
 * di ottimizzazione non sono previsti controlli).
*/

int combin_length(int n);
/* Calcola la lunghezza del vettore out da passare a combin */

void combin(int *n, int *k, int *out);
/*
 * Presa una sequenza di elementi che va da 1 a n, genera tutte le le possibili
 * combinazioni degli n elementi a gruppi di k. out e' il vettore di output.
*/

void grid(int *lev, int *fact, int *out);
/*
 * Crea un vettore contenente le combinazioni dei livelli dei fattori indicati,
 * costruendo una griglia per un piano fattoriale.
 * lev: vettore contenente i livelli dei fattori
 * fact: numero di fattori (lunghezza del vettore lev)
 * out: vettore di output di dimensione fact*prod(lev),
 * che va inizializzato fuori dalla funzione.
*/

void respdim(int *lev, int *fact, int *dim);
/* Calcola la dimensione del vettore R */

void averaging(double *param, int *lev, int *fact, int *sumlev, double *R);
/* Ricompone gli R a partire dai parametri s e w */

void residual(double *param, double *fixed, int *lev, int *fact, int *sumlev, double *observed, int *dimdata, double *deltaweights, int *numfix, double *valfix, double *RSS);
/* Calcola la sommatoria dei quadrati residua */

void residual_eq(double *param, double *fixed, int *lev, int *fact, int *sumlev, double *observed, int *dimdata, double *RSS);
/* Calcola la sommatoria dei quadrati residua per l'EAM */

void residual_null(double *param, double *fixed, int *lev, int *fact, int *sumlev, int *spos_first, int *in_st, double *observed, int *dimdata, int *model, double *RSS);
/* Calcola la sommatoria dei quadrati residua per i modelli nulli */

void numpar(double *param, int *len, int *out);
/* Calcola il numero di parametri */

void parmean(double *parameters, double *deltaweights, int *sumlev, int *freedom, int *count);
/* Media i parametri uguali entro un ceto delta */

void parmeanlast(double *param, double *fixed, int *sumlev, double *deltaweights, int *numfix, double *valfix);
/* Come la parmean ma e' indipendente dalla residual */
