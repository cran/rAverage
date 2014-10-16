#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "rAverage.h"

void respdim(int *lev, int *fact, int *dim)
{
    int i,j,k;
    int combsetlen = combin_length(*fact);
    int combset[combsetlen];
    int prod;
    int ncomb;
    *dim = lev[0];
    for(i=1; i<*fact; i++)
        *dim += lev[i];
    for(k=2; k<=*fact; k++) {
        ncomb = binomial(*fact,k);
        combin(fact,&k,combset);
        i = 0;
        while(i<(k*ncomb)) {
            prod = 1;
            for(j=0; j<k; j++) {
                prod *= lev[combset[i]-1];
                i++;
            }
            *dim += prod;
        }
    }
}

void averaging(double *param, int *lev, int *fact, int *sumlev, double *R)
{
/*
  ------------------------------------------------
    Dichiarazione variabili
  ------------------------------------------------
*/
    int k;             // Spanna tutti i disegni, da 2 a k fattori
    int i;             // Spanna le possibili combinazioni di fattori del disegno a k-vie
    int h,j;           // Indici usati nei cicli
    int ncomb;         // Numero di possibili combinazioni di k fattori
    int prodlev=lev[0];// Prodotto dei livelli dei fattori
    int prodlev_i;     // Prodotto dei livelli dei k fattori della i-esima combinazione
    int dim;           // Numero di R per una data combinazione i a k fattori
    int slot;          // Traccia il riempimento di tutto il vettore R
    int count=0;       // Contatore per il riempimento di s e w
    int combsetlen;    // Massima lunghezza del vettore combset
    int *combset;      // Contiene tutte le possibili combinazioni dei k fattori considerati
    int *combset_i;    // Isola la i-esima combinazione dei k fattori
    int *lev_i;        // Isola i livelli della i-esima combinazione per i k fattori
    int *index;        // Vettore contenente il piano fattoriale
    int *t_index;      // Piano fattoriale trasformato in indici
    int seq;           // Traccia il riempimento del vettore t_index
    int steps[*fact];  // Valori necessari per trasformare il piano fattoriale in indici
    /* Parametri del modello averaging */
    double s0 = param[0];
    double w0 = param[1];
    double s[sumlev[0]],w[sumlev[0]];
    double sum_sw=0;   // Numeratore (summa s*w) di ogni R
    double sum_w=0;    // Denominatore (somma w) di ogni R
/*
  ------------------------------------------------
    Inizializzazione variabili
  ------------------------------------------------
*/
    // Parametri del modello averaging:
    w0 = exp(w0);
    for(j=0; j<sumlev[0]; j++) {
        s[j] = param[2+j];
        w[j] = exp(param[sumlev[1]+j]);
    }
    // steps contiene i "salti" da effettuare all'interno dei vettori
    // s e w per passare dai parametri di un fattore all'altro:
    steps[0]=0;
    for(j=1; j<*fact; j++) {
        steps[j] = steps[j-1]+lev[j-1];
        prodlev *= lev[j];
    }
    
    combsetlen = combin_length(*fact);
    combset = calloc(combsetlen,sizeof(int));
    combset_i = calloc(combsetlen,sizeof(int));
    lev_i = calloc((*fact),sizeof(int));
    index = calloc(prodlev*(*fact),sizeof(int));
    t_index = calloc(prodlev*(*fact),sizeof(int));
/*
  ------------------------------------------------
    Calcolo degli R stimati
  ------------------------------------------------
*/
    //  ------------------------------------------
    //  Disegni a una via
    //  ------------------------------------------
    for(slot=0; slot<sumlev[0]; slot++)
        R[slot] = (s0*w0+s[slot]*w[slot])/(w0+w[slot]);
    //  ------------------------------------------
    //  Disegni a k-vie
    //  ------------------------------------------
    for(k=2; k<=*fact; k++) {
        // numero di possibili combinazioni dei k fattori:
        ncomb = binomial(*fact,k);
        // calcola le combinazioni e riempie il vettore combset tramite il passaggio del puntatore:
        combin(fact, &k, combset);
        //  si spannano tutte le combinazioni
        for(i=0; i<ncomb; i++) {
            // Estrazione della i-esima combinazione:
            prodlev_i = 1;
            for(h=0; h<k; h++) {
                combset_i[h] = combset[h+i*k];
                lev_i[h] = lev[combset_i[h]-1];
                prodlev_i *= lev_i[h];
            }
            // crea un vettore contenente le combinazioni dei livelli
            // dei fattori considerati:
            dim = prodlev_i*k;
            grid(lev_i, &k, index);
            // Le combinazioni vengono trasformate in indici:
            // prima le posizioni nei vettori s e w dei parametri per
            // il fattore A, poi le posizioni per il fattore B, ecc.:
            seq=0;
            for(j=0; j<prodlev_i; j++) {
                for(h=0; h<k; h++) {
                    t_index[seq] = index[j+h*prodlev_i]-1+steps[combset_i[h]-1];
                    seq++;
                }
            }
            //  ----------------------------------
            //  Ricostruzione degli R
            //  ----------------------------------
            for(h=0; h<dim; h++) {
                count++;
                sum_sw += s[t_index[h]]*w[t_index[h]];
                sum_w += w[t_index[h]];
                if(count==k) {
                    R[slot] = (s0*w0+sum_sw)/(w0+sum_w);
                    slot++;
                    count = 0;
                    sum_sw = 0;
                    sum_w = 0;
                }
            }
        }
    }
    free(combset);
    free(combset_i);
    free(lev_i);
    free(index);
    free(t_index);
}
