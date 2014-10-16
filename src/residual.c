#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "rAverage.h"

void numpar(double *param, int *len, int *out)
{
    int i=-1,j=0,k=0;
    int jump[*len];
    int count=-1;
    double diff=0;
    while(i<(*len-1)) {
        i++;
        for(j=0; j<count; j++)
            if(i==jump[j]) i++;
        k = i+1;
        while(k<*len) {
            for(j=0; j<count; j++)
                if(k==jump[j]) k++;
            diff = param[i]-param[k];
            if(diff<0) diff *= (-1);
            if(diff==0) { //diff<1e-8
                count++;
                jump[count] = k;
                (*out) = (*out)-1;
            }
            k++;
        }
    }
}

void parmean(double *parameters, double *deltaweights, int *sumlev, int *freedom, int *count)
{
    int i=0,j, k, den, step, flag, nweight=(*count);
    int record[sumlev[0]]; //posizioni dei pesi diversi dai fissati
    double diff, mean;
    
    while((nweight>1)&&(i<(nweight-1))) {
        den=1;
        mean = parameters[freedom[i]];
        record[0] = freedom[i];
        for(j=i+1;j<nweight;j++) {
            for(k=0;k<den;k++) {
                diff = parameters[record[k]]-parameters[freedom[j]];
                if(diff<0) diff *= -1;
                if(diff<=(*deltaweights)) {
                    mean *= den;
                    den++;
                    mean += parameters[freedom[j]];
                    mean /= den;
                    record[den-1] = freedom[j];
                    break;
                }
            }
        }
        if(den>1){
            for(j=0;j<den;j++) parameters[record[j]] = mean;
            i=0;
            step=0;
            for(j=0;j<nweight;j++){
                flag=0;
                for(k=0;k<den;k++){
                    if(freedom[j]==record[k]){
                        flag=1;
                        break;
                    }
                }
                if(flag==0) {
                    freedom[step]=freedom[j];
                    step+=1;
                } 
                if(step==(nweight-den)) break;
            }
            nweight-=den;
        } else {i++;}
    }
}

void parmeanlast(double *param, double *fixed, int *sumlev, double *deltaweights, int *numfix, double *valfix)
{
    // Dichiarazione variabili
    int i,j, count=sumlev[0];   // Indici da usare nei cicli
    int freedom[count];         // Vettore posizioni pesi liberissimi
    double diff;                // Differenze fixed parametri    
    
    if(*numfix==0){
        for(i=0; i<sumlev[0]; i++) freedom[i] = i+sumlev[1];
        parmean(param, deltaweights, sumlev, freedom, &count);
    }
    else {
        count=0;
        for(i=sumlev[1]; i<sumlev[2]; i++) {
            if(!isnan(fixed[i])) param[i] = fixed[i];
            else {
                for(j=0; j<(*numfix); j++) {
                    diff = valfix[j]-param[i];
                    if(diff<0) diff *=-1;
                    if(diff<=(*deltaweights)) param[i]=valfix[j];
                    else {
                        freedom[count] = i;
                        count++;
                    }
                }
            }
        }
        if(count>1) parmean(param, deltaweights, sumlev, freedom, &count);
    }
}

void residual(double *param, double *fixed, int *lev, int *fact, int *sumlev, double *observed, int *dimdata, double *deltaweights, int *numfix, double *valfix, double *RSS)
{
    // Dichiarazione variabili
    int i,j;                                // Indici da usare nei cicli
    int lentot=(dimdata[0])*(dimdata[1]);   // Lunghezza di observed 
    double resid;                           // Conterra' il residuo per ogni R
    double parameters[sumlev[2]];           // Vettore param con i pesi replicati
    double estimated[dimdata[1]];           // Conterra' gli R ricostruiti
    
    //Inizializzazione del vettore parameters
    for(i=0; i<sumlev[2]; i++)  {
        if(!isnan(fixed[i])) parameters[i] = fixed[i]; // Parametri fissi
        else parameters[i] = param[i]; //parametri liberi
    }
    
    // Calcolo RSS
    averaging(parameters, lev, fact, sumlev, estimated);
    j = 0;
    for(i=0; i<lentot; i++) {
        if(!isnan(observed[i])) {
            resid = estimated[j]-observed[i];
            *RSS += resid*resid;
        }
        if(i==((dimdata[0])*(j+1)-1)) j++;
    }
}

void residual_eq(double *param, double *fixed, int *lev, int *fact, int *sumlev, double *observed, int *dimdata, double *RSS)
{
    int i,j,k;                              // Indici da usare nei cicli
    int lentot=(dimdata[0])*(dimdata[1]);   // Lunghezza di observed
    double resid;                           // Conterra' il residuo per ogni R
    double parameters[sumlev[2]];           // Vettore param con i pesi replicati
    double estimated[dimdata[1]];           // Conterra' gli R ricostruiti
    
    for(i=0; i<sumlev[1]; i++)
        if(!isnan(fixed[i])) parameters[i] = fixed[i];
        else parameters[i] = param[i];
    // Inserimento dei pesi (il modello EAM ne richiede la replicazione):
    j = sumlev[1];
    for(i=0; i<*fact; i++) {
        for(k=0; k<lev[i]; k++) {
            parameters[j] = param[sumlev[1]+i];
            j++;
        }
    }
    // Calcolo RSS
    averaging(parameters, lev, fact, sumlev, estimated);
    j = 0;
    for(i=0; i<lentot; i++) {
        if(!isnan(observed[i])) {
            resid = estimated[j]-observed[i];
            *RSS += resid*resid;
        }
        if(i==((dimdata[0])*(j+1)-1)) j++;
    }
}

void residual_null(double *param, double *fixed, int *lev, int *fact, int *sumlev, int *spos_first, int *in_st, double *observed, int *dimdata, int *model, double *RSS)
{
    int i,j;                                // Indici da usare nei cicli
    int lentot=(dimdata[0])*(dimdata[1]);   // Lunghezza di observed 
    double resid;                           // Conterra' il residuo per ogni R
    double estimated[dimdata[1]];           // Conterra' gli R ricostruiti
    double parameters[sumlev[2]];
    
    // Permettere di poter fissare gli s
    
    // Stato iniziale
    if(*in_st==0 || *model==0) {
        parameters[0] = fixed[0];
        parameters[1] = fixed[1];
    } else {
        parameters[0] = param[0];
        parameters[1] = 0;
    }
    // Valori di scala
    if(*model==1) { // Un s per tutto
        for(i=2; i<sumlev[1]; i++)
            parameters[i] = param[*in_st];
    }
    if(*model==2) { // Un s per fattore
        for(i=0; i<*fact; i++) {
            for(j=0; j<lev[i]; j++)
                parameters[spos_first[i]+j] = param[i+*in_st];
        }
    }
    if(*model==3) { // Un s per ogni livello di ogni fattore
        for(i=2; i<sumlev[1]; i++) {
            if(!isnan(fixed[i])) parameters[i] = fixed[i]; // fissaggio degli s
            else parameters[i] = param[i-2+*in_st];
        }
    }
    
    // Pesi
    for(i=sumlev[1]; i<sumlev[2]; i++)
        parameters[i] = 0;
    
    
    // Calcolo RSS
    averaging(parameters, lev, fact, sumlev, estimated);
    j = 0;
    for(i=0; i<lentot; i++) {
        if(!isnan(observed[i])) {
            resid = estimated[j]-observed[i];
            *RSS += resid*resid;
        }
        if(i==((dimdata[0])*(j+1)-1)) j++;
    }
}
