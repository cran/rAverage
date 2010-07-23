#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "rAverage.h"

int binomial(int n, int k)
{
    int i=n-1,j=n;
    if(n==k) return(1);
    while(i>(n-k)) {
        j *= i;
        i--;
    }
    i = k-1;
    while(i>1) {
        k *= i;
        i--;
    }
    i = j/k;
    return(i);
}

int combin_length(int n)
{
    int len;
    if(n == 3)
        len = 6;
    else {
        if((n%2) == 0)
            len = (n/2)*binomial(n,n/2);
        else
            len = ((n-1)/2)*binomial(n,(n-1)/2);
    }
    return(len);
}

void combin(int *n, int *k, int *out)
{
    // Inizializzazione variabili
    int i,j,t;
    int h,pos,add;
    int sequence[*k], base[*k], origin[*k], stop[*k];
    for(j=0; j<*k; j++) {
        out[j] = j+1;
        sequence[j] = j+1;
        base[j] = j+1;
        origin[j] = j+1;
        stop[j] = *n-*k+1+j;
    }
    // Costruzione combinazioni
    t = -1;
    i = *k-1;
    while(sequence[0] < stop[0]) {
        for(j=0; j<*k; j++) {
            t = t+1;
            sequence[j] = base[j];
            out[t] = sequence[j];
        }
        while(sequence[*k-1] < stop[*k-1]) {
            sequence[*k-1] = sequence[*k-1]+1;
            for(j=0; j<*k; j++) {
                t = t+1;
                out[t] = sequence[j];
            }
        }
        if(sequence[i]==stop[i]) {
            i = i-1;
            for(j=0; j<*k; j++)
                base[j] = origin[j];
            for(j=i; j<*k; j++)
                base[j] = base[j]+1;
        }
        else {
            h = 0;
            for(j=*k-1; j>=i; j--) {
                h = h + (sequence[j]==stop[j]);
            }
            pos = *k-h-1;
            add = base[pos]+1;
            for(j=0; j<=h; j++)
                base[pos+j] = add+j;
        }
    }
}

void grid(int *lev, int *fact, int *out)
{
    int a=0,b=0,j=0,h=0,trace=1;
    int eachrep,numrep=1;
    for(a=0; a<*fact; a++) {
        // eachrep
        eachrep = 1;
        for(j=a+1; j<*fact; j++)
            eachrep = eachrep*lev[j];
        // numrep
        numrep = 1;
        for(j=0; j<a; j++)
            numrep *= lev[j];
        // index
        for(j=0; j<numrep; j++) {
            for(trace=1; trace<=lev[a]; trace++) {
                for(b=0; b<eachrep; b++) {
                    out[h] = trace;
                    h++;
                }
            }
        }
    }
}
